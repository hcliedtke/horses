#' Horse Filter
#'
#' This function filters the velocity and heart rate
#' for multiple rides in the same dataframe
#' The dataframe "data" has to be structured as follows:
#' col "ID" with horse identifier, col "Zeit" with timestamp (tz = "Europe/Berlin"),
#' col "Lat" with Latitude (decimal), col "Lon" with Longitude (decimal),
#' col "HF" with heart rate in bpm, col "V" with velocity in m/min

#' @export
horse_filter <- function(data) {
  data <- data %>% dplyr::mutate(Datum = as.Date(Zeit, tz = "Europe/Berlin"))
  data <- data %>% dplyr::mutate(HF = ifelse(HF == 0, NA, HF)) # remove zeros in HF
  data <- data %>% dplyr::group_by(ID, Datum) %>% tidyr::nest() # nest data frame by ID and date
  data <- data %>% # Close small gaps (max 10) in HF by LOCF
    dplyr::mutate(HF_LOCF = purrr::map(data, ~zoo::na.locf(.x$HF, na.rm = FALSE,
                                        fromLast = FALSE, maxgap = 10)))
  data <- data %>% # Close small gaps (max 10) in V by LOCF
    dplyr::mutate(V_LOCF = purrr::map(data, ~zoo::na.locf(.x$V, na.rm = FALSE,
                                       fromLast = FALSE, maxgap = 10)))
  data <- tidyr::unnest(data, cols = c(data, HF_LOCF, V_LOCF))
  data <- data %>% dplyr::group_by(ID, Datum) %>% tidyr::nest() # nest data frame by ID and date
  data <- data %>% #Calculate rolling average of HF (window size = 9)
    dplyr::mutate(HF_mean9 = purrr::map(data, ~as.numeric(slider::slide(.x$HF_LOCF, mean,
                                                  .before = 4, .after = 4))))
  data <- tidyr::unnest(data, cols = c(data, HF_mean9))
  data <- data %>% # Replace HF with moving average when diff > 5
    dplyr::mutate(HF_cor5 = ifelse(abs(HF_LOCF - HF_mean9) > 5, HF_mean9, HF_LOCF))
  data <- dplyr::ungroup(data)
  data <- data %>% dplyr::select(-HF_LOCF, -HF_mean9)
  # ----------------------------------------------------------------------------
  # Remove small (< 29) sequences of non-NA values in HF
  HR_seqs <- as.data.frame(cbind(data$ID, as.character(data$Datum),
                                 !is.na(data$HF_cor5), !is.na(data$HF_cor5)))
  colnames(HR_seqs) <- c("ID", "Datum", "run", "values")
  runs <- HR_seqs %>% dplyr::group_by(run = data.table::rleid(ID, Datum, run), ID, Datum) %>%
    summarise(values = dplyr::first(values), lengths = n())
  if (any(runs$lengths[runs$values == TRUE] < 29)) {
    end <- cumsum(runs$lengths)
    runs$end <- end
    runs <- runs %>% dplyr::mutate(start = end-lengths+1)
    short <- runs %>% dplyr::filter(lengths < 29 & values == TRUE)
    start <- short$start
    end <- short$end
    FUN <- function(x,y) {
      data$HF_cor5[x:y] <- NA
      as.vector(data$HF_cor5)
    }
    HF_cor5 <- as.data.frame(mapply(FUN, start, end, SIMPLIFY = T)) %>%
      dplyr::mutate(ID = data$ID, Zeit = data$Zeit)
    HF_cor5 <- HF_cor5 %>% dplyr::filter(rowSums(dplyr::across(dplyr::everything(), ~is.na(.x)))>0) %>%
      dplyr::select(ID, Zeit) %>% mutate(HFNA = "short")
    data <- dplyr::left_join(data, HF_cor5)
    data$HF_cor5[data$HFNA == "short"] <- NA
    data$HFNA <- NULL
    HR_seqs <- data$HF_cor5
    HR_seqs <- !is.na(HR_seqs)
    runs <- rle(HR_seqs)
    runs <- as.data.frame(unclass(runs))
  }
  FUN <- function(x) {
    replicate(runs$lengths[x], x)
  }
  b <- length(runs$lengths)
  x <- 1:b
  list <- sapply(x, FUN)
  vec <- unlist(list)
  data <- data %>% dplyr::mutate(HRGroup = vec)
  data_withoutNA <- data %>% dplyr::filter(!is.na(HF_cor5))
  # ----------------------------------------------------------------------------
  # Filter HF (windowsize = 29, p = 3)
  if(sum(data_withoutNA$HF_cor5) == 0) {
    data$HF_gefiltert <- NA
  } else {
    data_withoutNA <- data_withoutNA %>% dplyr::group_by(ID, Datum, HRGroup) %>% tidyr::nest()
    data_withoutNA <- data_withoutNA %>%
      dplyr::mutate(HF_gefiltert = purrr::map(data, ~signal::sgolayfilt(.x$HF_cor5, p = 3, n = 29)))
    data_withoutNA <- tidyr::unnest(data_withoutNA, cols = c(data, HF_gefiltert))
    data <- dplyr::left_join(data, data_withoutNA)
  }
  data <- data %>% dplyr::select(-HRGroup, -HF_cor5)
  # ----------------------------------------------------------------------------
  # Remove small (< 9) sequences of non-NA values in V
  V_seqs <- as.data.frame(cbind(data$ID, as.character(data$Datum),
                                !is.na(data$V_LOCF), !is.na(data$V_LOCF)))
  colnames(V_seqs) <- c("ID", "Datum", "run", "values")
  runs <- V_seqs %>% dplyr::group_by(run = data.table::rleid(ID, Datum, run), ID, Datum) %>%
    summarise(values = dplyr::first(values), lengths = n())
  if (any(runs$lengths[runs$values == TRUE] < 9)) {
    end <- cumsum(runs$lengths)
    runs$end <- end
    runs <- runs %>% dplyr::mutate(start = end-lengths+1)
    short <- runs %>% dplyr::filter(lengths < 9 & values == TRUE)
    start <- short$start
    end <- short$end
    FUN <- function(x,y) {
      data$V_LOCF[x:y] <- NA
      as.vector(data$V_LOCF)
    }
    V_LOCF <- as.data.frame(mapply(FUN, start, end, SIMPLIFY = T)) %>%
      dplyr::mutate(ID = data$ID, Zeit = data$Zeit)
    V_LOCF <- V_LOCF %>% dplyr::filter(rowSums(dplyr::across(dplyr::everything(), ~is.na(.x)))>0) %>%
      dplyr::select(ID, Zeit) %>% dplyr::mutate(VNA = "short")
    data <- dplyr::left_join(data, V_LOCF)
    data$V_LOCF[data$VNA == "short"] <- NA
    data$VNA <- NULL
    V_seqs <- data$V_LOCF
    V_seqs <- !is.na(V_seqs)
    runs <- rle(V_seqs)
    runs <- as.data.frame(unclass(runs))
  }
  FUN <- function(x) {
    replicate(runs$lengths[x], x)
  }
  b <- length(runs$lengths)
  x <- 1:b
  list <- sapply(x, FUN)
  vec <- unlist(list)
  data <- data %>% dplyr::mutate(VGroup = vec)
  data_withoutNA <- data %>% dplyr::filter(!is.na(V_LOCF))
  data_withoutNA <- data_withoutNA %>% dplyr::group_by(ID, Datum, VGroup) %>% tidyr::nest()
  # ----------------------------------------------------------------------------
  # Filter V (windowsize = 9, p = 3)
  data_withoutNA <- data_withoutNA %>%
    dplyr::mutate(V_gefiltert = purrr::map(data, ~signal::sgolayfilt(.x$V_LOCF, p = 3, n = 9)))
  data_withoutNA <- tidyr::unnest(data_withoutNA, cols = c(data, V_gefiltert))
  data <- dplyr::left_join(data, data_withoutNA)
  data <- data %>% dplyr::select(-VGroup, -V_LOCF)
  data <- data %>%
    dplyr::mutate(V_gefiltert = ifelse(V_gefiltert < 0, 0, V_gefiltert))
  data
}
