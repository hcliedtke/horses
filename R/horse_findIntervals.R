#' Horse Find Intervals
#'
#' This function assigns intervals to sections of the data
#' for multiple rides in the same dataframe
#' The dataframe "data" has to be structured as follows:
#' col "ID" with horse identifier, col "Zeit" with timestamp (tz = "Europe/Berlin"),
#' col "V" with velocity in m/min, col "Gait" with gait (1 = walk, 2 = trot, 3 = gallop)

#' @export
horse_findIntervals <- function(data) {
  data <- data %>% dplyr::mutate(Datum = as.Date(Zeit, tz = "Europe/Berlin"))
  data <- data %>% dplyr::group_by(ID, Datum) %>% tidyr::nest() # nest data frame by ID and date

  data <- data %>% # Close small gaps (max 10) in V by LOCF
    dplyr::mutate(V_LOCF = purrr::map(data, ~zoo::na.locf(.x$V, na.rm = FALSE,
                                                          fromLast = FALSE, maxgap = 10)))
  data <- tidyr::unnest(data, cols = c(data, V_LOCF))
  data <- dplyr::ungroup(data)
  # ----------------------------------------------------------------------------
  # Remove small (< 21) sequences of NA values in V
  V_seqs <- as.data.frame(cbind(data$ID, as.character(data$Datum),
                                !is.na(data$V_LOCF), !is.na(data$V_LOCF)))
  colnames(V_seqs) <- c("ID", "Datum", "run", "values")
  runs <- V_seqs %>% dplyr::group_by(run = data.table::rleid(ID, Datum, run), ID, Datum) %>%
    summarise(values = dplyr::first(values), lengths = n())
  if (any(runs$lengths[runs$values == TRUE] < 21)) {
    end <- cumsum(runs$lengths)
    runs$end <- end
    runs <- runs %>% dplyr::mutate(start = end-lengths+1)
    short <- runs %>% dplyr::filter(lengths < 21 & values == TRUE)
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
  vec <- Reduce(c, list) #Reduce
  data <- data %>% dplyr::mutate(VGroup = vec)
  data_withoutNA <- data %>% dplyr::filter(!is.na(V_LOCF))
  data_withoutNA <- data_withoutNA %>% dplyr::group_by(ID, Datum, VGroup) %>% tidyr::nest()
  # ----------------------------------------------------------------------------
  # Filter V (windowsize = 21, p = 3)
  data_withoutNA <- data_withoutNA %>%
    dplyr::mutate(V_gefiltert = purrr::map(data, ~signal::sgolayfilt(.x$V_LOCF, p = 3, n = 21)))
  data_withoutNA <- tidyr::unnest(data_withoutNA, cols = c(data, V_gefiltert))
  data <- dplyr::left_join(data, data_withoutNA)
  data <- data %>% dplyr::select(-VGroup, -V_LOCF)
  data <- data %>%
    dplyr::mutate(V_gefiltert = ifelse(V_gefiltert < 0, 0, V_gefiltert))
  # ----------------------------------------------------------------------------
  # assign gait by filtered velocity when not measured
  data <- data %>%
    dplyr::mutate(Gangart = case_when(is.na(Gait) & V_gefiltert <= 120 ~ "Schritt",
                                      is.na(Gait) & V_gefiltert > 120 & V_gefiltert <= 250 ~ "Trab",
                                      is.na(Gait) & V_gefiltert > 250 ~ "Galopp",
                                      Gait <= 1 ~ "Schritt",
                                      Gait == 2 ~ "Trab",
                                      Gait == 3 ~ "Galopp"))
  data <- data %>%
    dplyr::mutate(GangInt = case_when(Gangart == "Schritt" ~ "ST",
                                      Gangart == "Trab" ~ "ST",
                                      Gangart == "Galopp" ~ "G"))
  # ----------------------------------------------------------------------------
  # create a variable that distinguishes between rides
  data <- data %>% dplyr::mutate(Ritt = paste0(ID, "_", Datum))
  # create a function that assigns intervals separately for each ride
  FindInt <- function(filter) {
    filter = dplyr::enquo(filter)
    data_filtered <- data %>% dplyr::filter(Ritt == !!filter)
    runs <- rle(data_filtered$GangInt)
    runs <- as.data.frame(unclass(runs))
    if (any(runs$lengths < 20)) {
      runs <- runs %>% dplyr::mutate(end = cumsum(lengths))
      runs <- runs %>% dplyr::mutate(start = end-lengths+1)
      short <- runs %>% dplyr::filter(lengths < 20)
      start <- short$start
      end <- short$end
      FUN <- function(x,y) {
        data_filtered$GangInt[x:y] <- NA
        as.vector(data_filtered$GangInt)
      }
      GangInt <- as.data.frame(mapply(FUN, start, end, SIMPLIFY = T)) %>%
        dplyr::mutate(ID = data_filtered$ID, Zeit = data_filtered$Zeit)
      GangInt <- GangInt %>%
        dplyr::filter(rowSums(dplyr::across(tidyselect::everything(), ~is.na(.x)))>0) %>%
        dplyr::select(ID, Zeit) %>% dplyr::mutate(GangartNA = "short")
      data_filtered <- dplyr::left_join(data_filtered, GangInt)
      data_filtered$GangInt[data_filtered$GangartNA == "short"] <- NA
      data_filtered <- data_filtered %>%
        dplyr::mutate(GangInt = zoo::na.locf(GangInt, na.rm = FALSE,
                                             fromLast = TRUE, maxgap = Inf))
      data_filtered <- data_filtered %>%
        dplyr::mutate(GangInt = zoo::na.locf(GangInt, na.rm = FALSE,
                                             fromLast = FALSE, maxgap = Inf))
      data_filtered$GangartNA <- NULL
    }
    data_filtered <- data_filtered %>%
      dplyr::mutate(Galopp = ifelse(GangInt == "G", "G", "P"))
    runs <- rle(data_filtered$Galopp)
    runs <- as.data.frame(unclass(runs))
    runs <- runs %>% dplyr::mutate(end = cumsum(lengths))
    runs <- runs %>% dplyr::mutate(start = end-lengths+1)
    runs$values[1] <- "WU"
    runs$values[nrow(runs)] <- "CD"
    Galopp <- runs %>% dplyr::filter(values == "G")

    if(nrow(Galopp) == 0) {
      rep(NA, nrow(data_filtered))
    } else {
      Galopp <- Galopp %>% dplyr::mutate(Nr = seq(1:nrow(Galopp))) %>%
        dplyr::mutate(Intervall = paste0(values, Nr)) %>% dplyr::select(-Nr)
      Pause <- runs %>% dplyr::filter(values == "P")
      if (nrow(Pause) > 0) {
        Pause <- Pause %>% dplyr::mutate(Nr = seq(1:nrow(Pause))) %>%
          dplyr::mutate(Intervall = paste0(values, Nr)) %>% dplyr::select(-Nr)
      }
      WU <- runs %>% dplyr::filter(values == "WU") %>% dplyr::mutate(Intervall = "WU")
      CD <- runs %>% dplyr::filter(values == "CD") %>% dplyr::mutate(Intervall = "CD")
      Intervalle <- rbind(WU, Galopp, Pause, CD) %>% dplyr::arrange(start)
      FUN <- function(x, y) {
        replicate(Intervalle$lengths[x], y)
      }
      b <- length(Intervalle$lengths)
      x <- 1:b
      y <- Intervalle$Intervall
      list <- mapply(FUN, x, y)
      unlist(list)
    }
  }
  # ----------------------------------------------------------------------------
  # apply the function to the different rides
  Ritte_list <- unique(data$Ritt)
  Intervalle <- sapply(Ritte_list, FindInt, simplify = FALSE)
  Intervalle <- plyr::ldply(Intervalle, cbind)
  colnames(Intervalle) <- c("Ritt", "Intervall")
  cbind(data, Intervalle) %>% dplyr::select(-Ritt, -GangInt)
}
