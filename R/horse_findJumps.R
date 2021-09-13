#' Horse Find Jumps
#'
#' This function overlays data from Cross-Country App and finds
#' location of jumps on gps tracks for multiple rides in one dataframe
#' Following objects must be present:
#' "kml": kml-file with track and efforts from Cross-Country App
#'   list with length = number of efforts + number of minute markers + track,
#'   efforts and minute markers: double(1 x Lon, Lat, Alt),
#'   track: double(length track x Lon, Lat, Alt)
#' "efforts": csv-file with efforts from Cross-Country App
#'   dataframe (Type, Number, Name, Strides, Seconds.From.Start(mm:ss),
#'   Metres.From.Start, Seconds.To.Finish(mm:ss), Metres.To.Finish,
#'   Seconds.From.Previous(mm:ss), Metres.From.Previous, Comment)
#' The dataframe "data" has to be structured as follows:
#' col "ID" with horse identifier, col "Zeit" with timestamp,
#' col "Lat" with Latitude (decimal), col "Lon" with Longitude (decimal),
#' col "V" with velocity in m/min
#' col "Bestzeit" with the optimum time ("hms" "difftime", hh:mm:ss)
#' col "Hindernis" with the effort at which horse was eliminated
#' col "Zeit_Gelände" with ridden cross-country time ("hms" "difftime", hh:mm:ss)

#' @export
horse_findJumps <- function(data) {
  # ----------------------------------------------------------------------------
  # Convert sec from start to time in min
  # ----------------------------------------------------------------------------
  efforts <- efforts %>%
    dplyr::mutate(Time = as.numeric(sapply(strsplit(efforts$Seconds.From.Start, ":"), "[", 1))+
             as.numeric(sapply(strsplit(efforts$Seconds.From.Start, ":"), "[", 2))/60)
  # ----------------------------------------------------------------------------
  # Extract track from kml list
  # ----------------------------------------------------------------------------
  index <- length(kml)
  track <- as.data.frame(kml[[index]])
  colnames(track) <- c("lon", "lat", "alt")
  # ----------------------------------------------------------------------------
  # Extract effort coordinates from kml list
  # ----------------------------------------------------------------------------
  EffortCount <- nrow(efforts)
  EffortsCoordinates <- as.data.frame(kml[1:EffortCount])
  colnames(EffortsCoordinates) <- paste(c("lon", "lat", "alt"),
                                        rep(1:EffortCount, each = 3))
  lon <- EffortsCoordinates %>% dplyr::select(starts_with("lon"))
  lon <- lon %>% tidyr::gather(Number, value) %>% dplyr::select(value)
  colnames(lon) <- "lon"
  lat <- EffortsCoordinates %>% dplyr::select(starts_with("lat"))
  lat <- lat %>% tidyr::gather(Number, value) %>% select(value)
  colnames(lat) <- "lat"
  alt <- EffortsCoordinates %>% dplyr::select(starts_with("alt"))
  alt <- alt %>% tidyr::gather(Number, value) %>% select(value)
  colnames(alt) <- "alt"
  EffortsCoordinates <- cbind(lon, lat, alt)
  EffortsCoordinates <- EffortsCoordinates %>%
    dplyr::mutate(Name = paste0("H", efforts$Number)) %>%
    dplyr::mutate(Time = efforts$Time)
  # ----------------------------------------------------------------------------
  # Join track and effort coordinates
  # ----------------------------------------------------------------------------
  Bestzeit <- as.numeric(dplyr::first(data$Bestzeit))/60
  track <- dplyr::left_join(track, EffortsCoordinates)
  track$Name[1] <- "Start"
  track$Name[nrow(track)] <- "Ziel"
  track$Time[1] <- 0
  track$Time[nrow(track)] <- Bestzeit
  # ----------------------------------------------------------------------------
  # Extract minute marker coordinates from kml list
  # ----------------------------------------------------------------------------
  MPCount <- ifelse(round(Bestzeit) - Bestzeit == 0, Bestzeit - 1, floor(Bestzeit))
  minMP <- EffortCount + 1
  maxMP <- EffortCount + MPCount
  MPCoordinates <- as.data.frame(kml[minMP:maxMP])
  colnames(MPCoordinates) <- paste(c("lon", "lat", "alt"), rep(1:MPCount, each = 3))
  lon <- MPCoordinates %>% dplyr::select(starts_with("lon"))
  lon <- lon %>% tidyr::gather(Number, value) %>% select(value)
  colnames(lon) <- "lon"
  lat <- MPCoordinates %>% dplyr::select(starts_with("lat"))
  lat <- lat %>% tidyr::gather(Number, value) %>% select(value)
  colnames(lat) <- "lat"
  alt <- MPCoordinates %>% dplyr::select(starts_with("alt"))
  alt <- alt %>% tidyr::gather(Number, value) %>% select(value)
  colnames(alt) <- "alt"
  MPCoordinates <- cbind(lon, lat, alt)
  Number <- seq(1:MPCount)
  MPCoordinates <- MPCoordinates %>% dplyr::mutate(Name = paste0("M", Number))
  MPCoordinates <- MPCoordinates %>% dplyr::mutate(Time = Number)
  Start <- track[1,]
  # ----------------------------------------------------------------------------
  # Combine effort coordinates and minute marker coordinates to one dataframe
  # ----------------------------------------------------------------------------
  Coordinates <- rbind(EffortsCoordinates, MPCoordinates, Start) %>%
    arrange(Time)
  # ------------------------------------------------------------------------------
  # Identify periods of high acceleration as possible start times in data
  # ------------------------------------------------------------------------------
  data <- data %>% dplyr::mutate(Datum = as.Date(Zeit))
  data <- data %>% dplyr::group_by(ID, Datum)
  data <- data %>% dplyr::mutate(acc = tsibble::difference(V))
  data <- data %>% dplyr::mutate(acc = slider::slide(acc, mean, .after = 10))
  data <- dplyr::ungroup(data)
  # ------------------------------------------------------------------------------
  # Find start time on different tracks
  # ------------------------------------------------------------------------------
  data <- data %>% dplyr::mutate(ID_Datum = paste0(ID, "_", Datum))
  Start <- function(filter) {
    filter = dplyr::enquo(filter)
    data_filtered <- data %>% dplyr::filter(ID_Datum == !!filter & acc > 15)
    LatNull <- Coordinates$lat[Coordinates$Name == "Start"]
    LonNull <- Coordinates$lon[Coordinates$Name == "Start"]
    x <- ((data_filtered$Lon-LonNull)*pi/180)*cos((data_filtered$Lat*pi)/180)*6371000
    y <- ((data_filtered$Lat-LatNull)*pi/180)*6371000
    Distance <- (x^2 + y^2)^0.5
    index <- which.min(Distance)
    data_filtered$Zeit[index]
  }
  ID_list <- unique(data$ID_Datum)
  Start_Times <- sapply(ID_list, Start, simplify = FALSE)
  Start_Times <- as.data.frame(Start_Times)
  colnames(Start_Times) <- paste0("ID", ID_list)
  Start_XC <- Start_Times %>% tidyr::pivot_longer(cols = dplyr::everything(), names_to = "ID",
                                           values_to = "Start")
  Start_XC <- Start_XC %>% dplyr::mutate(ID = as.numeric(unlist(str_split(ID, "_"))))
  Start_XC <- Start_XC %>% dplyr::mutate(ID = as.numeric(gsub("ID", "", ID)))
  data <- dplyr::left_join(data, Start_XC)
  # ------------------------------------------------------------------------------
  # Find jumps and minute markers on different tracks
  # ------------------------------------------------------------------------------
  Jumps <- function(filter) {
    filter = dplyr::enquo(filter)
    data_filtered <- data %>% dplyr::filter(ID == !!filter & Zeit > Start)
    FindJumps <- function(index) {
      LatNull <- Coordinates$lat[index]
      LonNull <- Coordinates$lon[index]
      x <- ((data_filtered$Lon-LonNull)*pi/180)*cos((data_filtered$Lat*pi)/180)*6371000
      y <- ((data_filtered$Lat-LatNull)*pi/180)*6371000
      Distance <- (x^2 + y^2)^0.5
      which.min(Distance)
    }
    Coordinates_list <- seq(from = 2, to = nrow(Coordinates))
    index <- sapply(Coordinates_list, FindJumps)
    data_filtered$Zeit[index]
  }
  ID_list <- unique(data$ID)
  Jump_Times <- sapply(ID_list, Jumps, simplify = FALSE)
  Jump_Times <- as.data.frame(Jump_Times)
  colnames(Jump_Times) <- paste0("ID", ID_list)
  Jump_Times <- rbind(Start_Times, Jump_Times)
  Jump_Times <- Jump_Times %>% dplyr::mutate(Efforts = Coordinates$Name,
                                      Index = seq(1:nrow(Jump_Times)))
  # ------------------------------------------------------------------------------
  # Remove jump times after elimination in incomplete XC rides
  # ------------------------------------------------------------------------------
  if (sum(!is.na(data$Hindernis))>0) {
    maxJump <- data %>% dplyr::group_by(ID, Datum) %>%
      dplyr::summarize(Hindernis = dplyr::first(Hindernis)) %>% dplyr::ungroup()
    maxJump <- maxJump %>% dplyr::mutate(Hindernis = ifelse(is.na(Hindernis),
                                                     dplyr::last(EffortsCoordinates$Name),
                                                     Hindernis))
    maxJump <- maxJump %>% dplyr::mutate(ID = paste0("ID", ID))
    ID_list <- paste0("ID", ID_list)
    F <- function(x) {
      which(Coordinates$Name == x)
    }
    vector <- maxJump$Hindernis
    Hindernis_Index <- sapply(vector, F)
    maxJump <- maxJump %>% dplyr::mutate(maxIndex = Hindernis_Index)
    maxJump <- maxJump %>% dplyr::mutate(maxIndex = unname(maxJump$maxIndex))

    incompleteXC <- data %>% dplyr::filter(!is.na(Hindernis)) %>%
      dplyr::mutate(ID = paste0("ID", ID))
    incompleteXC <- unique(incompleteXC$ID)
    incompleteXC_sym <- dplyr::syms(incompleteXC)

    Find_maxJump <- function (new_col) {
      ID <- dplyr::enquo(new_col)
      Effort_index <- maxJump %>% dplyr::filter(maxJump$ID == !! as_label(ID))
      Effort_index <- Effort_index$maxIndex
      new_col <- dplyr::enquo(new_col)
      new_col_name <- dplyr::quo_name(new_col)
      min <- Effort_index + 1
      max <- nrow(Coordinates)
      index <- seq(from = min, to = max)
      Jump_Times <- Jump_Times %>% dplyr::mutate(!!new_col_name := replace(!!new_col, index, NA))
      Jump_Times
    }

    liste <- lapply(incompleteXC_sym, Find_maxJump)
    names(liste) <- incompleteXC

    listNA <- function(ID) {
      df <- liste[[ID]]
      new_df <- df[, colSums(is.na(df)) > 0]
      new_df
    }

    newlist <- lapply(incompleteXC, listNA)
    names(newlist) <- incompleteXC
    Jump_Times_oNA <- Jump_Times %>% dplyr::select(!all_of(incompleteXC))
    Jump_Times <- cbind(Jump_Times_oNA, newlist)
    Jump_Times_IDs <- Jump_Times %>% dplyr::select(-Efforts, -Index) %>% as.matrix()
    #Jump_Times_IDs <- apply(Jump_Times_IDs, 2, sort, na.last = TRUE)
    Jump_Times_ordered <- cbind(Jump_Times_IDs, dplyr::select(Jump_Times, Efforts, Index))
    Jump_Times_long <- Jump_Times_ordered %>% tidyr::pivot_longer(!c(Efforts, Index),
                                                           names_to = "ID",
                                                           values_to = "Zeit")
    Jump_Times_long$ID <- as.numeric(gsub("ID", "", Jump_Times_long$ID))
    Jump_Times_long$Zeit <- as.POSIXct(Jump_Times_long$Zeit, tz = "Europe/Berlin")
    data <- dplyr::left_join(data, dplyr::select(Jump_Times_long, -Index))
    # --------------------------------------------------------------------------
    # Calculate finish time
    # --------------------------------------------------------------------------
    Start_Zeit <- data %>% dplyr::filter(Efforts == "Start") %>%
      dplyr::mutate(Start_Zeit = Zeit) %>%
      dplyr::select(ID, Datum, Start_Zeit)
    data <- dplyr::left_join(data, Start_Zeit)
    data <- data %>%
      dplyr::mutate(Zeit_Gelände_sec = as.numeric(Zeit_Gelände))
    Ziel_Zeit_NA <- data %>% dplyr::filter(!is.na(Hindernis) & Hindernis == Efforts) %>%
      dplyr::mutate(Ziel_Zeit = Zeit) %>% dplyr::select(ID, Datum, Ziel_Zeit)
    Ziel_Zeit_nonNA <- data %>% dplyr::filter(is.na(Hindernis) & Zeit == Start_Zeit) %>%
      dplyr::mutate(Ziel_Zeit = Start_Zeit + Zeit_Gelände_sec - 1) %>%
      dplyr::select(ID, Datum, Ziel_Zeit)
    Ziel_Zeit <- rbind(Ziel_Zeit_NA, Ziel_Zeit_nonNA)
    data <- dplyr::left_join(data, Ziel_Zeit)
    # --------------------------------------------------------------------------
    # Assign intervals based on start and finish time
    # --------------------------------------------------------------------------
    data <- data %>%
      dplyr::mutate(Intervall = dplyr::case_when(Zeit < Start_Zeit ~ "WU",
                                   Zeit >= Start_Zeit &
                                     Zeit <= Ziel_Zeit ~ "XC",
                                   Zeit > Ziel_Zeit ~ "CD"))
  }
}
