#' Make height profile from Cross-Country App
#'
#' This function uses data from Cross-Country App and creates
#' a height profile
#' Following objects must be present:
#' "kml": kml-file with track and efforts from Cross-Country App
#'   list with length = number of efforts + number of minute markers + track,
#'   efforts and minute markers: double(1 x Lon, Lat, Alt),
#'   track: double(length track x Lon, Lat, Alt)
#' "efforts": csv-file with efforts from Cross-Country App
#'   dataframe (Type, Number, Name, Strides, Seconds.From.Start(mm:ss),
#'   Metres.From.Start, Seconds.To.Finish(mm:ss), Metres.To.Finish,
#'   Seconds.From.Previous(mm:ss), Metres.From.Previous, Comment)

#' @export
horse_heightProfilefromXCApp <- function(kml, efforts) {
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
  Bestzeit <- efforts$Time[nrow(efforts)] +
    as.numeric(sapply(strsplit(efforts$Seconds.To.Finish[nrow(efforts)], ":"), "[", 1))+
    as.numeric(sapply(strsplit(efforts$Seconds.To.Finish[nrow(efforts)], ":"), "[", 2))/60
  track <- dplyr::left_join(track, EffortsCoordinates)
  track$Name[1] <- "Start"
  track$Name[nrow(track)] <- "Ziel"
  track$Time[1] <- 0
  track$Time[nrow(track)] <- Bestzeit
  # ----------------------------------------------------------------------------
  # Calculate distance from GPS coordinates
  # ----------------------------------------------------------------------------
  track <- track %>% dplyr::mutate(Lon_first = first(lon),
                                   Lat_first = first(lat))
  track <- track %>% dplyr::mutate(x = ((lon - Lon_first)*pi/180)*cos((lat*pi)/180)*6371000,
                                   y = ((lat - Lat_first)*pi/180)*6371000)
  track <- track %>% dplyr::mutate(Delta_x = as.numeric(tsibble::difference(x)),
                                   Delta_y = as.numeric(tsibble::difference(y)))
  track <- track %>% dplyr::mutate(GPSDistance = ((Delta_x)^2+(Delta_y)^2)^0.5)
  track <- track %>% dplyr::select(-Lat_first, -Lon_first, -x, -Delta_x, -y, -Delta_y)
  # ----------------------------------------------------------------------------
  # Create data frame for height profile
  # ----------------------------------------------------------------------------

  height_profile <- track %>% dplyr::mutate(kumDistance = cumsum(tidyr::replace_na(GPSDistance,0)))
  height_profile <- height_profile %>% dplyr::mutate(alt_diff = alt - first(alt))
  height_profile
}
