#' Horse GPS Distance
#'
#' This function calculates the distance (GPSDistance) in meters between locations
#' from gps latitude (Lat) and longitude (Lon) for multiple rides in the same dataframe
#' The dataframe "data" has to be structured as follows:
#' col "ID" with horse identifier, col "Zeit" with timestamp (tz = "Europe/Berlin"),
#' col "Lat" with Latitude (decimal), col "Lon" with Longitude (decimal)

#' @export
horse_GPSDist <- function(data) {
  data <- data %>% dplyr::mutate(Datum = as.Date(Zeit, tz = "Europe/Berlin"))
  data <- data %>% dplyr::group_by(ID, Datum) %>% tidyr::nest()
  data <- data %>% dplyr::mutate(Delta_t = purrr::map(data, ~as.numeric(tsibble::difference(.x$Zeit))))
  data <- data %>% dplyr::mutate(Lon_first = purrr::map(data, ~dplyr::first(.x$Lon)))
  data <- data %>% dplyr::mutate(Lat_first = purrr::map(data, ~dplyr::first(.x$Lat)))
  data <- tidyr::unnest(data, cols = c(data, Delta_t, Lon_first, Lat_first))
  data <- data %>%
    dplyr::mutate(x = ((Lon - Lon_first)*pi/180)*cos((Lat*pi)/180)*6371000)
  data <- data %>%
    dplyr::mutate(y = ((Lat - Lat_first)*pi/180)*6371000)
  data <- data %>% dplyr::group_by(ID, Datum) %>% tidyr::nest()
  data <- data %>% dplyr::mutate(Delta_x = purrr::map(data, ~as.numeric(tsibble::difference(.x$x))))
  data <- data %>% dplyr::mutate(Delta_y = purrr::map(data, ~as.numeric(tsibble::difference(.x$y))))
  data <- tidyr::unnest(data, cols = c(data, Delta_x, Delta_y))
  data <- dplyr::ungroup(data)
  data <- data %>%
    dplyr::mutate(GPSDistance = ifelse(Delta_t != 0,(((Delta_x/Delta_t)^2+(Delta_y/Delta_t)^2)^0.5)*Delta_t, 0))
  data <- data %>% dplyr::select(-Lat_first, -Lon_first, -x, -Delta_x, -y, -Delta_y)
}
