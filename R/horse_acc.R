#' Horse Acceleration
#'
#' This function calculates the radial acceleration in meters per squaresecond
#' from gps velocity (filtered) for multiple rides in the same dataframe
#' The dataframe "data" has to be structured as follows:
#' col "ID" with horse identifier, col "Zeit" with timestamp,
#' col "Delta_t" with time difference in seconds,
#' col "V_gefiltert" with filtered velocity in meters per minute

#' @export
horse_acc <- function(data) {
  data <- data %>% dplyr::mutate(Datum = as.Date(Zeit))
  data <- data %>% dplyr::group_by(ID, Datum) %>% tidyr::nest()
  data <- data %>% dplyr::mutate(Delta_V = purrr::map(data, ~as.numeric(tsibble::difference(.x$V_gefiltert))))
  data <- tidyr::unnest(data, cols = c(data, Delta_V))
  data <- dplyr::ungroup(data)
  data <- data %>%
    dplyr::mutate(Acceleration = ifelse(Delta_t != 0, (Delta_V/60)/Delta_t, 0))
  data <- data %>% dplyr::select(-Delta_V)
}
