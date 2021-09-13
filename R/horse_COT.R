#' Horse Oxygen Cost of Transport
#'
#' This function calculates the Oxygen Cost of Transport (COT in ml/kg)
#' from gps distance, acceleration and elevation for multiple rides in the same dataframe
#' The dataframe "data" has to be structured as follows:
#' col "ID" with horse identifier, col "Zeit" with timestamp (tz = "Europe/Berlin"),
#' col "GPSDistance" with gps distance in m
#' col "Acceleration" with radial acceleration in m/s^2
#' col "Altitude" with elevation in m

#' @export
horse_COT <- function(data) {
  #-----------------------------------------------------------------------------
  # Calculate COT from gps distance (COTp)
  #-----------------------------------------------------------------------------
  data <- data %>% dplyr::mutate(COTp = 0.123*GPSDistance)
  #-----------------------------------------------------------------------------
  # Calculate COT from acceleration (COTa)
  #-----------------------------------------------------------------------------
  # Calculate overall acceleration (g)
  data <- data %>% dplyr::mutate(g = (Acceleration^2 + 9.81^2)^0.5)
  # Calculate angle between g and the ground
  data <- data %>% dplyr::mutate(alpha = (atan(g/Acceleration))*180/pi)
  # Calculate equivalent slope (ES)
  data <- data %>% dplyr::mutate(ES = tan((90-alpha)*pi/180))
  # Calculate equivalent mass (EM)
  data <- data %>% dplyr::mutate(EM = g/9.81)
  # Calculate COTa
  data <- data %>% dplyr::mutate(COTa = (ifelse(ES >= 0,
                                                1.561*ES,
                                                1.591*ES + 9.762*(ES^2) + 14*(ES^3)))*GPSDistance*EM)
  data <- data %>% dplyr::select(-g, -alpha, -ES, -EM)
  #-----------------------------------------------------------------------------
  # Calculate COT from gps elevation (COTe)
  #-----------------------------------------------------------------------------
  data <- data %>% dplyr::mutate(Datum = as.Date(Zeit, tz = "Europe/Berlin"))
  data <- data %>% dplyr::group_by(ID, Datum) %>% tidyr::nest()
  data <- data %>% dplyr::mutate(Delta_A = purrr::map(data, ~as.numeric(tsibble::difference(.x$Altitude))))
  data <- tidyr::unnest(data, cols = c(data, Delta_A))
  data <- dplyr::ungroup(data)
  data <- data %>% dplyr::mutate(Slope = ifelse(GPSDistance != 0, Delta_A/GPSDistance, 0))
  data <- data %>% dplyr::select(-Delta_A)
  data <- data %>% dplyr::mutate(COTe = (ifelse(Slope >= 0,
                                                1.561*Slope,
                                                1.591*Slope + 9.762*(Slope^2) + 14*(Slope^3)))
                                 *GPSDistance)
  #-----------------------------------------------------------------------------
  # Calculate COT sum
  #-----------------------------------------------------------------------------
  data <- data %>% dplyr::mutate(COT = COTp + COTa + COTe)
}
