#' Run extractEventRows, after you have ran rmInvalidGP function to standardize data values.
#'
#' @param data dataframe
#' @param software Tobii Software version
#' @param ... additional arguments are of either the form value or tag = value. Component names are created based on the tag (if present) or the deparsed argument itself.
#'
#' @importFrom rlang .data
#'
#' @return a list: 1. dataframe with only gazestream data extracted, 2. dataframe with only event data extracted
#'

extractEventRows <- function(data, software, ...) {
  if (software == "TobiiPro") {
    gazeStreamData <- data %>%
      dplyr::filter(.data$Sensor == "Eye Tracker")
    eventData <- data %>%
      dplyr::filter(.data$Sensor != "Eye Tracker" |
        is.na(.data$Sensor))
  } else if (software == "TobiiStudio") {
    gazeStreamData <- data %>%
      dplyr::filter(.data$eyeTrackerTimestamp != -9999)
    eventData <- data %>%
      dplyr::filter(.data$eyeTrackerTimestamp == -9999 |
        is.na(.data$eyeTrackerTimestamp))
  } else {
    stop("extractEventRows: unrecognized software value '", software, "'")
  }
  return(list(gazeStreamData, eventData))
}
