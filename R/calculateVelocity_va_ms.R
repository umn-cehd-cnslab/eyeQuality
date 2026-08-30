#' Calculate Velocity in Visual Angle
#'
#' Establish timestamps and
#'
#' @param data dataframe
#' @param gazeX_va string, column containing x coordinates of gaze stream in visual angles
#' @param gazeY_va string, column containing y coordinates of gaze stream in visual angles
#' @param timestamp string, column containing recording timestamps in ms
#' @param ... additional passed parameters from parent function
#'
#' @importFrom dplyr lag
#'
#' @return data a dataframe with calculated velocity for gaze X and gaze Y and euclidean distance
#' @export
#'
calculateVelocity_va_ms <- function(data, gazeX_va, gazeY_va, timestamp, ...) {
  print("Calculating velocity...")
  # define velocity new column names
  x.vel <- "velocityX_va_ms"
  y.vel <- "velocityY_va_ms"

  # specify data stream from column titles
  xstream <- data[[gazeX_va]]
  ystream <- data[[gazeY_va]]
  timestamps <- data[[timestamp]]

  # You should calculate in visual angle space, not pixels

  # Establish VA Velocity in x, y, euclidean in VA PER SECOND
  data[[x.vel]] <-
    c(((xstream - dplyr::lag(
      dplyr::lag(xstream)
    )) / (timestamps - dplyr::lag(
      dplyr::lag(timestamps)
    )) * 1000)[2:length(xstream)], NA)
  data[[y.vel]] <-
    c(((ystream - dplyr::lag(
      dplyr::lag(ystream)
    )) / (timestamps - dplyr::lag(
      dplyr::lag(timestamps)
    )) * 1000)[2:length(ystream)], NA)
  # get euclidean velocity
  data$velocityEuclidean_va_ms <-
    round(sqrt(data[[x.vel]]^2 + data[[y.vel]]^2), 2)

  return(data)
}
