#' calculateFrequency_hz
#' calculate eye tracker temporal resolution (Hz)
#'
#' @param data a dataframe, with recording timestamps IN MILLISECONDS
#' @param timepointIDs a range of timepoints by timepoint ID to include. Default includes all timepoints
#'
#' @return recordingFrequency_hz
#' @export
#'
#' @examples
#' exampledata <- data.frame("recordingTimestamp_ms" = seq(1, 1000, 2))
#' calculateFrequency_hz(exampledata)
#' calculateFrequency_hz(exampledata, 2:nrow(exampledata)) # excludes the first timepoint
#'
calculateFrequency_hz <- function(data, timepointIDs = 1:nrow(data)) {
  recordingFrequency_ms <- mean(diff(data$recordingTimestamp_ms[timepointIDs], lag = 1))
  recordingFrequency_hz <- 1000 / round(recordingFrequency_ms, 1)
  # TODO: check precision on rounding
  # TODO: set precision on recHz so it's consistent? 5 digits?
  return(recordingFrequency_hz)
}
