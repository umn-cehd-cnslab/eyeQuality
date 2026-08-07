#' Check that file contains any gaze data to be processed
#'
#' @param data a dataframe
#' @param gazeColumn column containing gaze data to assess
#' @param ... additional passed parameters from parent function
#'
#' @return dataExists a Boolean indicating if any valid gaze data exists
#' @export
#'
checkGazeDataExists <- function(data, gazeColumn, ...) {
  dataExists <- TRUE
  if (nrow(data) == 0) {
    dataExists <- FALSE
  }
  if (sum(!is.na(data[[gazeColumn]])) == 0) {
    dataExists <- FALSE
  }
  return(dataExists)
}
