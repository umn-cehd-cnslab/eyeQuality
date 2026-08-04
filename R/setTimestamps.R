#' Set time range of data to be processed
#'
#' @param data your dataframe of eyetracking data
#' @param timeStart an integer of recording timestamp where data of interest begins
#' @param timeEnd an integer of recording timestamp where data of interest ends
#' @param setTimes boolean TRUE/FALSE if we should filter data based on the timeStart and timeEnd provided
#' @param ... arguments to be passed
#'
#' @importFrom rlang .data
#' @return dataframe containing only data within the specified timeframe
#' @export
#'
setTimestamps <-
  function(data, timeStart, timeEnd, setTimes = TRUE, ...) {
    if (setTimes) {
      data <- data %>%
        filter(.data$recordingTimestamp_ms >= timeStart &
          .data$recordingTimestamp_ms <= timeEnd)
    }
    return(data)
  }
