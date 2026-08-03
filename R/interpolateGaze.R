#' Interpolate gaps in gaze data
#'
#' @param data dataframe
#' @param recordingFrequency_hz recording Hz (calculated in calculateFrequency_hz)
#' @param columnsToInterpolate list of column names to be interpolated
#' @param maxGapLength_ms integer for max ms of gaps to interpolate over. Default value 50ms (pg. 119 Liz's dissertation)
#' @param ... additional passed parameters from parent function
#'
#' @importFrom zoo na.approx
#'
#' @return data with 8 new columns of interpolated left & right X/Y gaze points, left & right pupils, left and right Z position
#' @export
#

interpolateGaze <-
  function(data, recordingFrequency_hz, columnsToInterpolate, maxGapLength_ms = 50, ...) {
    # gap duration / sampling rate = number of samples in gap to update
    sampling.dur <- 1000 / recordingFrequency_hz
    maxGapPoints <- floor(maxGapLength_ms / sampling.dur)
    print(
      paste(
        "Filling in gaps of ",
        maxGapLength_ms,
        "ms. For your sampling rate, this is equivalent to ",
        maxGapPoints,
        " gaze points",
        sep = ""
      )
    )

    #Create new names for cols, stripping a trailing ".valid" so downstream
    #column names are unchanged (e.g. "gazeLeftX.valid" -> "gazeLeftX.int",
    #not "gazeLeftX.valid.int")
    newCols <- paste0(sub("\\.valid$", "", columnsToInterpolate), ".int")

    #na.approx replaces NAs by interpolation
    for (int in 1:length(columnsToInterpolate)) {
      n <- newCols[int]
      i <- columnsToInterpolate[int]
      data[[n]] <-
        round(na.approx(data[[i]], na.rm = FALSE, maxgap = maxGapPoints), 2)
    }


    return(data)

  }
