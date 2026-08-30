#' Run renameColumns or direct to custom mapping
#'
#' @param data dataframe
#' @param software "TobiiStudio" or "TobiiPro"
#'
#' @importFrom stringr str_detect
#' @importFrom rlang .data
#' @return your reformatted data with standardized column names (Event, EventValue, RecordingDuration, ResolutionHeight, ResolutionWidth, EyeTrackerTimestamp, RecordingTimestamp, GazePointLeftX, GazePointLeftY, GazePointRightX, GazePointRightY, EyePosLeftX, EyePosLeftY, EyePosLeftZ, EyePosRightX, EyePosRightY, EyePosRightZ, PupilLeft, PupilRight, ValidityLeft, ValidityRight)
#'
standardizeColumnNames <- function(data, software) {
  if (str_detect(software, "TobiiStudio") |
    str_detect(software, "TobiiPro")) {
    renameColumns(data, software)
  } else {
    stop(
      "warning: Your software input should be 'TobiiPro' or  'TobiiStudio'.
          Other eye tracking programs are not supported by this package. To custom assign column names,
          from your software, please see our help document & template"
    )
  }
}
