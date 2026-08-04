#' Assign values to final preprocessed column names
#'
#' @param data dataframe
#' @param noise_reduction Boolean indicating if noise reduction should be implemented on gaze data
#' @param ... additional arguments are of either the form value or tag = value. Component names are created based on the tag (if present) or the deparsed argument itself.
#'
#' @return data
#'
assignFinalColumnNames <- function(data,
                                   smoothGaze_boolean,
                                   ...) {
  if (smoothGaze_boolean) {
    # final gaze pixel columns
    data$gazeX.preprocessed_px <- data$gazeX.smooth
    data$gazeY.preprocessed_px <- data$gazeY.smooth
    # final z distance column
    data$distanceZ.preprocessed_mm <- data$distanceZ.smooth
    # final pupil diameter column
    data$pupil.preprocessed <- data$pupil.smooth
  } else if (!noise_reduction) {
    data$gazeX.preprocessed_px <- data$gazeX.eyeSelect
    data$gazeY.preprocessed_px <- data$gazeY.eyeSelect
    # final z distance column
    data$distanceZ.preprocessed_mm <- data$distanceZ.eyeSelect
    # final pupil diameter column
    data$pupil.preprocessed <- data$pupil.eyeSelect
  }

  # final visual angle gaze columns
  data$gazeX.preprocessed_va <- data$gazeX_va
  data$gazeY.preprocessed_va <- data$gazeY_va

  # final velocity columns
  data$velocityX.preprocessed_va_ms <- data$velocityX.smooth_va_ms
  data$velocityY.preprocessed_va_ms <- data$velocityY.smooth_va_ms
  data$velocityEuclidean.preprocessed_va_ms <- data$velocityEuclidean.smooth_va_ms

  return(data)
}
