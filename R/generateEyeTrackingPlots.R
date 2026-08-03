#' generateEyeTrackingPlots.R
#'
#' @param data dataframe
#' @import ggplot2
#' @import ggpubr
#' @return plots
#' @export
#'
generateEyeTrackingPlots <- function(data) {
  # Use the ".valid" column when present (includeIntermediates = TRUE), and
  # fall back to the raw column name otherwise (default includeIntermediates
  # = FALSE, where removeInvalidGaze()'s ".valid" columns are stripped before
  # eyeQuality() returns/saves its output).
  resolveValidCol <- function(data, col) {
    validCol <- paste0(col, ".valid")
    if (validCol %in% colnames(data)) validCol else col
  }

  # Plot raw data for each channel
  rawGaze_leftX <- data %>% plotGazeAndBlinks(resolveValidCol(data, "gazeLeftX")) + ggplot2::labs(y = "Left eye x-position")
  rawGaze_leftY <- data %>% plotGazeAndBlinks(resolveValidCol(data, "gazeLeftY")) + ggplot2::labs(y = "Left eye y-position")
  rawGaze_rightX <- data %>% plotGazeAndBlinks(resolveValidCol(data, "gazeRightX")) + ggplot2::labs(y = "Right eye x-position")
  rawGaze_rightY <- data %>% plotGazeAndBlinks(resolveValidCol(data, "gazeRightY")) + ggplot2::labs(y = "Right eye y-position")

  rawGaze_leftPupil <- data %>% plotGazeAndBlinks(resolveValidCol(data, "pupilLeft")) + ggplot2::labs(y = "Left pupil measurement")
  rawGaze_rightPupil <- data %>% plotGazeAndBlinks(resolveValidCol(data, "pupilRight")) + ggplot2::labs(y = "Right pupil measurement")

  rawGaze_leftZ <- data %>% plotGazeAndBlinks(resolveValidCol(data, "distanceLeftZ")) + ggplot2::labs(y = "Left eye z-distance")
  rawGaze_rightZ <- data %>% plotGazeAndBlinks(resolveValidCol(data, "distanceRightZ")) + ggplot2::labs(y = "Right eye z-distance")


  rawGazePlot <- ggpubr::ggarrange(
    rawGaze_leftX,
    rawGaze_rightX,
    rawGaze_leftY,
    rawGaze_rightY,
    rawGaze_leftZ,
    rawGaze_rightZ,
    rawGaze_leftPupil,
    rawGaze_rightPupil,
    labels = c("Left X", "Right X", "Left Y", "Right Y", "Left Z", "Right Z", "Left Pupil", "Right Pupil"),
    label.x = 0.7,
    label.y = 0.08,
    ncol = 2, nrow = 4,
    common.legend = TRUE, legend = "bottom"
  )

  # Get gaze heatmap showing density of fixations on the screen
  gazeHeatmap <- plotGazeHeatmap(data)

  # Get final plot of smoothed gaze for X and Y positions
  gaze_X <- data %>% plotGazeAndBlinks("gazeX.preprocessed_px", showFixations = TRUE) + ggplot2::labs(x = "Recording Timestamp (ms)", y = "Gaze x-position")
  gaze_Y <- data %>% plotGazeAndBlinks("gazeY.preprocessed_px", showFixations = TRUE) + ggplot2::labs(x = "Recording Timestamp (ms)", y = "Gaze y-position")

  gazePlot <- ggpubr::ggarrange(
    gaze_X,
    gaze_Y,
    labels = c("Gaze X", "Gaze Y"),
    label.x = 0.7,
    label.y = 0.08,
    ncol = 2, nrow = 1,
    common.legend = TRUE, legend = "bottom"
  )

  return(list(rawGazePlot, gazeHeatmap, gazePlot))
}
