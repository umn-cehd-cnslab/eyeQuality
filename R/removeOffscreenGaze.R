#' Remove offscreen gazepoints and related interpolations - rename to excludeOffscreenGazepoint?
#'
#' @param data your dataframe
#' @param gazeX string column containing name of variable with X gaze data
#' @param gazeY string column containing name of variable with Y gaze data
#' @param distanceZ string column containing name of variable with Z distance data in mm
#' @param overwrite optional list of column names in addition to gaze columns which should be replaced by NA when offscreen. Default = NA
#' @param displayResolutionX_px X Display resolution in pixels
#' @param displayResolutionY_px Y Display resolution in pixels
#' @param displayDimensionX_mm X Display dimension in mm
#' @param displayDimensionY_mm Y Display dimension in mm
#' @param offscreenValidityRange_va integer Degrees of Visual angle outside of screen which can be kept as valid. Default = 5 VA
#' @param verbose optional Boolean. When `TRUE`, emits a P3-10 diagnostic
#'   ("detected N samples with gaze coordinates outside display bounds")
#'   plus row ranges for any long contiguous run of offscreen samples.
#'   Default `FALSE`; purely additive, does not change which samples are
#'   marked offscreen.
#' @param ... additional arguments are of either the form value or tag = value. Component names are created based on the tag (if present) or the deparsed argument itself.
#'
#' @return data
#' @export
#'
removeOffscreenGaze <-
  function(data,
           gazeX,
           gazeY,
           distanceZ,
           overwrite = NA,
           displayResolutionX_px,
           displayResolutionY_px,
           displayDimensionX_mm,
           displayDimensionY_mm,
           offscreenValidityRange_va = 5,
           verbose = FALSE,
           ...) {
    # get participant's median distance from screen
    medZ <- median(data[[distanceZ]], na.rm = TRUE)

    # get new acceptable boundaries
    display.vax <-
      round(calculateVisualAngle(displayResolutionX_px, medZ, displayResolutionX_px, displayDimensionX_mm), 2) + offscreenValidityRange_va
    display.vay <-
      round(calculateVisualAngle(displayResolutionY_px, medZ, displayResolutionY_px, displayDimensionY_mm), 2) + offscreenValidityRange_va

    # set acceptable boundaries in pixel space
    resx.max <-
      convertVisualAngToPixels(display.vax, medZ, displayResolutionX_px, displayDimensionX_mm)
    resx.min <-
      convertVisualAngToPixels(-display.vax, medZ, displayResolutionX_px, displayDimensionX_mm)
    resy.max <-
      convertVisualAngToPixels(display.vay, medZ, displayResolutionY_px, displayDimensionY_mm)
    resy.min <-
      convertVisualAngToPixels(-display.vay, medZ, displayResolutionY_px, displayDimensionY_mm)

    # get list to label datapoints as on or offscreen, based on acceptable boundaries
    invalid_offscreen <-
      detectOffscreenGaze(
        data,
        gazeX,
        gazeY,
        displayResolutionX_px = resx.max,
        displayResolutionY_px = resy.max,
        minDisplayResolutionX_px = resx.min,
        minDisplayResolutionY_px = resy.min
      )

    # P3-10: report samples flagged offscreen before they're masked to NA
    # below -- the plan's illustrative "detected N samples with gaze
    # coordinates outside display bounds" diagnostic.
    eyeLabel <- if (grepl("Left", gazeX, ignore.case = TRUE)) {
      "left eye"
    } else if (grepl("Right", gazeX, ignore.case = TRUE)) {
      "right eye"
    } else {
      "gaze"
    }
    .diagnose_consecutive_runs(
      invalid_offscreen == "offscreen",
      paste0(eyeLabel, " gaze coordinates outside display bounds"),
      verbose
    )

    # mark as NA
    # Get columns to be overwritten if flagged as offscreen
    replace_cols <- c(gazeX, gazeY, overwrite)
    replace_rows <- which(invalid_offscreen == "offscreen")

    # replace all offscreen columns with NA
    for (rc in replace_cols) {
      if (is.na(rc)) {
        next()
      }
      data[replace_rows, rc] <- NA
    }

    # for these out-of-range gazepoints, update offscreen label to whether or not (1 / 0) gp is excluded as binary
    invalid_offscreen <- ifelse(invalid_offscreen == "offscreen", "offscreen.exclusionary", invalid_offscreen)

    # add new column for offscreen label
    if (grepl("Left", gazeX, ignore.case = TRUE)) {
      data$gazeLeft.offscreen <- invalid_offscreen
    } else if (grepl("Right", gazeX, ignore.case = TRUE)) {
      data$gazeRight.offscreen <- invalid_offscreen
    } else {
      data$gaze.offscreen <- invalid_offscreen
    }

    return(data)
  }
