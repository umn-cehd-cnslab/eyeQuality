#' calculate visual angle from pixels
#'
#' @param data dataframe
#' @param displayResolutionX_px int display resolution for width in px
#' @param displayResolutionY_px int display resolution for height in px
#' @param displayDimensionX_mm int display width in mm
#' @param displayDimensionY_mm int display height in mm
#' @param gazeX string column name of gazepoints on X axis
#' @param gazeY string column name of gazepoints on Y axis
#' @param distanceZ string column name of distance from screen for each timepoint
#' @param ... additional passed parameters from parent function
#'
#' @return data_va dataframe with calculated visual angles in columns appended _va
#' @export
#'
calculateGaze_va <-
  function(data,
           displayResolutionX_px,
           displayResolutionY_px,
           displayDimensionX_mm,
           displayDimensionY_mm,
           gazeX,
           gazeY,
           distanceZ,
           ...) {
    print("Calculating VA...")
    data_va <- data

    # calculate visual angle
    data_va$gazeX_va <-
      round(calculateVisualAngle(data[[gazeX]], data[[distanceZ]], displayResolutionX_px, displayDimensionX_mm), 2)
    data_va$gazeY_va <-
      round(calculateVisualAngle(data[[gazeY]], data[[distanceZ]], displayResolutionY_px, displayDimensionY_mm), 2)

    return(data_va)
  }
