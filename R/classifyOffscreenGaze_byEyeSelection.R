#' Categorize offScreen gazepoints based on eye selection method
#'
#' @param eyeSelection_method "Maximize", "Strict", "Left", "Right"
#' @param gazeLeft.offscreen list of ordered gaze point offscreen assignments for left eye
#' @param gazeRight.offscreen list of ordered gaze point offscreen assignments for right eye
#' @param offscreen.eyeSelect list of ordered gaze point offscreen assignments for eyeSelect column
#'
#' @return categoryList
#' @export
#'
classifyOffscreenGaze_byEyeSelection <- function(eyeSelection_method, gazeLeft.offscreen,
                                                 gazeRight.offscreen,
                                                 offscreen.eyeSelect) {
  # create list of df length
  categoryList <- rep(NA, length(offscreen.eyeSelect))

  if (eyeSelection_method == "Maximize") {
    categoryList <-
      case_when(
        # if both gp were excluded
        gazeLeft.offscreen == "offscreen.exclusionary" &
          (gazeRight.offscreen == "offscreen.exclusionary" | is.na(gazeRight.offscreen)) ~ "offscreen.exclusionary",
        gazeRight.offscreen == "offscreen.exclusionary" &
          (gazeLeft.offscreen == "offscreen.exclusionary" | is.na(gazeLeft.offscreen)) ~ "offscreen.exclusionary",

        # if one gp is within range or NA, we would use offscreen.eyeSelect to determine
        ((gazeLeft.offscreen == "offscreen.withinRange" | is.na(gazeLeft.offscreen)) & offscreen.eyeSelect == "offscreen") |
          ((gazeRight.offscreen == "offscreen.withinRange" | is.na(gazeLeft.offscreen)) & offscreen.eyeSelect == "offscreen") ~ "offscreen.withinRange",

        # anything marked as onscreen for eyeSelect is correct (case 1 in range / onscreen, with average onscreen, case both onscreen)
        offscreen.eyeSelect == "onscreen" ~ "onscreen",

        # when both values are NA
        is.na(gazeLeft.offscreen) & is.na(gazeRight.offscreen) ~ NA,
        # default
        TRUE ~ NA
      )
  }

  if (eyeSelection_method == "Strict") {
    categoryList <-
      case_when(
        # when either value is NA
        is.na(gazeLeft.offscreen) | is.na(gazeRight.offscreen) ~ NA,

        # if both gp were excluded
        gazeLeft.offscreen == "offscreen.exclusionary" | gazeRight.offscreen == "offscreen.exclusionary" ~ "offscreen.exclusionary",

        # if one gp is within range or NA, we would use offscreen.eyeSelect to determine
        (gazeLeft.offscreen == "offscreen.withinRange" & offscreen.eyeSelect == "offscreen") |
          (gazeRight.offscreen == "offscreen.withinRange" & offscreen.eyeSelect == "offscreen") ~ "offscreen.withinRange",

        # anything marked as onscreen for eyeSelect is correct
        offscreen.eyeSelect == "onscreen" ~ "onscreen",

        # default - covers other cases
        TRUE ~ NA
      )
  }
  return(categoryList)
}
