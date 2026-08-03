#' Classify Offscreen gaze points in preprocessing pipeline
#'
#' @param data dataframe
#' @param displayResolutionX_px X display resolution in pixels
#' @param displayResolutionY_px Y display resolution in pixels
#' @param eyeSelection_method "Maximize", "Strict", "Left", "Right"
#' @param ... additional arguments are of either the form value or tag = value. Component names are created based on the tag (if present) or the deparsed argument itself.
#'
#' @return data
#' @export
#'
classifyOffscreenGaze <- function(data,
                                  displayResolutionX_px,
                                  displayResolutionY_px,
                                  eyeSelection_method,
                                  ...) {
  ### Get offscreen information for left eye (after interpolation)
  gazeLeft.offscreenInRange <- detectOffscreenGaze(data,
    gazeX = "gazeLeftX.int",
    gazeY = "gazeLeftY.int",
    displayResolutionX_px,
    displayResolutionY_px
  )

  # recode new column as offscreen exclusionary / within range or onscreen
  data$gazeLeft.offscreen <- ifelse(gazeLeft.offscreenInRange == "offscreen" & data$gazeLeft.offscreen != "offscreen.exclusionary",
    "offscreen.withinRange", data$gazeLeft.offscreen
  )
  data$gazeLeft.offscreen[gazeLeft.offscreenInRange == "onscreen"] <- "onscreen"

  ### Get offscreen information for right eye (after interpolation)
  gazeRight.offscreenInRange <- detectOffscreenGaze(data,
    gazeX = "gazeRightX.int",
    gazeY = "gazeRightY.int",
    displayResolutionX_px,
    displayResolutionY_px
  )


  # recode new column as offscreen exclusionary / within range or onscreen
  data$gazeRight.offscreen <- ifelse(gazeRight.offscreenInRange == "offscreen" & data$gazeRight.offscreen != "offscreen.exclusionary",
    "offscreen.withinRange", data$gazeRight.offscreen
  )
  data$gazeRight.offscreen[gazeRight.offscreenInRange == "onscreen"] <- "onscreen"

  ### Get final classification of offscreen-ness based on EyeSelection option
  ## MAXIMIZE OR STRICT
  if (grepl("Maximize|Strict", eyeSelection_method)) {
    data$offscreen.eyeSelect <- detectOffscreenGaze(data,
      gazeX = "gazeX.eyeSelect",
      gazeY = "gazeY.eyeSelect",
      displayResolutionX_px,
      displayResolutionY_px
    )

    # recode new column as offscreen exclusionary / within range or onscreen
    data$offscreen.classification <- classifyOffscreenGaze_byEyeSelection(
      eyeSelection_method, data$gazeLeft.offscreen,
      data$gazeRight.offscreen, data$offscreen.eyeSelect
    )

    data <- data %>%
      select(-matches("^offscreen\\.eyeSelect$"))
  }

  ## LEFT
  if (eyeSelection_method == "Left") {
    data$offscreen.classification <- data$gazeLeft.offscreen
  }
  ## RIGHT
  if (eyeSelection_method == "Right") {
    data$offscreen.classification <- data$gazeRight.offscreen
  }

  return(data)
}
