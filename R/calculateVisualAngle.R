#' Calculate Visual Angle from pixel position
#'
#' @param gaze_px Integer, gaze position as a pixel coordinate (X or Y)
#' @param distanceZ  Integer, distance from the screen in mm
#' @param displayResolution_px Integer, display resolution on the relevant dimension (X ir Y)
#' @param displayDimension_mm Integer, display dimension on the relevant dimension (X or Y)
#'
#' @return returns Angle in degrees
#' @export
#'
calculateVisualAngle <- function(gaze_px, distanceZ, displayResolution_px, displayDimension_mm) {
  Rad <- atan2(((gaze_px - ((
    displayResolution_px + 1
  ) * .5)) * displayDimension_mm), (distanceZ * displayResolution_px))
  Ang <- Rad * (180 / pi)
  return(Ang)
}
