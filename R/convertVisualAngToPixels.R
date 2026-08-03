#' Calculate pixel position from visual angle
#'
#' @param gazeVA integer Visual angle to be converted to pixel space
#' @param distanceZ integer distance Z from the screen in mm
#' @param displayResolution_px display resolution in px
#' @param displayDimension_mm display dimension in mm
#'
#' @importFrom pracma deg2rad
#'
#' @return px the pixel value for the input VA
#' @export
#'
convertVisualAngToPixels <- function(gazeVA, distanceZ, displayResolution_px, displayDimension_mm) {
  px <-
    (tan(deg2rad(gazeVA)) * distanceZ * displayResolution_px / displayDimension_mm) + (displayResolution_px /
      2)
  return(px)
}
