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
#' @section Screen-center pixel convention:
#' The `displayResolution_px / 2` center used here must stay in sync with
#' `calculateVisualAngle()`'s screen-center convention -- see that
#' function's matching roxygen section for the full trace of why
#' `displayResolution_px / 2` (not `(displayResolution_px + 1) / 2`) is the
#' correct center for this package's pixel coordinates, which derive from
#' Tobii's ADCS normalized `[0.0, 1.0]` display coordinates multiplied by
#' `displayResolution_px` (a continuous edge-to-edge span, not a discrete
#' 0-indexed grid). With matching centers, `calculateVisualAngle()` and
#' this function are exact algebraic inverses: writing `c` for the shared
#' center pixel, `calculateVisualAngle()` computes
#' `Ang = atan(((px - c) * dim_mm) / (distanceZ * res_px)) * 180 / pi`, and
#' this function computes
#' `px' = tan(Ang * pi / 180) * distanceZ * res_px / dim_mm + c`, which
#' algebraically simplifies back to `px' = (px - c) + c = px` for any
#' shared `c` -- so any mismatch between the two functions' center formulas
#' (as existed before this fix: `calculateVisualAngle()` used
#' `(res_px + 1) / 2` while this function used `res_px / 2`) introduces a
#' systematic half-pixel bias rather than a true round trip.
convertVisualAngToPixels <- function(gazeVA, distanceZ, displayResolution_px, displayDimension_mm) {
  px <-
    (tan(deg2rad(gazeVA)) * distanceZ * displayResolution_px / displayDimension_mm) + (displayResolution_px / 2)
  return(px)
}
