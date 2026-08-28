#' Calculate Visual Angle from pixel position
#'
#' @param gaze_px Integer, gaze position as a pixel coordinate (X or Y)
#' @param distanceZ  Integer, distance from the screen in mm
#' @param displayResolution_px Integer, display resolution on the relevant dimension (X or Y)
#' @param displayDimension_mm Integer, display dimension on the relevant dimension (X or Y)
#'
#' @return returns Angle in degrees
#' @export
#'
#' @section Screen-center pixel convention:
#' `gaze_px` values reaching this function originate from Tobii's Active
#' Display Coordinate System (ADCS): both adapters' `standardize()` rename
#' raw device columns straight onto `gazeLeftX/Y`/`gazeRightX/Y` with no
#' arithmetic in between (Tobii Studio's `"GazePointLeftX (ADCSpx)"` in
#' `R/tobii-studio-adapter.R`; Tobii Pro's `"Gaze point left X"`, which
#' Tobii Pro Lab exports in the same ADCS-derived pixel units, in
#' `R/tobii-pro-adapter.R`). Per Tobii's SDK documentation, ADCS is a
#' normalized coordinate where `(0.0, 0.0)` is the exact upper-left corner
#' of the active display area and `(1.0, 1.0)` is the exact lower-right
#' corner; the `...px` pixel variants are that normalized value multiplied
#' by `displayResolution_px`, so pixel 0 is the left/top display edge and
#' `displayResolution_px` is the right/bottom edge -- a continuous
#' edge-to-edge span, not a discrete 0-indexed pixel grid running
#' `0..(displayResolution_px - 1)`. The screen center under that convention
#' is therefore `displayResolution_px * .5` exactly (not
#' `(displayResolution_px + 1) * .5`, which would be the center of a
#' 1-indexed discrete grid). This also matches how `detectOffscreenGaze()`
#' (`R/detectOffscreenGaze.R`) already treats the on-screen pixel range as
#' `[0, displayResolution_px]`, and it makes this function an exact inverse
#' of `convertVisualAngToPixels()`, which uses the same
#' `displayResolution_px * .5` center -- see that function's matching
#' section for the round-trip argument.
calculateVisualAngle <- function(gaze_px, distanceZ, displayResolution_px, displayDimension_mm) {
  Rad <- atan2(((gaze_px - (displayResolution_px * .5)) * displayDimension_mm), (distanceZ * displayResolution_px))
  Ang <- Rad * (180 / pi)
  return(Ang)
}
