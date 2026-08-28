# calculateVisualAngle() converts a pixel gaze coordinate to an angle (in
# degrees) subtended at the eye, via
#   Rad = atan2((gaze_px - displayResolution_px * 0.5) * displayDimension_mm,
#               distanceZ * displayResolution_px)
#   Ang = Rad * (180 / pi)
#
# The screen-center pixel is displayResolution_px * 0.5 -- see
# calculateVisualAngle()'s roxygen ("Screen-center pixel convention") for why:
# this package's gaze_px values derive from Tobii's ADCS normalized [0,1]
# coordinates multiplied by displayResolution_px, a continuous edge-to-edge
# span (pixel 0 = left/top edge, displayResolution_px = right/bottom edge),
# not a discrete 0-indexed pixel grid. The expected values below were
# corrected from an earlier (displayResolution_px + 1) * 0.5 formula that
# used the wrong center convention and was not an exact inverse of
# convertVisualAngToPixels() -- see that function's own tests for the
# round-trip proof.

test_that("calculateVisualAngle returns 0 for a gaze point at the exact screen center", {
  # center pixel for a 1920px-wide display is 1920 * 0.5 = 960
  result <- calculateVisualAngle(
    gaze_px = 960,
    distanceZ = 600,
    displayResolution_px = 1920,
    displayDimension_mm = 600
  )

  expect_equal(result, 0)
})

test_that("calculateVisualAngle matches a hand-derived off-center value", {
  # Rad = atan2((1400 - 1920*0.5) * 594, 600 * 1920)
  #     = atan2(440 * 594, 1152000) = atan2(261360, 1152000)
  # Ang = Rad * 180/pi
  # (Previously asserted 12.77 under the pre-fix (res+1)*0.5 = 960.5 center;
  # 12.78 is correct for the res*0.5 = 960 center.)
  result <- calculateVisualAngle(
    gaze_px = 1400,
    distanceZ = 600,
    displayResolution_px = 1920,
    displayDimension_mm = 594
  )

  expect_equal(round(result, 2), 12.78)
})

test_that("calculateVisualAngle returns a negative angle for a gaze point left/above center", {
  # Rad = atan2((0 - 1000*0.5) * 300, 500 * 1000)
  #     = atan2(-500 * 300, 500000) = atan2(-150000, 500000)
  # (Previously asserted -16.715 under the pre-fix (res+1)*0.5 = 500.5
  # center; -16.6992 is correct for the res*0.5 = 500 center.)
  result <- calculateVisualAngle(
    gaze_px = 0,
    distanceZ = 500,
    displayResolution_px = 1000,
    displayDimension_mm = 300
  )

  expect_equal(round(result, 4), -16.6992)
})
