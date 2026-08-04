# calculateVisualAngle() converts a pixel gaze coordinate to an angle (in
# degrees) subtended at the eye, via
#   Rad = atan2((gaze_px - (displayResolution_px + 1) * 0.5) * displayDimension_mm,
#               distanceZ * displayResolution_px)
#   Ang = Rad * (180 / pi)

test_that("calculateVisualAngle returns 0 for a gaze point at the exact screen center", {
  # center pixel for a 1920px-wide display is (1920+1)*0.5 = 960.5
  result <- calculateVisualAngle(
    gaze_px = 960.5,
    distanceZ = 600,
    displayResolution_px = 1920,
    displayDimension_mm = 600
  )

  expect_equal(result, 0)
})

test_that("calculateVisualAngle matches a hand-derived off-center value", {
  # Rad = atan2((1400 - (1920+1)*0.5) * 594, 600 * 1920)
  #     = atan2(439.5 * 594, 1152000) = atan2(261123, 1152000)
  # Ang = Rad * 180/pi
  result <- calculateVisualAngle(
    gaze_px = 1400,
    distanceZ = 600,
    displayResolution_px = 1920,
    displayDimension_mm = 594
  )

  expect_equal(round(result, 2), 12.77)
})

test_that("calculateVisualAngle returns a negative angle for a gaze point left/above center", {
  # Rad = atan2((0 - (1000+1)*0.5) * 300, 500 * 1000)
  #     = atan2(-500.5 * 300, 500000) = atan2(-150150, 500000)
  result <- calculateVisualAngle(
    gaze_px = 0,
    distanceZ = 500,
    displayResolution_px = 1000,
    displayDimension_mm = 300
  )

  expect_equal(round(result, 4), -16.715)
})
