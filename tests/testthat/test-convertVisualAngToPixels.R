# convertVisualAngToPixels() converts a visual angle (degrees) back to a
# pixel gaze coordinate, via
#   px = tan(deg2rad(gazeVA)) * distanceZ * displayResolution_px / displayDimension_mm
#        + displayResolution_px / 2
#
# The screen-center pixel is displayResolution_px / 2 -- the same convention
# calculateVisualAngle() uses (see its roxygen "Screen-center pixel
# convention" section and this function's own roxygen section for the full
# trace: gaze_px values in this package derive from Tobii's ADCS normalized
# [0,1] coordinates multiplied by displayResolution_px, a continuous
# edge-to-edge span rather than a discrete 0-indexed pixel grid). Keeping
# both functions on the same center formula is what makes them exact
# algebraic inverses of one another -- see test-visualAngleRoundTrip.R for
# the round-trip proof.

test_that("convertVisualAngToPixels returns the center pixel for 0 degrees", {
  result <- convertVisualAngToPixels(
    gazeVA = 0,
    distanceZ = 600,
    displayResolution_px = 1920,
    displayDimension_mm = 600
  )

  expect_equal(result, 960)
})

test_that("convertVisualAngToPixels matches a hand-derived off-center value", {
  # px = tan(10 * pi/180) * 600 * 1920 / 594 + 1920/2
  result <- convertVisualAngToPixels(
    gazeVA = 10,
    distanceZ = 600,
    displayResolution_px = 1920,
    displayDimension_mm = 594
  )

  expect_equal(round(result, 2), 1301.97)
})

test_that("convertVisualAngToPixels returns a pixel left/above center for a negative angle", {
  # px = tan(-5 * pi/180) * 500 * 1000 / 300 + 1000/2
  result <- convertVisualAngToPixels(
    gazeVA = -5,
    distanceZ = 500,
    displayResolution_px = 1000,
    displayDimension_mm = 300
  )

  expect_equal(round(result, 4), 354.1856)
})
