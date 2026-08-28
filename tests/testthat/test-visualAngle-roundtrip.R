# calculateVisualAngle() (pixel -> visual angle) and convertVisualAngToPixels()
# (visual angle -> pixel) are meant to be exact inverses of one another: for
# any gaze_px, convertVisualAngToPixels(calculateVisualAngle(gaze_px, ...), ...)
# should return gaze_px, modulo floating-point rounding.
#
# Before this fix, calculateVisualAngle() used a screen-center pixel of
# (displayResolution_px + 1) * 0.5 while convertVisualAngToPixels() used
# displayResolution_px / 2 -- a half-pixel mismatch that broke the round trip
# by a constant 0.5px offset regardless of gaze position. Both functions now
# share the displayResolution_px / 2 center convention (see
# calculateVisualAngle()'s roxygen "Screen-center pixel convention" section
# for the full trace of why that's the convention this package's pixel data
# actually follows), so the round trip below should hold to within ordinary
# floating-point tolerance.

test_that("calculateVisualAngle and convertVisualAngToPixels round-trip for representative X-axis pixel positions", {
  distanceZ <- 600
  displayResolution_px <- 1920
  displayDimension_mm <- 594

  # center, left edge, right edge, and two off-center positions
  positions <- c(960, 0, 1920, 250, 1700)

  for (px in positions) {
    va <- calculateVisualAngle(px, distanceZ, displayResolution_px, displayDimension_mm)
    roundtrip_px <- convertVisualAngToPixels(va, distanceZ, displayResolution_px, displayDimension_mm)
    expect_equal(roundtrip_px, px, tolerance = 1e-9)
  }
})

test_that("calculateVisualAngle and convertVisualAngToPixels round-trip for representative Y-axis pixel positions", {
  distanceZ <- 550
  displayResolution_px <- 1080
  displayDimension_mm <- 340

  # center, top edge, bottom edge, and two off-center positions
  positions <- c(540, 0, 1080, 100, 950)

  for (px in positions) {
    va <- calculateVisualAngle(px, distanceZ, displayResolution_px, displayDimension_mm)
    roundtrip_px <- convertVisualAngToPixels(va, distanceZ, displayResolution_px, displayDimension_mm)
    expect_equal(roundtrip_px, px, tolerance = 1e-9)
  }
})

test_that("round trip holds without the un-rounded intermediate visual angle", {
  # calculateGaze_va() and removeOffscreenGaze() both round the intermediate
  # visual angle to 2 decimal places before using it further (see
  # R/calculateGaze_va.R and R/removeOffscreenGaze.R). Confirm the round trip
  # is still reasonably tight (sub-pixel) even through that rounding, since
  # that's the actual call pattern used by the pipeline.
  distanceZ <- 600
  displayResolution_px <- 1920
  displayDimension_mm <- 594
  positions <- c(960, 0, 1920, 250, 1700)

  for (px in positions) {
    va <- round(calculateVisualAngle(px, distanceZ, displayResolution_px, displayDimension_mm), 2)
    roundtrip_px <- convertVisualAngToPixels(va, distanceZ, displayResolution_px, displayDimension_mm)
    expect_equal(roundtrip_px, px, tolerance = 1)
  }
})
