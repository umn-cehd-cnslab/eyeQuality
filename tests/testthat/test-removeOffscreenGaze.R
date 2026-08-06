# removeOffscreenGaze() marks gazepoints outside the acceptable display
# bounds (display resolution + offscreenValidityRange_va of visual angle
# slack) as NA, and records a gazeLeft.offscreen/gazeRight.offscreen/
# gaze.offscreen label column. No dedicated unit test file existed for this
# function prior to P3-10; this file both fills that gap for the basic
# masking behavior and covers the new P3-10 verbose diagnostics
# (.diagnose_consecutive_runs() reporting offscreen runs before they're
# masked to NA).
#
# Fixture: 18 samples at a constant 600mm viewing distance, on a
# 1920x1080px / 594x344mm display. Rows 1-5 and 14-18 sit at gaze (500, 500)
# (comfortably onscreen); rows 6-13 (8 samples) sit at gaze (-5000, -5000),
# far outside the display bounds even with the default 5VA slack -- verified
# by direct execution against this fixture.

offscreenFixture <- function() {
  data.frame(
    gazeLeftX.valid = c(rep(500, 5), rep(-5000, 8), rep(500, 5)),
    gazeLeftY.valid = c(rep(500, 5), rep(-5000, 8), rep(500, 5)),
    distanceLeftZ.valid = rep(600, 18)
  )
}

test_that("removeOffscreenGaze masks gaze coordinates outside display bounds to NA and leaves onscreen rows untouched", {
  result <- removeOffscreenGaze(
    offscreenFixture(),
    gazeX = "gazeLeftX.valid",
    gazeY = "gazeLeftY.valid",
    distanceZ = "distanceLeftZ.valid",
    displayResolutionX_px = 1920,
    displayResolutionY_px = 1080,
    displayDimensionX_mm = 594,
    displayDimensionY_mm = 344
  )

  expect_true(all(is.na(result$gazeLeftX.valid[6:13])))
  expect_true(all(is.na(result$gazeLeftY.valid[6:13])))
  expect_true(all(!is.na(result$gazeLeftX.valid[c(1:5, 14:18)])))
  expect_equal(result$gazeLeftX.valid[c(1:5, 14:18)], rep(500, 10))
})

test_that("removeOffscreenGaze records gazeLeft.offscreen as 'offscreen.exclusionary' for masked rows and 'onscreen' otherwise", {
  result <- removeOffscreenGaze(
    offscreenFixture(),
    gazeX = "gazeLeftX.valid",
    gazeY = "gazeLeftY.valid",
    distanceZ = "distanceLeftZ.valid",
    displayResolutionX_px = 1920,
    displayResolutionY_px = 1080,
    displayDimensionX_mm = 594,
    displayDimensionY_mm = 344
  )

  expect_true(all(result$gazeLeft.offscreen[6:13] == "offscreen.exclusionary"))
  expect_true(all(result$gazeLeft.offscreen[c(1:5, 14:18)] == "onscreen"))
})

test_that("removeOffscreenGaze uses a gazeRight.offscreen column name when gazeX names the right eye", {
  data <- offscreenFixture()
  names(data) <- c("gazeRightX.valid", "gazeRightY.valid", "distanceRightZ.valid")

  result <- removeOffscreenGaze(
    data,
    gazeX = "gazeRightX.valid",
    gazeY = "gazeRightY.valid",
    distanceZ = "distanceRightZ.valid",
    displayResolutionX_px = 1920,
    displayResolutionY_px = 1080,
    displayDimensionX_mm = 594,
    displayDimensionY_mm = 344
  )

  expect_true("gazeRight.offscreen" %in% names(result))
  expect_false("gazeLeft.offscreen" %in% names(result))
})

# --- P3-10 verbose diagnostics ----------------------------------------------

test_that("removeOffscreenGaze(verbose = TRUE) reports the consecutive run of offscreen samples before masking", {
  out <- capture.output(
    removeOffscreenGaze(
      offscreenFixture(),
      gazeX = "gazeLeftX.valid",
      gazeY = "gazeLeftY.valid",
      distanceZ = "distanceLeftZ.valid",
      displayResolutionX_px = 1920,
      displayResolutionY_px = 1080,
      displayDimensionX_mm = 594,
      displayDimensionY_mm = 344,
      verbose = TRUE
    )
  )

  expect_true(any(grepl(
    "8 sample\\(s\\) flagged for: left eye gaze coordinates outside display bounds", out
  )))
  expect_true(any(grepl(
    "rows 6-13: 8 consecutive samples flagged for: left eye gaze coordinates outside display bounds", out
  )))
})

test_that("removeOffscreenGaze(verbose = TRUE) labels the diagnostic 'right eye' when gazeX names the right eye", {
  data <- offscreenFixture()
  names(data) <- c("gazeRightX.valid", "gazeRightY.valid", "distanceRightZ.valid")

  out <- capture.output(
    removeOffscreenGaze(
      data,
      gazeX = "gazeRightX.valid",
      gazeY = "gazeRightY.valid",
      distanceZ = "distanceRightZ.valid",
      displayResolutionX_px = 1920,
      displayResolutionY_px = 1080,
      displayDimensionX_mm = 594,
      displayDimensionY_mm = 344,
      verbose = TRUE
    )
  )

  expect_true(any(grepl("right eye gaze coordinates outside display bounds", out)))
  expect_false(any(grepl("left eye gaze coordinates", out)))
})

test_that("removeOffscreenGaze(verbose = FALSE) (default) emits no diagnostics", {
  expect_silent(
    removeOffscreenGaze(
      offscreenFixture(),
      gazeX = "gazeLeftX.valid",
      gazeY = "gazeLeftY.valid",
      distanceZ = "distanceLeftZ.valid",
      displayResolutionX_px = 1920,
      displayResolutionY_px = 1080,
      displayDimensionX_mm = 594,
      displayDimensionY_mm = 344
    )
  )
})
