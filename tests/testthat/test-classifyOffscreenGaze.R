# classifyOffscreenGaze() re-derives each eye's offscreen status from the
# interpolated gaze columns (gazeLeftX.int/Y.int, gazeRightX.int/Y.int) via
# detectOffscreenGaze(), recodes it against a pre-existing gazeLeft.offscreen/
# gazeRight.offscreen marker (as would be set upstream by
# removeOffscreenGaze() -- "offscreen.exclusionary" for points already
# excluded, or an initial placeholder otherwise), and finally derives
# offscreen.classification from the chosen eyeSelection_method.
#
# All expected values below were confirmed by direct execution against this
# 4-row fixture (100x100px display):
#   row 1: onscreen for both eyes
#   row 2: offscreen for both eyes, not previously exclusionary -> recoded
#          "offscreen.withinRange"
#   row 3: onscreen for both eyes, but pre-marked "offscreen.exclusionary" --
#          the onscreen recode overrides the prior exclusionary marker
#   row 4: offscreen for both eyes, pre-marked "offscreen.exclusionary" --
#          stays "offscreen.exclusionary" since it's still offscreen

fixture <- function() {
  data.frame(
    gazeLeftX.int = c(50, 150, 50, 150),
    gazeLeftY.int = c(50, 50, 50, 50),
    gazeRightX.int = c(52, 152, 52, 152),
    gazeRightY.int = c(51, 51, 51, 51),
    gazeX.eyeSelect = c(51, 151, 51, 151),
    gazeY.eyeSelect = c(50.5, 50.5, 50.5, 50.5),
    gazeLeft.offscreen = c("", "", "offscreen.exclusionary", "offscreen.exclusionary"),
    gazeRight.offscreen = c("", "", "offscreen.exclusionary", "offscreen.exclusionary")
  )
}

test_that("classifyOffscreenGaze (Maximize) classifies onscreen/withinRange/exclusionary rows correctly", {
  result <- classifyOffscreenGaze(
    fixture(),
    displayResolutionX_px = 100,
    displayResolutionY_px = 100,
    eyeSelection_method = "Maximize"
  )

  expect_equal(
    result$gazeLeft.offscreen,
    c("onscreen", "offscreen.withinRange", "onscreen", "offscreen.exclusionary")
  )
  expect_equal(
    result$offscreen.classification,
    c("onscreen", "offscreen.withinRange", "onscreen", "offscreen.exclusionary")
  )
})

test_that("classifyOffscreenGaze (Left) uses only the left eye's offscreen classification", {
  data <- fixture()
  # give left and right eyes different in/out-of-range status on row 2 to
  # confirm Left ignores the right eye entirely
  data$gazeLeftX.int[2] <- 50 # left onscreen
  data$gazeRightX.int[2] <- 150 # right offscreen

  result <- classifyOffscreenGaze(
    data,
    displayResolutionX_px = 100,
    displayResolutionY_px = 100,
    eyeSelection_method = "Left"
  )

  expect_equal(result$offscreen.classification, result$gazeLeft.offscreen)
  expect_equal(result$offscreen.classification[2], "onscreen")
})

test_that("classifyOffscreenGaze (Right) uses only the right eye's offscreen classification", {
  data <- fixture()
  data$gazeLeftX.int[2] <- 50 # left onscreen
  data$gazeRightX.int[2] <- 150 # right offscreen

  result <- classifyOffscreenGaze(
    data,
    displayResolutionX_px = 100,
    displayResolutionY_px = 100,
    eyeSelection_method = "Right"
  )

  expect_equal(result$offscreen.classification, result$gazeRight.offscreen)
  expect_equal(result$offscreen.classification[2], "offscreen.withinRange")
})
