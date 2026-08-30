# eyeSelection() combines left/right eye gaze, pupil, and distance streams
# into a single "eyeSelect" stream according to one of four strategies. All
# expected values below were hand-derived from the function's documented
# per-method logic and confirmed by direct execution.
#
# Shared 3-row fixture used across all four methods:
#   row 1: both eyes present (left != right, to catch an accidental
#           left/right swap)
#   row 2: left eye missing, right eye present
#   row 3: both eyes missing

testData <- function() {
  data.frame(
    gazeLeftX.int = c(10, NA, NA),
    gazeLeftY.int = c(20, NA, NA),
    gazeRightX.int = c(12, 15, NA),
    gazeRightY.int = c(22, 25, NA),
    pupilLeft.int = c(3, NA, NA),
    pupilRight.int = c(3.4, 3.2, NA),
    distanceLeftZ.int = c(600, NA, NA),
    distanceRightZ.int = c(602, 605, NA)
  )
}

test_that("eyeSelection Maximize averages both eyes when present and falls back to the available eye otherwise", {
  result <- eyeSelection(testData(), eyeSelection_method = "Maximize")

  expect_equal(result$gazeX.eyeSelect, c(11, 15, NaN))
  expect_equal(result$gazeY.eyeSelect, c(21, 25, NaN))
  expect_equal(result$pupil.eyeSelect, c(3.2, 3.2, NaN))
  expect_equal(result$distanceZ.eyeSelect, c(601, 605, NaN))
  expect_equal(
    result$gazeX.es.selection,
    c("mean_maximized", "right_only", "both_na")
  )
})

test_that("eyeSelection Strict returns NA for a row wherever either eye is missing", {
  result <- eyeSelection(testData(), eyeSelection_method = "Strict")

  expect_equal(result$gazeX.eyeSelect, c(11, NA, NA))
  expect_equal(result$gazeY.eyeSelect, c(21, NA, NA))
  expect_equal(result$pupil.eyeSelect, c(3.2, NA, NA))
  expect_equal(result$distanceZ.eyeSelect, c(601, NA, NA))
  expect_equal(
    result$gazeX.es.selection,
    c("mean_strict", "left_na", "both_na")
  )
})

test_that("eyeSelection Left always uses the left eye stream, regardless of the right eye's data", {
  result <- eyeSelection(testData(), eyeSelection_method = "Left")

  expect_equal(result$gazeX.eyeSelect, c(10, NA, NA))
  expect_equal(result$gazeY.eyeSelect, c(20, NA, NA))
  expect_equal(result$pupil.eyeSelect, c(3, NA, NA))
  expect_equal(result$distanceZ.eyeSelect, c(600, NA, NA))
  expect_equal(
    result$gazeX.es.selection,
    c("left_only", "left_na", "left_na")
  )
})

test_that("eyeSelection Right always uses the right eye stream, regardless of the left eye's data", {
  result <- eyeSelection(testData(), eyeSelection_method = "Right")

  expect_equal(result$gazeX.eyeSelect, c(12, 15, NA))
  expect_equal(result$gazeY.eyeSelect, c(22, 25, NA))
  expect_equal(result$pupil.eyeSelect, c(3.4, 3.2, NA))
  expect_equal(result$distanceZ.eyeSelect, c(602, 605, NA))
  expect_equal(
    result$gazeX.es.selection,
    c("right_only", "right_only", "right_na")
  )
})

# --- P3-10 verbose diagnostics ----------------------------------------------
# A 10-row fixture with a 7-row run of both-eyes-missing data (long enough to
# clear .diagnose_consecutive_runs()'s default min_run_length = 5), used to
# confirm eyeSelection() actually reports the consecutive-missing-data run
# for each eyeSelection_method's own "no usable gaze" condition.

longMissingData <- function() {
  data.frame(
    gazeLeftX.int = c(1:3, rep(NA, 7)),
    gazeLeftY.int = c(1:3, rep(NA, 7)),
    gazeRightX.int = c(1:3, rep(NA, 7)),
    gazeRightY.int = c(1:3, rep(NA, 7)),
    pupilLeft.int = c(1:3, rep(NA, 7)),
    pupilRight.int = c(1:3, rep(NA, 7)),
    distanceLeftZ.int = c(1:3, rep(NA, 7)),
    distanceRightZ.int = c(1:3, rep(NA, 7))
  )
}

test_that("eyeSelection Maximize (verbose = TRUE) reports the consecutive run of both-eyes-missing samples", {
  out <- capture.output(eyeSelection(longMissingData(), eyeSelection_method = "Maximize", verbose = TRUE))

  expect_true(any(grepl(
    "rows 4-10: 7 consecutive samples flagged for: gaze data missing in both eyes after eye selection \\(Maximize\\)",
    out
  )))
})

test_that("eyeSelection Strict (verbose = TRUE) reports the consecutive run of both-eyes-missing samples", {
  out <- capture.output(eyeSelection(longMissingData(), eyeSelection_method = "Strict", verbose = TRUE))

  expect_true(any(grepl(
    "rows 4-10: 7 consecutive samples flagged for: gaze data missing in both eyes after eye selection \\(Strict\\)",
    out
  )))
})

test_that("eyeSelection Left (verbose = TRUE) reports the consecutive run of missing left-eye samples", {
  out <- capture.output(eyeSelection(longMissingData(), eyeSelection_method = "Left", verbose = TRUE))

  # Note: the source label text embeds literal double quotes around "Left"
  # (`eyeSelection_method = "Left"`), which print()'s own display quoting
  # re-escapes with a literal backslash in the captured console text -- so
  # this pattern intentionally stops short of the quoted word itself rather
  # than trying to match that print()-escaping exactly.
  expect_true(any(grepl(
    "rows 4-10: 7 consecutive samples flagged for: left eye gaze missing \\(eyeSelection_method =",
    out
  )))
})

test_that("eyeSelection Right (verbose = TRUE) reports the consecutive run of missing right-eye samples", {
  out <- capture.output(eyeSelection(longMissingData(), eyeSelection_method = "Right", verbose = TRUE))

  expect_true(any(grepl(
    "rows 4-10: 7 consecutive samples flagged for: right eye gaze missing \\(eyeSelection_method =",
    out
  )))
})

test_that("eyeSelection(verbose = FALSE) (default) emits no [verbose]-tagged diagnostic lines", {
  # eyeSelection() has pre-existing, non-verbose-gated progress print() calls
  # (e.g. "selecting eye based on maximized approach"), so this checks
  # specifically for the absence of the P3-10 "[verbose]" tag rather than
  # full silence.
  out <- capture.output(eyeSelection(longMissingData(), eyeSelection_method = "Maximize"))

  expect_false(any(grepl("\\[verbose\\]", out)))
})
