# Regression coverage for the adapter normalize_validity() implementations
# (.tobii_studio_norm_validity() in R/tobii-studio-adapter.R,
# .tobii_pro_normalize_validity() in R/tobii-pro-adapter.R), which mask
# invalid/out-of-threshold samples into new `.valid`-suffixed columns and
# compute the new confidenceLeft/confidenceRight (0-1) columns
# (?eyeQuality-schema).
#
# Originally (P3-06) the .valid-column masking tests below asserted the
# adapter output was equivalent to the old standalone removeInvalidGaze()
# path (called once per eye), which was still live in the eyeQuality()
# pipeline at the time. P3-07 rewired the pipeline to call the adapter
# registry directly, retiring that old path from live use, so those tests
# now assert masking against literal expected values instead of re-deriving
# them by calling the now-pipeline-dead removeInvalidGaze(). Fixture setup
# below uses each adapter's own (live) standardize() method rather than the
# old standardizeColumnNames() dispatcher.
#
# The confidenceLeft/confidenceRight correctness tests never depended on the
# old removeInvalidGaze() path (there is no pre-Phase-3 equivalent to compare
# against) and are carried forward largely unchanged, aside from the same
# standardize() setup swap.

# --- .valid-column masking on realistic fixtures ---------------------------
# The "sample" fixtures are uniformly valid (TobiiStudio: validityLeft/Right
# = 0; TobiiPro: validityLeft/Right = "Valid") and contain no -9999 sentinel
# values, so normalize_validity() should leave every `.valid` column
# byte-identical to its raw source column. The "monocular" fixtures are
# uniformly worst-validity on one eye only (TobiiStudio: validityRight = 4;
# TobiiPro: validityRight = "Invalid"), so the right-eye `.valid` columns
# should come back fully masked while the left-eye ones are untouched.

test_that("TobiiStudio adapter normalize_validity() leaves .valid columns unmasked on the all-valid sample fixture", {
  fp <- testthat::test_path("fixtures", "tobii_studio_sample.tsv")
  data <- importData(fp)
  std_data <- tobii_studio_adapter$standardize(data)

  result <- tobii_studio_adapter$normalize_validity(std_data)

  expect_equal(result$gazeLeftX.valid, std_data$gazeLeftX)
  expect_equal(result$gazeRightX.valid, std_data$gazeRightX)
  expect_equal(result$pupilLeft.valid, std_data$pupilLeft)
  expect_equal(result$pupilRight.valid, std_data$pupilRight)
})

test_that("TobiiStudio adapter normalize_validity() fully masks the right eye and leaves the left eye untouched on the monocular fixture", {
  fp <- testthat::test_path("fixtures", "tobii_studio_monocular.tsv")
  data <- importData(fp)
  std_data <- tobii_studio_adapter$standardize(data)

  result <- tobii_studio_adapter$normalize_validity(std_data)

  # fixture is uniformly validityLeft = 0 (best, <= default threshold 2)
  expect_equal(result$gazeLeftX.valid, std_data$gazeLeftX)
  expect_equal(result$gazeLeftY.valid, std_data$gazeLeftY)
  # fixture is uniformly validityRight = 4 (worst, > default threshold 2)
  expect_true(all(is.na(result$gazeRightX.valid)))
  expect_true(all(is.na(result$gazeRightY.valid)))
})

test_that("TobiiPro adapter normalize_validity() leaves .valid columns unmasked on the all-valid sample fixture", {
  fp <- testthat::test_path("fixtures", "tobii_pro_sample.tsv")
  data <- importData(fp)
  std_data <- tobii_pro_adapter$standardize(data)

  result <- tobii_pro_adapter$normalize_validity(std_data)

  expect_equal(result$gazeLeftX.valid, std_data$gazeLeftX)
  expect_equal(result$gazeRightX.valid, std_data$gazeRightX)
  expect_equal(result$pupilLeft.valid, std_data$pupilLeft)
  expect_equal(result$pupilRight.valid, std_data$pupilRight)
})

test_that("TobiiPro adapter normalize_validity() fully masks the right eye and leaves the left eye untouched on the monocular fixture", {
  fp <- testthat::test_path("fixtures", "tobii_pro_monocular.tsv")
  data <- importData(fp)
  std_data <- tobii_pro_adapter$standardize(data)

  result <- tobii_pro_adapter$normalize_validity(std_data)

  # fixture is uniformly validityLeft = "Valid"
  expect_equal(result$gazeLeftX.valid, std_data$gazeLeftX)
  expect_equal(result$gazeLeftY.valid, std_data$gazeLeftY)
  # fixture is uniformly validityRight = "Invalid"
  expect_true(all(is.na(result$gazeRightX.valid)))
  expect_true(all(is.na(result$gazeRightY.valid)))
})

# --- confidenceLeft/confidenceRight correctness ----------------------------
# Brand new columns with no "old" path to compare against, so these are
# tested against known/derived values instead.

test_that("TobiiStudio adapter normalize_validity() sets confidence to 1 for both eyes when validity is best (0) on all-valid fixture", {
  fp <- testthat::test_path("fixtures", "tobii_studio_sample.tsv")
  data <- importData(fp)
  std_data <- tobii_studio_adapter$standardize(data)

  result <- tobii_studio_adapter$normalize_validity(std_data)

  expect_true(all(result$confidenceLeft == 1))
  expect_true(all(result$confidenceRight == 1))
})

test_that("TobiiStudio adapter normalize_validity() derives confidence from the native 0-4 validity scale on the monocular fixture", {
  fp <- testthat::test_path("fixtures", "tobii_studio_monocular.tsv")
  data <- importData(fp)
  std_data <- tobii_studio_adapter$standardize(data)

  result <- tobii_studio_adapter$normalize_validity(std_data)

  # fixture is uniformly validityLeft = 0 (best) / validityRight = 4 (worst)
  expect_true(all(result$confidenceLeft == 1))
  expect_true(all(result$confidenceRight == 0))
})

test_that("TobiiPro adapter normalize_validity() sets confidence to 1 for both eyes when validity is 'Valid' on all-valid fixture", {
  fp <- testthat::test_path("fixtures", "tobii_pro_sample.tsv")
  data <- importData(fp)
  std_data <- tobii_pro_adapter$standardize(data)

  result <- tobii_pro_adapter$normalize_validity(std_data)

  expect_true(all(result$confidenceLeft == 1))
  expect_true(all(result$confidenceRight == 1))
})

test_that("TobiiPro adapter normalize_validity() derives confidence from the native Valid/Invalid strings on the monocular fixture", {
  fp <- testthat::test_path("fixtures", "tobii_pro_monocular.tsv")
  data <- importData(fp)
  std_data <- tobii_pro_adapter$standardize(data)

  result <- tobii_pro_adapter$normalize_validity(std_data)

  # fixture is uniformly validityLeft = "Valid" / validityRight = "Invalid"
  expect_true(all(result$confidenceLeft == 1))
  expect_true(all(result$confidenceRight == 0))
})

# --- NA-validity synthetic cases: .valid masking + confidence ------------
# Adapted from test-removeInvalidGaze.R's synthetic NA-validity test cases.
# Unlike removeInvalidGaze() (called once per eye), the adapter
# normalize_validity() methods operate on both eyes in a single call, so
# these synthetic frames need both validityLeft and validityRight columns
# present (.tobii_*_confidence() reads both). This locks in the documented
# P3-06 decision that NA validity maps to confidence = 1 for both eyes,
# matching the pre-existing (and otherwise unrelated) quirk that NA validity
# is also left unmasked in the .valid columns.

test_that("TobiiPro adapter normalize_validity() sets confidence to 1 at NA-validity rows and masks Invalid rows in .valid columns", {
  data <- data.frame(
    gazeLeftX = c(1, 2, 3, 4),
    gazeLeftY = c(1, 2, 3, 4),
    gazeRightX = c(1, 2, 3, 4),
    gazeRightY = c(1, 2, 3, 4),
    validityLeft = c("Valid", "Invalid", "Valid", NA),
    validityRight = c("Valid", "Valid", "Invalid", NA),
    stringsAsFactors = FALSE
  )

  result <- tobii_pro_adapter$normalize_validity(data)

  expect_equal(result$gazeLeftX.valid, c(1, NA, 3, 4))
  expect_equal(result$gazeRightX.valid, c(1, 2, NA, 4))
  expect_equal(result$confidenceLeft, c(1, 0, 1, 1))
  expect_equal(result$confidenceRight, c(1, 1, 0, 1))
})

test_that("TobiiStudio adapter normalize_validity() sets confidence to 1 at NA-validity rows and masks out-of-threshold rows in .valid columns", {
  data <- data.frame(
    gazeLeftX = c(1, 2, 3, -9999),
    gazeLeftY = c(1, 2, 3, 4),
    gazeRightX = c(1, 2, 3, 4),
    gazeRightY = c(1, 2, 3, 4),
    validityLeft = c(0, 3, 1, NA),
    validityRight = c(0, 0, 4, NA),
    stringsAsFactors = FALSE
  )

  result <- tobii_studio_adapter$normalize_validity(data)

  # gazeLeftX row 2 masked (validity 3 > threshold 2); row 4 masked via the
  # -9999 sentinel regardless of its NA validity
  expect_equal(result$gazeLeftX.valid, c(1, NA, 3, NA))
  # gazeLeftY row 4 is NOT -9999, so NA validity leaves it unmasked
  expect_equal(result$gazeLeftY.valid, c(1, NA, 3, 4))
  # gazeRightY row 3 masked (validity 4 > threshold 2); row 4 NA validity
  # unmasked
  expect_equal(result$gazeRightY.valid, c(1, 2, NA, 4))
  expect_equal(result$confidenceLeft, c(1, 0.25, 0.75, 1))
  expect_equal(result$confidenceRight, c(1, 1, 0, 1))
})

# --- threshold parameter ----------------------------------------------------
# TobiiStudio: threshold is actually wired -- .tobii_studio_norm_validity()
# falls back to default_thresholds$validityThreshold (2) when NULL, and an
# explicit non-default value is passed straight through to the masking
# comparison (validityVals > threshold). Confirm an explicit threshold
# actually changes the masking outcome relative to the default.
#
# TobiiPro: threshold is accepted for interface-signature compatibility but
# NOT wired to anything -- .tobii_pro_normalize_validity()'s threshold
# argument is never read in its body (Tobii Pro's validity is the binary
# "Valid"/"Invalid" string with no numeric threshold concept, matching the
# pre-Phase-3 removeInvalidGaze() TobiiPro branch, which also ignored its
# `threshold` argument). Since there's no threshold behavior to test, the
# test below instead documents that passing one has no effect on the
# output, which is the correct behavior for now.

test_that("TobiiStudio adapter normalize_validity() threshold argument changes the masking outcome relative to the default", {
  fp <- testthat::test_path("fixtures", "tobii_studio_monocular.tsv")
  data <- importData(fp)
  std_data <- tobii_studio_adapter$standardize(data)

  # fixture is uniformly validityRight = 4
  default_result <- tobii_studio_adapter$normalize_validity(std_data)
  loose_result <- tobii_studio_adapter$normalize_validity(std_data, threshold = 4)

  # default threshold (2): validity 4 > 2, so gazeRightX.valid is fully masked
  expect_true(all(is.na(default_result$gazeRightX.valid)))
  # threshold = 4: validity 4 is not > 4, so gazeRightX.valid is unmasked
  expect_true(all(!is.na(loose_result$gazeRightX.valid)))
  expect_equal(loose_result$gazeRightX.valid, std_data$gazeRightX)
})

# --- P3-10 verbose diagnostics ----------------------------------------------
# normalize_validity()'s verbose behavior (via .tobii_*_mask_eye()): a
# consecutive-run diagnostic for masked/below-threshold samples per eye, plus
# a separate note about the documented NA-validity-not-masked quirk when NA
# validity is actually present for that eye.

test_that("TobiiStudio adapter normalize_validity(verbose = TRUE) reports a long consecutive run of below-threshold validity on the monocular fixture", {
  fp <- testthat::test_path("fixtures", "tobii_studio_monocular.tsv")
  data <- importData(fp)
  std_data <- tobii_studio_adapter$standardize(data)

  out <- capture.output(tobii_studio_adapter$normalize_validity(std_data, verbose = TRUE))

  expect_true(any(grepl(
    "200 sample\\(s\\) flagged for: right eye validity below threshold \\(2\\)", out
  )))
  expect_true(any(grepl(
    "rows 1-200: 200 consecutive samples flagged for: right eye validity below threshold \\(2\\)", out
  )))
  # left eye is uniformly valid (validity 0) -- nothing should be flagged there
  expect_false(any(grepl("left eye validity below threshold", out)))
})

test_that("TobiiStudio adapter normalize_validity(verbose = TRUE) notes the NA-validity-not-masked quirk only when NA validity is actually present", {
  data <- data.frame(
    gazeLeftX = c(1, 2, 3, 4),
    gazeLeftY = c(1, 2, 3, 4),
    gazeRightX = c(1, 2, 3, 4),
    gazeRightY = c(1, 2, 3, 4),
    validityLeft = c(0, 0, 0, NA),
    validityRight = c(0, 0, 0, 0)
  )

  out <- capture.output(tobii_studio_adapter$normalize_validity(data, verbose = TRUE))

  expect_true(any(grepl(
    "1 sample\\(s\\) have NA validity for the left eye -- these are NOT masked", out
  )))
  expect_false(any(grepl("NA validity for the right eye", out)))
})

test_that("TobiiStudio adapter normalize_validity(verbose = FALSE) (default) emits no diagnostics even on the monocular fixture", {
  fp <- testthat::test_path("fixtures", "tobii_studio_monocular.tsv")
  data <- importData(fp)
  std_data <- tobii_studio_adapter$standardize(data)

  expect_silent(tobii_studio_adapter$normalize_validity(std_data))
})

test_that("TobiiPro adapter normalize_validity(verbose = TRUE) reports a long consecutive run of 'Invalid' validity on the monocular fixture", {
  fp <- testthat::test_path("fixtures", "tobii_pro_monocular.tsv")
  data <- importData(fp)
  std_data <- tobii_pro_adapter$standardize(data)

  out <- capture.output(tobii_pro_adapter$normalize_validity(std_data, verbose = TRUE))

  # Note: the source label text embeds literal double quotes around
  # "Invalid" (`right eye validity == "Invalid"`), which print()'s own
  # display quoting re-escapes with a literal backslash in the captured
  # console text -- so these patterns intentionally stop short of the quoted
  # word itself rather than trying to match that print()-escaping exactly.
  expect_true(any(grepl(
    "200 sample\\(s\\) flagged for: right eye validity ==", out
  )))
  expect_true(any(grepl(
    "rows 1-200: 200 consecutive samples flagged for: right eye validity ==", out
  )))
  expect_false(any(grepl("left eye validity ==", out)))
})

test_that("TobiiPro adapter normalize_validity(verbose = TRUE) notes the NA-validity-not-masked quirk only when NA validity is actually present", {
  data <- data.frame(
    gazeLeftX = c(1, 2, 3, 4),
    gazeLeftY = c(1, 2, 3, 4),
    gazeRightX = c(1, 2, 3, 4),
    gazeRightY = c(1, 2, 3, 4),
    validityLeft = c("Valid", "Valid", "Valid", NA),
    validityRight = c("Valid", "Valid", "Valid", "Valid"),
    stringsAsFactors = FALSE
  )

  out <- capture.output(tobii_pro_adapter$normalize_validity(data, verbose = TRUE))

  expect_true(any(grepl(
    "1 sample\\(s\\) have NA validity for the left eye -- these are NOT masked", out
  )))
  expect_false(any(grepl("NA validity for the right eye", out)))
})

test_that("TobiiPro adapter normalize_validity(verbose = FALSE) (default) emits no diagnostics even on the monocular fixture", {
  fp <- testthat::test_path("fixtures", "tobii_pro_monocular.tsv")
  data <- importData(fp)
  std_data <- tobii_pro_adapter$standardize(data)

  expect_silent(tobii_pro_adapter$normalize_validity(std_data))
})

test_that("TobiiPro adapter normalize_validity() threshold argument is accepted but has no effect on the masking outcome", {
  fp <- testthat::test_path("fixtures", "tobii_pro_monocular.tsv")
  data <- importData(fp)
  std_data <- tobii_pro_adapter$standardize(data)

  default_result <- tobii_pro_adapter$normalize_validity(std_data)
  explicit_result <- tobii_pro_adapter$normalize_validity(std_data, threshold = 0.5)

  valid_cols <- names(default_result)[grepl("\\.valid$", names(default_result))]
  expect_equal(default_result[valid_cols], explicit_result[valid_cols])
})
