# P3-06 follow-up: regression coverage for the new adapter normalize_validity()
# implementations (.tobii_studio_norm_validity() in R/tobii-studio-adapter.R,
# .tobii_pro_normalize_validity() in R/tobii-pro-adapter.R), which were ported
# unchanged from the pre-Phase-3 removeInvalidGaze() TobiiStudio/TobiiPro
# branches (R/removeInvalidGaze.R), plus the brand-new confidenceLeft/
# confidenceRight (0-1) columns that have no pre-Phase-3 equivalent to compare
# against. Both the old path (removeInvalidGaze(), called once per eye) and
# the new path (registered_adapters()[["TobiiStudio"/"TobiiPro"]]$
# normalize_validity(), called once for both eyes) are still live in the
# codebase as of P3-06 -- the actual eyeQuality() pipeline still calls
# removeInvalidGaze() directly and isn't rewired to the adapter registry
# until P3-07.
#
# The two paths return different shapes by design: the old path only adds
# .valid-suffixed columns, while the new path adds .valid-suffixed columns
# *plus* confidenceLeft/confidenceRight. So the equivalence tests below
# compare only the .valid-suffixed columns between the two paths, and the
# confidence columns are tested separately against known/derived values
# since there is no "old" to compare them against.
#
# NOTE: this test file is deliberately temporary-shaped, mirroring
# test-adapterStandardize.R and test-adapterExtractEvents.R. Once P3-07
# removes the old removeInvalidGaze() code path, this old-vs-new comparison
# will no longer make sense -- at that point these tests should be
# simplified to assert the adapter normalize_validity() output against known
# expected values directly, rather than against the (removed) old path.

# --- Old-vs-new .valid-column equivalence ---------------------------------

test_that("TobiiStudio adapter normalize_validity() .valid columns match old removeInvalidGaze() path on the sample fixture (binocular-valid)", {
  fp <- testthat::test_path("fixtures", "tobii_studio_sample.tsv")
  data <- importData(fp)
  std_data <- standardizeColumnNames(data, "TobiiStudio")

  old_result <- removeInvalidGaze(std_data, "left", "TobiiStudio")
  old_result <- removeInvalidGaze(old_result, "right", "TobiiStudio")
  new_result <- registered_adapters()[["TobiiStudio"]]$normalize_validity(std_data)

  valid_cols <- names(old_result)[grepl("\\.valid$", names(old_result))]
  expect_true(length(valid_cols) > 0)
  expect_equal(old_result[valid_cols], new_result[valid_cols])
})

test_that("TobiiStudio adapter normalize_validity() .valid columns match old removeInvalidGaze() path on the monocular fixture (real masking)", {
  fp <- testthat::test_path("fixtures", "tobii_studio_monocular.tsv")
  data <- importData(fp)
  std_data <- standardizeColumnNames(data, "TobiiStudio")

  old_result <- removeInvalidGaze(std_data, "left", "TobiiStudio")
  old_result <- removeInvalidGaze(old_result, "right", "TobiiStudio")
  new_result <- registered_adapters()[["TobiiStudio"]]$normalize_validity(std_data)

  valid_cols <- names(old_result)[grepl("\\.valid$", names(old_result))]
  expect_true(length(valid_cols) > 0)
  expect_equal(old_result[valid_cols], new_result[valid_cols])
})

test_that("TobiiPro adapter normalize_validity() .valid columns match old removeInvalidGaze() path on the sample fixture (binocular-valid)", {
  fp <- testthat::test_path("fixtures", "tobii_pro_sample.tsv")
  data <- importData(fp)
  std_data <- standardizeColumnNames(data, "TobiiPro")

  old_result <- removeInvalidGaze(std_data, "left", "TobiiPro")
  old_result <- removeInvalidGaze(old_result, "right", "TobiiPro")
  new_result <- registered_adapters()[["TobiiPro"]]$normalize_validity(std_data)

  valid_cols <- names(old_result)[grepl("\\.valid$", names(old_result))]
  expect_true(length(valid_cols) > 0)
  expect_equal(old_result[valid_cols], new_result[valid_cols])
})

test_that("TobiiPro adapter normalize_validity() .valid columns match old removeInvalidGaze() path on the monocular fixture (real masking)", {
  fp <- testthat::test_path("fixtures", "tobii_pro_monocular.tsv")
  data <- importData(fp)
  std_data <- standardizeColumnNames(data, "TobiiPro")

  old_result <- removeInvalidGaze(std_data, "left", "TobiiPro")
  old_result <- removeInvalidGaze(old_result, "right", "TobiiPro")
  new_result <- registered_adapters()[["TobiiPro"]]$normalize_validity(std_data)

  valid_cols <- names(old_result)[grepl("\\.valid$", names(old_result))]
  expect_true(length(valid_cols) > 0)
  expect_equal(old_result[valid_cols], new_result[valid_cols])
})

# --- confidenceLeft/confidenceRight correctness ----------------------------
# Brand new columns with no "old" path to compare against, so these are
# tested against known/derived values instead.

test_that("TobiiStudio adapter normalize_validity() sets confidence to 1 for both eyes when validity is best (0) on all-valid fixture", {
  fp <- testthat::test_path("fixtures", "tobii_studio_sample.tsv")
  data <- importData(fp)
  std_data <- standardizeColumnNames(data, "TobiiStudio")

  result <- tobii_studio_adapter$normalize_validity(std_data)

  expect_true(all(result$confidenceLeft == 1))
  expect_true(all(result$confidenceRight == 1))
})

test_that("TobiiStudio adapter normalize_validity() derives confidence from the native 0-4 validity scale on the monocular fixture", {
  fp <- testthat::test_path("fixtures", "tobii_studio_monocular.tsv")
  data <- importData(fp)
  std_data <- standardizeColumnNames(data, "TobiiStudio")

  result <- tobii_studio_adapter$normalize_validity(std_data)

  # fixture is uniformly validityLeft = 0 (best) / validityRight = 4 (worst)
  expect_true(all(result$confidenceLeft == 1))
  expect_true(all(result$confidenceRight == 0))
})

test_that("TobiiPro adapter normalize_validity() sets confidence to 1 for both eyes when validity is 'Valid' on all-valid fixture", {
  fp <- testthat::test_path("fixtures", "tobii_pro_sample.tsv")
  data <- importData(fp)
  std_data <- standardizeColumnNames(data, "TobiiPro")

  result <- tobii_pro_adapter$normalize_validity(std_data)

  expect_true(all(result$confidenceLeft == 1))
  expect_true(all(result$confidenceRight == 1))
})

test_that("TobiiPro adapter normalize_validity() derives confidence from the native Valid/Invalid strings on the monocular fixture", {
  fp <- testthat::test_path("fixtures", "tobii_pro_monocular.tsv")
  data <- importData(fp)
  std_data <- standardizeColumnNames(data, "TobiiPro")

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
  std_data <- standardizeColumnNames(data, "TobiiStudio")

  # fixture is uniformly validityRight = 4
  default_result <- tobii_studio_adapter$normalize_validity(std_data)
  loose_result <- tobii_studio_adapter$normalize_validity(std_data, threshold = 4)

  # default threshold (2): validity 4 > 2, so gazeRightX.valid is fully masked
  expect_true(all(is.na(default_result$gazeRightX.valid)))
  # threshold = 4: validity 4 is not > 4, so gazeRightX.valid is unmasked
  expect_true(all(!is.na(loose_result$gazeRightX.valid)))
  expect_equal(loose_result$gazeRightX.valid, std_data$gazeRightX)
})

test_that("TobiiPro adapter normalize_validity() threshold argument is accepted but has no effect on the masking outcome", {
  fp <- testthat::test_path("fixtures", "tobii_pro_monocular.tsv")
  data <- importData(fp)
  std_data <- standardizeColumnNames(data, "TobiiPro")

  default_result <- tobii_pro_adapter$normalize_validity(std_data)
  explicit_result <- tobii_pro_adapter$normalize_validity(std_data, threshold = 0.5)

  valid_cols <- names(default_result)[grepl("\\.valid$", names(default_result))]
  expect_equal(default_result[valid_cols], explicit_result[valid_cols])
})
