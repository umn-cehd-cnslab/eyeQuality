# P3-05 follow-up: regression coverage for the new adapter extract_events()
# implementations (.tobii_studio_extract_events() in
# R/tobii-studio-adapter.R, .tobii_pro_extract_events() in
# R/tobii-pro-adapter.R), which were ported unchanged from the pre-Phase-3
# extractEventRows() TobiiStudio/TobiiPro branches (R/extractEventRows.R).
# Both the old path (standardizeColumnNames() -> extractEventRows()) and the
# new path (registered_adapters()[["TobiiStudio"/"TobiiPro"]]$extract_events())
# are still live in the codebase as of P3-05 -- the actual eyeQuality()
# pipeline still calls extractEventRows() directly and isn't rewired to the
# adapter registry until P3-07.
#
# The two paths return different shapes by design: the old extractEventRows()
# returns an unnamed positional list(gazeStreamData, eventData), while the
# new extract_events() returns a named list(gaze = ..., events = ...), to
# match the documented adapter interface contract (R/adapter-interface.R,
# R/eyeQuality-schema.R). So these tests compare the underlying data
# (old_result[[1]] vs new_result$gaze, etc.) rather than the raw lists --
# a naive identical() on the two lists would fail on the name difference
# alone, even though the actual split logic is unchanged.
#
# NOTE: this test file is deliberately temporary-shaped, mirroring
# test-adapterStandardize.R. Once P3-07 removes the old
# extractEventRows()/standardizeColumnNames() code path, this old-vs-new
# comparison will no longer make sense -- at that point these tests should be
# simplified to assert the adapter extract_events() output against known
# expected values directly, rather than against the (removed) old path.

test_that("TobiiStudio adapter extract_events() matches old extractEventRows() path on the sample fixture", {
  fp <- testthat::test_path("fixtures", "tobii_studio_sample.tsv")
  data <- importData(fp)
  std_data <- standardizeColumnNames(data, "TobiiStudio")

  old_result <- extractEventRows(std_data, software = "TobiiStudio")
  new_result <- registered_adapters()[["TobiiStudio"]]$extract_events(std_data)

  expect_equal(old_result[[1]], new_result$gaze)
  expect_equal(old_result[[2]], new_result$events)
})

test_that("TobiiStudio adapter extract_events() matches old extractEventRows() path on the monocular fixture", {
  fp <- testthat::test_path("fixtures", "tobii_studio_monocular.tsv")
  data <- importData(fp)
  std_data <- standardizeColumnNames(data, "TobiiStudio")

  old_result <- extractEventRows(std_data, software = "TobiiStudio")
  new_result <- registered_adapters()[["TobiiStudio"]]$extract_events(std_data)

  expect_equal(old_result[[1]], new_result$gaze)
  expect_equal(old_result[[2]], new_result$events)
})

test_that("TobiiPro adapter extract_events() matches old extractEventRows() path on the sample fixture", {
  fp <- testthat::test_path("fixtures", "tobii_pro_sample.tsv")
  data <- importData(fp)
  std_data <- standardizeColumnNames(data, "TobiiPro")

  old_result <- extractEventRows(std_data, software = "TobiiPro")
  new_result <- registered_adapters()[["TobiiPro"]]$extract_events(std_data)

  expect_equal(old_result[[1]], new_result$gaze)
  expect_equal(old_result[[2]], new_result$events)
})

test_that("TobiiPro adapter extract_events() matches old extractEventRows() path on the monocular fixture", {
  fp <- testthat::test_path("fixtures", "tobii_pro_monocular.tsv")
  data <- importData(fp)
  std_data <- standardizeColumnNames(data, "TobiiPro")

  old_result <- extractEventRows(std_data, software = "TobiiPro")
  new_result <- registered_adapters()[["TobiiPro"]]$extract_events(std_data)

  expect_equal(old_result[[1]], new_result$gaze)
  expect_equal(old_result[[2]], new_result$events)
})

# --- Error-path coverage --------------------------------------------------
# Adapter-world analog of P1-03's extractEventRows() regression test
# (test-extractEventRows.R's "errors immediately on an unrecognized software
# value" test). The new adapters have no `software` argument to dispatch on
# -- the registry already selects the right adapter -- so the equivalent
# failure mode is calling extract_events() on data that is missing the
# device-specific discriminator column it filters on. Both should fail
# loudly (naming the missing column) rather than silently returning garbage
# or an opaque downstream error.

test_that("TobiiStudio adapter extract_events() errors clearly when eyeTrackerTimestamp is missing", {
  data <- data.frame(someOtherColumn = 1:3)

  expect_error(
    tobii_studio_adapter$extract_events(data),
    regexp = "eyeTrackerTimestamp"
  )
})

test_that("TobiiPro adapter extract_events() errors clearly when Sensor is missing", {
  data <- data.frame(someOtherColumn = 1:3)

  expect_error(
    tobii_pro_adapter$extract_events(data),
    regexp = "Sensor"
  )
})

# --- Synthetic data with actual event rows --------------------------------
# All 4 Tobii fixtures happen to have zero event rows in both the old and
# new paths, so the equivalence tests above don't exercise the branch that
# actually separates gaze rows from event rows. These synthetic cases
# (mirroring test-extractEventRows.R's own synthetic-data tests for the old
# function) construct minimal standardized-shape data with a genuine mix of
# gaze and event rows, and confirm the new adapter methods split them
# identically to the old function.

test_that("TobiiPro adapter extract_events() splits gaze vs event rows on synthetic data", {
  data <- data.frame(
    Sensor = c("Eye Tracker", "Eye Tracker", "Mouse", NA),
    stringsAsFactors = FALSE
  )

  old_result <- extractEventRows(data, software = "TobiiPro")
  new_result <- tobii_pro_adapter$extract_events(data)

  expect_equal(nrow(new_result$gaze), 2)
  expect_equal(nrow(new_result$events), 2)
  expect_equal(old_result[[1]], new_result$gaze)
  expect_equal(old_result[[2]], new_result$events)
})

test_that("TobiiStudio adapter extract_events() splits gaze vs event rows on synthetic data", {
  data <- data.frame(
    eyeTrackerTimestamp = c(0, 17, -9999, NA),
    stringsAsFactors = FALSE
  )

  old_result <- extractEventRows(data, software = "TobiiStudio")
  new_result <- tobii_studio_adapter$extract_events(data)

  expect_equal(nrow(new_result$gaze), 2)
  expect_equal(nrow(new_result$events), 2)
  expect_equal(old_result[[1]], new_result$gaze)
  expect_equal(old_result[[2]], new_result$events)
})
