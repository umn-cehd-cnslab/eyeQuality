# P1-16: calculateOutputMetrics() used bare min()/max() for every group_min
# / group_max summary column. When a group had zero rows (e.g. a recording
# with no saccades or blinks) or every candidate value was NA (e.g. a
# single-sample IVT.classification run at the very start/end of a recording,
# where getSequenceGroupEndpoints() can't compute a duration), min()/max()
# silently returned Inf/-Inf with a warning instead of the correct NA. The
# fix introduced safe_min()/safe_max() helpers at the top of
# R/calculateOutputMetrics.R that special-case the all-NA/empty case. These
# tests protect both the helpers directly and the end-to-end behavior on a
# real fixture that naturally produces empty saccade/blink/missing groups.

test_that("safe_min/safe_max return NA instead of Inf/-Inf for empty or all-NA input", {
  expect_true(is.na(safe_min(numeric(0))))
  expect_true(is.na(safe_max(numeric(0))))
  expect_true(is.na(safe_min(c(NA_real_, NA_real_))))
  expect_true(is.na(safe_max(c(NA_real_, NA_real_))))
})

test_that("safe_min/safe_max behave like min/max na.rm=TRUE when real values are present", {
  expect_equal(safe_min(c(3, 1, NA, 2)), 1)
  expect_equal(safe_max(c(3, 1, NA, 2)), 3)
})

test_that("safe_min/safe_max don't warn on empty or all-NA input", {
  expect_no_warning(safe_min(numeric(0)))
  expect_no_warning(safe_max(numeric(0)))
  expect_no_warning(safe_min(c(NA_real_, NA_real_)))
  expect_no_warning(safe_max(c(NA_real_, NA_real_)))
})

test_that("calculateOutputMetrics reports NA, not Inf/-Inf, for group_min/group_max of empty IVT groups", {
  skip_on_cran()

  # gaze is held constant at one pixel location for the whole 200-row
  # recording, so IVT classification never produces a saccade or a blink,
  # and "missing" never occurs either -- these three groups are genuinely
  # empty (zero rows). "unclassified" additionally exercises the all-NA
  # case: it occurs at rows 1 and 200 only, and getSequenceGroupEndpoints()
  # can't compute a duration for either boundary sample, so the group has
  # rows but every duration value is NA.
  fp <- testthat::test_path("fixtures", "tobii_studio_sample.tsv")
  result <- eyeQuality(
    fp,
    displayDimensionX_mm = 594,
    displayDimensionY_mm = 344,
    saveData = FALSE,
    includeIntermediates = TRUE
  )

  expect_no_warning(metrics <- calculateOutputMetrics(result))

  for (row in c("ivt_saccades", "ivt_blinks", "ivt_missing", "ivt_unclassified")) {
    expect_true(is.na(metrics[row, "group_min"]), info = row)
    expect_true(is.na(metrics[row, "group_max"]), info = row)
    expect_false(isTRUE(is.infinite(metrics[row, "group_min"])), info = row)
    expect_false(isTRUE(is.infinite(metrics[row, "group_max"])), info = row)
  }

  # ivt_fixations, by contrast, is a real non-empty group and should still
  # report real min/max values, not NA -- confirms the fix didn't blank out
  # legitimate groups along with the empty ones.
  expect_false(is.na(metrics["ivt_fixations", "group_min"]))
  expect_false(is.na(metrics["ivt_fixations", "group_max"]))
})
