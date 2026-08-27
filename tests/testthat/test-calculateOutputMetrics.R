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

# Bug: the "interpolated_LeftEye"/"interpolated_RightEye" (and, by the same
# mechanism, "smoothed_X"/"smoothed_Y"/"smoothed_pupil"/"smoothed_distZ"/
# "smoothed_velocity_X"/"smoothed_velocity_Y") rows relied on a
# `.data$col %>% replace(is.na(.data), 0)` idiom to zero out NAs on both
# sides of a `!=` comparison before counting how many rows changed. Bare
# `.data` inside dplyr::summarise() is the whole-tibble rlang data pronoun,
# not the single column being piped -- `is.na(.data)` therefore evaluated to
# a single scalar (not an elementwise mask), so `replace()` was a no-op and
# any real NA surviving on either side of the `!=` (which the raw ".valid"
# column always has wherever there's a genuine tracking gap) turned the
# whole `ifelse()`/`sum()` into NA via ordinary NA propagation -- not zero,
# not a missing row, an actual NA value silently written into
# qcsummary.tsv. Every recording with any raw tracking loss at all hit this,
# which is effectively every real recording; the pre-existing fixtures
# (tobii_studio_sample.tsv, tobii_pro_sample.tsv, etc.) all happen to have
# zero raw-invalid samples by design, which is why nothing caught this
# earlier. Fixed by using magrittr's `.` (the actual LHS of the pipe, i.e.
# the single column) instead of `.data` inside each replace() call.
test_that("calculateOutputMetrics reports a real interpolated count, not NA, when a recording has a short, interpolatable gap", {
  skip_on_cran()

  # tests/testthat/fixtures/tobii_studio_interpolatable_blink.tsv is
  # tobii_studio_sample.tsv (constant gaze, otherwise 200/200 valid) with a
  # 2-sample (~34ms) left-eye blink introduced at rows 100-101 -- well
  # within interpolateGaze()'s default 50ms maxGapLength_ms window and
  # flanked by valid samples on both sides, so na.approx() actually fills
  # it. This is deliberately a genuine "interpolated" gap, not merely a
  # "missing raw data" one too large to fill, so the test exercises the
  # actual .int-vs-.valid comparison this row depends on.
  fp <- testthat::test_path("fixtures", "tobii_studio_interpolatable_blink.tsv")
  result <- eyeQuality(
    fp,
    displayDimensionX_mm = 594,
    displayDimensionY_mm = 344,
    saveData = FALSE,
    includeIntermediates = TRUE
  )

  metrics <- calculateOutputMetrics(result)

  expect_equal(metrics["missing_raw_data_LeftEye", "n"], 2)
  expect_equal(metrics["interpolated_LeftEye", "n"], 2)
  expect_equal(metrics["interpolated_LeftEye", "percent_numerator"], 2)
  expect_false(is.na(metrics["interpolated_LeftEye", "percent"]))

  # The right eye had no gap at all in this fixture -- confirms the fix
  # reports a real 0, not NA, when there was nothing to interpolate either.
  expect_equal(metrics["interpolated_RightEye", "n"], 0)
  expect_false(is.na(metrics["interpolated_RightEye", "percent"]))
})

test_that("saveFiles()'s qcsummary.tsv carries a real interpolated_LeftEye value through to disk, not the literal string \"NA\"", {
  skip_on_cran()

  fp <- testthat::test_path("fixtures", "tobii_studio_interpolatable_blink.tsv")
  out_dir <- tempfile("p_interp_qc_")
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  eyeQuality(
    fp,
    displayDimensionX_mm = 594,
    displayDimensionY_mm = 344,
    saveData = TRUE,
    outputDir = out_dir
  )

  qc_files <- list.files(out_dir, pattern = "qcsummary\\.tsv$", recursive = TRUE, full.names = TRUE)
  expect_length(qc_files, 1)

  qc <- read.delim(qc_files[1], stringsAsFactors = FALSE)
  left_row <- qc[qc$qc_metric == "interpolated_LeftEye", ]
  expect_equal(nrow(left_row), 1)
  expect_false(is.na(left_row$n))
  expect_equal(left_row$n, 2)
})
