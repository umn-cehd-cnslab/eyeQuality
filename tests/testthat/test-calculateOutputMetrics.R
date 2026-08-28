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

# Precision (SD + RMS sample-to-sample) QC metrics. Ports an existing lab
# script (previously run ad hoc, outside this package, against IBIS study
# data) into the pipeline, generalized to three fixation-selection variants
# (whole file / longest fixation / median fixation) and a sampling-rate-
# derived rolling window instead of a hardcoded ~300Hz assumption. These
# tests cover the internal helpers directly (.select_fixation_rows(),
# .precision_window_size_samples(), .calculate_precision_metrics()) plus
# end-to-end wiring through calculateOutputMetrics() and saveFiles().

test_that(".select_fixation_rows(\"longest\") includes every fixation tied for the max duration", {
  # Fixations 2 and 3 are both 100ms (tied for longest); 1 and 4 are
  # shorter. Mirrors the reference script's unique(na.omit(...)) tie
  # handling: both fixation indices' rows should come back together.
  data <- data.frame(
    IVT.fixationIndex = c(1, 1, 2, 2, 3, 3, 4),
    IVT.fixationDuration_ms = c(50, 50, 100, 100, 100, 100, 30)
  )

  result <- .select_fixation_rows(data, "longest")

  expect_equal(sort(unique(result$IVT.fixationIndex)), c(2, 3))
  expect_equal(nrow(result), 4)
})

test_that(".select_fixation_rows(\"median\") includes every fixation tied for the median duration, computed over all rows (not distinct fixations)", {
  # Full IVT.fixationDuration_ms column (one value per row, duplicated
  # across every row of a fixation, exactly like the reference script):
  # 100, 200, 200, 200, 300, 200, 400 -> sorted: 100,200,200,200,200,300,400
  # -> median (n=7) = the 4th value = 200. Fixation 2 (3 rows) and
  # fixation 4 (1 row) both have duration 200 -- despite fixation 2
  # dominating the row-based median by sample count, both tied fixations'
  # rows should be selected together, same tie-handling as "longest".
  data <- data.frame(
    IVT.fixationIndex = c(1, 2, 2, 2, 3, 4, 5),
    IVT.fixationDuration_ms = c(100, 200, 200, 200, 300, 200, 400)
  )
  expect_equal(median(data$IVT.fixationDuration_ms), 200)

  result <- .select_fixation_rows(data, "median")

  expect_equal(sort(unique(result$IVT.fixationIndex)), c(2, 4))
  expect_equal(nrow(result), 4)
})

test_that(".select_fixation_rows() returns zero rows, not an error, when there are no classified fixations", {
  data <- data.frame(
    IVT.fixationIndex = c(NA_real_, NA_real_, NA_real_),
    IVT.fixationDuration_ms = c(NA_real_, NA_real_, NA_real_)
  )

  expect_no_error(longest <- .select_fixation_rows(data, "longest"))
  expect_no_error(median_sel <- .select_fixation_rows(data, "median"))
  expect_equal(nrow(longest), 0)
  expect_equal(nrow(median_sel), 0)
})

test_that(".precision_window_size_samples() derives window size from the recording's actual sampling rate, not a hardcoded ~300Hz assumption", {
  # The reference script hardcoded window.size <- floor(window/3.3)+1,
  # baking in a ~300Hz sampling-rate assumption (1000/300 = 3.3ms/sample).
  # A recording sampled at a genuinely different rate must produce a
  # genuinely different window size, not silently reproduce 31 (the old
  # constant's result for a 100ms window).
  old_hardcoded_window_size <- floor(100 / 3.3) + 1
  expect_equal(old_hardcoded_window_size, 31)

  # ~1000Hz (1ms/sample) -- far from the old 300Hz assumption.
  data_1000hz <- data.frame(recordingTimestamp_ms = seq(0, by = 1, length.out = 500))
  expect_equal(.precision_window_size_samples(data_1000hz, window_ms = 100), 101)

  # ~58.8Hz (17ms/sample, this package's own Tobii Studio test fixtures'
  # rate) -- also far from the old 300Hz assumption, and different again
  # from the 1000Hz case above.
  data_58hz <- data.frame(recordingTimestamp_ms = seq(0, by = 17, length.out = 500))
  expect_equal(.precision_window_size_samples(data_58hz, window_ms = 100), 6)
})

test_that(".precision_window_size_samples() returns NA, not an error, when recordingTimestamp_ms is unavailable", {
  data <- data.frame(x = 1:5)
  expect_no_error(result <- .precision_window_size_samples(data, window_ms = 100))
  expect_true(is.na(result))
})

test_that(".calculate_precision_metrics() computes sdX/sdY and RMS-S2S correctly for a hand-verified gaze stream", {
  # gazeX drifts 0,1,3,6,10 (sdX = sd(c(0,1,3,6,10))); gazeY is held
  # constant at 0 (sdY = 0). With window_size_samples = 3 (lag = 1),
  # runner::runner() only has enough trailing samples to evaluate the
  # rolling RMS-S2S function once over this 5-row stream, so rmsX_mean and
  # rmsX_median come out identical -- both values confirmed by direct
  # execution.
  data <- data.frame(
    gazeX.preprocessed_va = c(0, 1, 3, 6, 10),
    gazeY.preprocessed_va = c(0, 0, 0, 0, 0)
  )

  result <- .calculate_precision_metrics(data, "all", window_size_samples = 3)

  expect_equal(result$sdX, sd(c(0, 1, 3, 6, 10)))
  expect_equal(result$sdY, 0)
  expect_equal(result$rmsX_mean, 1.68633, tolerance = 1e-5)
  expect_equal(result$rmsX_median, 1.68633, tolerance = 1e-5)
  expect_equal(result$rmsY_mean, 0)
  expect_equal(result$rmsY_median, 0)
  expect_equal(result$rmsEuc_mean, 1.68633, tolerance = 1e-5)
  expect_equal(result$rmsEuc_median, 1.68633, tolerance = 1e-5)
})

test_that(".calculate_precision_metrics() degrades gracefully to NA for longest/median fixation variants when there are zero classified fixations, while \"all\" still computes normally", {
  data <- data.frame(
    gazeX.preprocessed_va = c(0, 1, 3, 6, 10),
    gazeY.preprocessed_va = c(0, 0, 0, 0, 0),
    IVT.fixationIndex = rep(NA_real_, 5),
    IVT.fixationDuration_ms = rep(NA_real_, 5)
  )

  expect_no_error(result_all <- .calculate_precision_metrics(data, "all", window_size_samples = 3))
  expect_no_error(result_longest <- .calculate_precision_metrics(data, "longest", window_size_samples = 3))
  expect_no_error(result_median <- .calculate_precision_metrics(data, "median", window_size_samples = 3))

  expect_equal(result_all$sdX, sd(c(0, 1, 3, 6, 10)))
  expect_false(is.na(result_all$sdX))

  for (metric in names(result_longest)) {
    expect_true(is.na(result_longest[[metric]]), info = metric)
    expect_true(is.na(result_median[[metric]]), info = metric)
  }
})

test_that(".calculate_precision_metrics() returns all-NA, not an error, when the visual-angle gaze columns are missing entirely", {
  # Forward-compatibility guard for a future non-screen-based adapter --
  # not currently reachable via any built-in adapter, but shouldn't crash
  # calculateOutputMetrics() if it ever is.
  data <- data.frame(
    gazeX = c(0, 1, 3),
    IVT.fixationIndex = c(1, 1, 1),
    IVT.fixationDuration_ms = c(10, 10, 10)
  )

  expect_no_error(result <- .calculate_precision_metrics(data, "all", window_size_samples = 3))
  for (metric in names(result)) {
    expect_true(is.na(result[[metric]]), info = metric)
  }
})

test_that("calculateOutputMetrics() reports real, non-NA whole-file precision on a fixture with genuine gaze variance", {
  skip_on_cran()

  # tobii_studio_precision_movement.tsv is a synthetic-but-real Tobii
  # Studio export with deterministically jittering gaze (not the constant
  # single-pixel gaze most other fixtures in this suite use, which would
  # trivially produce sdX = sdY = 0 and defeat the point of this test).
  fp <- testthat::test_path("fixtures", "tobii_studio_precision_movement.tsv")
  result <- eyeQuality(
    fp,
    displayDimensionX_mm = 594,
    displayDimensionY_mm = 344,
    saveData = FALSE,
    includeIntermediates = TRUE
  )

  metrics <- calculateOutputMetrics(result)

  # Ground truth computed directly from the pipeline's own output columns,
  # the same way .calculate_precision_metrics() computes it -- this
  # protects the wiring (right columns, right formula) rather than
  # asserting a magic number that would need updating with any upstream
  # pipeline change.
  expect_equal(
    metrics["precision_sdX_wholeFile", "precision_value"],
    sd(result$gazeX.preprocessed_va, na.rm = TRUE)
  )
  expect_equal(
    metrics["precision_sdY_wholeFile", "precision_value"],
    sd(result$gazeY.preprocessed_va, na.rm = TRUE)
  )
  expect_false(is.na(metrics["precision_rmsEuc_mean_wholeFile", "precision_value"]))
  expect_true(metrics["precision_sdX_wholeFile", "precision_value"] > 0)
  expect_true(metrics["precision_sdY_wholeFile", "precision_value"] > 0)

  # percent/percent_numerator/percent_denominator don't apply to precision
  # rows (these are raw visual-angle-degree magnitudes, not ratios) and
  # should be left NA, not populated with something nonsensical.
  expect_true(is.na(metrics["precision_sdX_wholeFile", "percent"]))
  expect_true(is.na(metrics["precision_sdX_wholeFile", "percent_numerator"]))
  expect_true(is.na(metrics["precision_sdX_wholeFile", "percent_denominator"]))
})

test_that("saveFiles()'s qcsummary.tsv carries all 24 new precision rows through to disk", {
  skip_on_cran()

  fp <- testthat::test_path("fixtures", "tobii_studio_precision_movement.tsv")
  out_dir <- tempfile("p_precision_qc_")
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
  expect_true("precision_value" %in% names(qc))

  metrics <- c("sdX", "sdY", "rmsX_mean", "rmsX_median", "rmsY_mean", "rmsY_median", "rmsEuc_mean", "rmsEuc_median")
  variants <- c("wholeFile", "longestFixation", "medianFixation")
  expected_rows <- as.vector(outer(metrics, variants, function(m, v) paste0("precision_", m, "_", v)))
  expect_equal(length(expected_rows), 24)

  for (row_name in expected_rows) {
    row <- qc[qc$qc_metric == row_name, ]
    expect_equal(nrow(row), 1, info = row_name)
    expect_false(is.na(row$precision_value), info = row_name)
  }
})
