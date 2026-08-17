# P9-05: regression tests for the synchronous/pure pieces of the Setup app's
# background batch-run mechanism (inst/shiny-apps/setup/background_run.R):
# count_completed_qcsummary_files() and poll_batch_progress()'s counting
# logic. Sourced via system.file() for the same reasons as
# test-runSetupApp.R (P9-01) -- it's the only portable way to reach inst/
# files from tests, and doubles as a packaging-location check.
#
# start_background_batch()/ensure_future_plan() (the actual future/promises
# async machinery) are deliberately NOT covered here with a full async
# integration test -- that was independently exercised end-to-end against
# real eyeQualityBatch() runs (happy path, mixed success/failure, upfront
# validation error) as part of this task's verification, and a from-scratch
# testthat re-implementation of that same async harness would mostly test
# future/promises' own resolution machinery rather than this file's logic,
# for a real flakiness cost (timing-dependent later::run_now() loops). The
# two functions below are pure/synchronous and count-based, so they get
# direct unit coverage instead.

background_run_path <- system.file("shiny-apps", "setup", "background_run.R", package = "eyeQuality")
if (!nzchar(background_run_path)) {
  stop("test-background_run.R: could not locate inst/shiny-apps/setup/background_run.R via system.file()")
}
source(background_run_path, local = TRUE)

# Creates a qcsummary output file at the exact path get_qcsummary_output_path()
# (R/eyeQualityBatch.R) would construct for a given raw input file, so
# count_completed_qcsummary_files()'s naming-convention assumption is
# exercised against the real convention rather than a hand-guessed one.
touch_qcsummary <- function(inputFile, batchName, outputDir = NULL) {
  path <- get_qcsummary_output_path(inputFile, batchName = batchName, outputDir = outputDir)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  file.create(path)
  path
}

test_that("count_completed_qcsummary_files finds qcsummary outputs recursively under directoryBIDS by default", {
  root <- tempfile("p905_qc_")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  touch_qcsummary(file.path(root, "sub-01", "ses-01", "a_recording-eyetracking_physio.tsv"), batchName = "run1")
  touch_qcsummary(file.path(root, "sub-02", "ses-01", "b_recording-eyetracking_physio.tsv"), batchName = "run1")

  found <- count_completed_qcsummary_files(root, batchName = "run1")

  expect_length(found, 2)
  expect_true(all(grepl("_desc-run1_preproc_qcsummary\\.tsv$", found)))
})

test_that("count_completed_qcsummary_files only counts outputs matching the given batchName, not other batches", {
  root <- tempfile("p905_qc_")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  touch_qcsummary(file.path(root, "sub-01", "a_recording-eyetracking_physio.tsv"), batchName = "run1")
  touch_qcsummary(file.path(root, "sub-01", "a_recording-eyetracking_physio.tsv"), batchName = "run2")

  expect_length(count_completed_qcsummary_files(root, batchName = "run1"), 1)
  expect_length(count_completed_qcsummary_files(root, batchName = "run2"), 1)
})

test_that("count_completed_qcsummary_files returns empty character vector when nothing has completed yet", {
  root <- tempfile("p905_qc_")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  found <- count_completed_qcsummary_files(root, batchName = "run1")

  expect_equal(found, character(0))
})

test_that("count_completed_qcsummary_files returns empty character vector when directoryBIDS does not exist yet", {
  # Mirrors calling poll_batch_progress() before a run has ever created its
  # target directory -- must not error, just report zero progress.
  missing_dir <- file.path(tempdir(), "p905_does_not_exist_xyz")

  found <- count_completed_qcsummary_files(missing_dir, batchName = "run1")

  expect_equal(found, character(0))
})

test_that("count_completed_qcsummary_files looks under outputDir instead of directoryBIDS when outputDir is supplied", {
  root <- tempfile("p905_qc_")
  out <- tempfile("p905_out_")
  dir.create(root, recursive = TRUE)
  dir.create(out, recursive = TRUE)
  on.exit(unlink(c(root, out), recursive = TRUE), add = TRUE)

  # Written under directoryBIDS's own default derivatives/ location -- should
  # NOT be counted once outputDir is supplied.
  touch_qcsummary(file.path(root, "sub-01", "a_recording-eyetracking_physio.tsv"), batchName = "run1")
  # Written under the override outputDir -- should be counted.
  touch_qcsummary(file.path(root, "sub-01", "a_recording-eyetracking_physio.tsv"), batchName = "run1", outputDir = out)

  found <- count_completed_qcsummary_files(root, batchName = "run1", outputDir = out)

  expect_length(found, 1)
  expect_true(startsWith(found, out))
})

test_that("poll_batch_progress reports n_done from completed qcsummary files and echoes n_expected back unchanged", {
  root <- tempfile("p905_poll_")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  touch_qcsummary(file.path(root, "sub-01", "a_recording-eyetracking_physio.tsv"), batchName = "run1")
  touch_qcsummary(file.path(root, "sub-02", "b_recording-eyetracking_physio.tsv"), batchName = "run1")

  result <- poll_batch_progress(root, batchName = "run1", n_expected = 4)

  expect_equal(result$n_done, 2)
  expect_equal(result$n_expected, 4)
})

test_that("poll_batch_progress reports n_failed as NA before a batch summary file exists (mid-run/not-yet-started)", {
  root <- tempfile("p905_poll_")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  result <- poll_batch_progress(root, batchName = "run1", n_expected = 4)

  expect_true(is.na(result$n_failed))
  expect_equal(result$n_done, 0)
})

test_that("poll_batch_progress reports n_failed as NA if the batch summary file exists but its failedfiles section isn't parseable yet", {
  root <- tempfile("p905_poll_")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  # Mimics a batch summary file mid-write (e.g. only the header line written
  # so far) -- parsePreprocessingBatchSummary() should error against this,
  # and poll_batch_progress() must catch that and report NA rather than
  # propagating the error to the polling caller.
  summary_file <- file.path(root, "preprocessing_batch_summary_desc-run1.txt")
  writeLines("-----------------", summary_file)

  result <- poll_batch_progress(root, batchName = "run1", n_expected = 4)

  expect_true(is.na(result$n_failed))
})

test_that("poll_batch_progress reports the real n_failed count once the batch summary's failedfiles section is parseable", {
  root <- tempfile("p905_poll_")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  touch_qcsummary(file.path(root, "sub-01", "a_recording-eyetracking_physio.tsv"), batchName = "run1")
  touch_qcsummary(file.path(root, "sub-02", "b_recording-eyetracking_physio.tsv"), batchName = "run1")

  # Hand-built reproduction of the exact byte format eyeQualityBatch()'s
  # print_or_save() writes (see test-parsePreprocessingBatchSummary.R's
  # write_batch_summary_fixture() for the authoritative version of this
  # format) -- only the "Files that failed processing" section actually
  # matters for poll_batch_progress()'s n_failed logic, but the header lines
  # before it are included so parsePreprocessingBatchSummary()'s own section
  # lookup succeeds.
  summary_file <- file.path(root, "preprocessing_batch_summary_desc-run1.txt")
  lines <- c(
    "-----------------",
    "starting batch run: 2026-01-01 00:00:00",
    "------ Skipped files (already processed, resumability; n = 0):  ",
    "",
    "number of cores = 1",
    "------ Successfully processed files (n = 2):  ",
    "sub-01/a_recording-eyetracking_physio.tsv\nsub-02/b_recording-eyetracking_physio.tsv",
    "------ Files that failed processing (n = 1):  ",
    "sub-03/c_recording-eyetracking_physio.tsv\n  error: stub error",
    "--- BATCH PROCESSING SUMMARY:  ",
    '"directory": "/data", "n (ET Files)": "3", "n (preprocessed)": "2", "n (failed preprocessing)": "1"'
  )
  readr::write_lines(lines, summary_file)

  result <- poll_batch_progress(root, batchName = "run1", n_expected = 3)

  expect_equal(result$n_done, 2)
  expect_equal(result$n_failed, 1)
})

# P9-06: regression tests for the pure display-support helpers added on top
# of P9-05's polling mechanism -- get_failed_file_details() (wraps
# parsePreprocessingBatchSummary()'s "failedfiles" section),
# estimate_remaining_seconds()/format_duration_seconds() (naive linear ETA).
# All three are plain functions with no reactives, so they're covered
# directly like count_completed_qcsummary_files()/poll_batch_progress() above.

test_that("get_failed_file_details returns NULL when the batch summary file does not exist yet", {
  root <- tempfile("p906_failed_")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  expect_null(get_failed_file_details(root, batchName = "run1"))
})

test_that("get_failed_file_details returns NULL when the batch summary exists but isn't parseable yet (mid-write)", {
  root <- tempfile("p906_failed_")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  summary_file <- file.path(root, "preprocessing_batch_summary_desc-run1.txt")
  writeLines("-----------------", summary_file)

  expect_null(get_failed_file_details(root, batchName = "run1"))
})

test_that("get_failed_file_details returns a zero-row file/error data frame when the failedfiles section reports n = 0", {
  root <- tempfile("p906_failed_")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  summary_file <- file.path(root, "preprocessing_batch_summary_desc-run1.txt")
  lines <- c(
    "-----------------",
    "starting batch run: 2026-01-01 00:00:00",
    "------ Skipped files (already processed, resumability; n = 0):  ",
    "",
    "number of cores = 1",
    "------ Successfully processed files (n = 2):  ",
    "sub-01/a_recording-eyetracking_physio.tsv\nsub-02/b_recording-eyetracking_physio.tsv",
    "------ Files that failed processing (n = 0):  ",
    "",
    "--- BATCH PROCESSING SUMMARY:  ",
    '"directory": "/data", "n (ET Files)": "2", "n (preprocessed)": "2", "n (failed preprocessing)": "0"'
  )
  readr::write_lines(lines, summary_file)

  result <- get_failed_file_details(root, batchName = "run1")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(colnames(result), c("file", "error"))
})

test_that("get_failed_file_details returns real per-file names and error messages once the batch summary is parseable", {
  root <- tempfile("p906_failed_")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  summary_file <- file.path(root, "preprocessing_batch_summary_desc-run1.txt")
  lines <- c(
    "-----------------",
    "starting batch run: 2026-01-01 00:00:00",
    "------ Skipped files (already processed, resumability; n = 0):  ",
    "",
    "number of cores = 1",
    "------ Successfully processed files (n = 1):  ",
    "sub-01/a_recording-eyetracking_physio.tsv",
    "------ Files that failed processing (n = 2):  ",
    paste0(
      "sub-02/b_recording-eyetracking_physio.tsv\n  error: not chronologically ordered\n",
      "sub-03/c_recording-eyetracking_physio.tsv\n  error: missing required column"
    ),
    "--- BATCH PROCESSING SUMMARY:  ",
    '"directory": "/data", "n (ET Files)": "3", "n (preprocessed)": "1", "n (failed preprocessing)": "2"'
  )
  readr::write_lines(lines, summary_file)

  result <- get_failed_file_details(root, batchName = "run1")

  expect_equal(nrow(result), 2)
  expect_equal(result$file, c(
    "sub-02/b_recording-eyetracking_physio.tsv",
    "sub-03/c_recording-eyetracking_physio.tsv"
  ))
  expect_equal(result$error, c("not chronologically ordered", "missing required column"))
})

test_that("estimate_remaining_seconds withholds an estimate (NA) before any file has completed", {
  expect_true(is.na(estimate_remaining_seconds(Sys.time() - 10, n_done = 0, n_expected = 5)))
})

test_that("estimate_remaining_seconds withholds an estimate (NA) once nothing remains", {
  expect_true(is.na(estimate_remaining_seconds(Sys.time() - 10, n_done = 5, n_expected = 5)))
})

test_that("estimate_remaining_seconds handles n_expected = 0 without dividing by zero", {
  expect_true(is.na(estimate_remaining_seconds(Sys.time() - 10, n_done = 0, n_expected = 0)))
})

test_that("estimate_remaining_seconds returns NA rather than a negative estimate for a future (bad-clock) start_time", {
  # start_time after "now" would otherwise make elapsed negative, and thus
  # the naive rate/remaining-time math produce a nonsensical negative ETA --
  # this is an honest guard against that, not a realistic scenario.
  future_start <- Sys.time() + 100
  expect_true(is.na(estimate_remaining_seconds(future_start, n_done = 2, n_expected = 5)))
})

test_that("estimate_remaining_seconds returns NA for NULL/NA inputs", {
  expect_true(is.na(estimate_remaining_seconds(NULL, n_done = 1, n_expected = 5)))
  expect_true(is.na(estimate_remaining_seconds(Sys.time(), n_done = NA_integer_, n_expected = 5)))
  expect_true(is.na(estimate_remaining_seconds(Sys.time(), n_done = 1, n_expected = NA_integer_)))
})

test_that("estimate_remaining_seconds extrapolates linearly from elapsed time and completed-file rate", {
  # 2 of 4 files done after ~10 real seconds elapsed -> rate = 0.2 files/sec,
  # 2 remaining -> ~10 more seconds. Real Sys.time() elapsed during test
  # execution is negligible relative to the 10-second start offset, so a
  # generous tolerance absorbs that without making the test vacuous.
  eta <- estimate_remaining_seconds(Sys.time() - 10, n_done = 2, n_expected = 4)
  expect_equal(eta, 10, tolerance = 0.5)
})

test_that("format_duration_seconds returns NA_character_ for NA input", {
  expect_true(is.na(format_duration_seconds(NA_real_)))
})

test_that("format_duration_seconds returns NA_character_ for negative input", {
  expect_true(is.na(format_duration_seconds(-5)))
})

test_that("format_duration_seconds formats sub-minute durations in seconds", {
  expect_equal(format_duration_seconds(30), "~30 sec")
})

test_that("format_duration_seconds never renders a misleading '~0 sec' for a tiny positive duration", {
  expect_equal(format_duration_seconds(0.2), "~1 sec")
})

test_that("format_duration_seconds formats sub-hour durations in minutes", {
  expect_equal(format_duration_seconds(90), "~1.5 min")
})

test_that("format_duration_seconds formats hour-plus durations in hours", {
  expect_equal(format_duration_seconds(7200), "~2.0 hr")
})

test_that("count_completed_qcsummary_files is called by poll_batch_progress with the same outputDir override", {
  # Regression guard against poll_batch_progress() forgetting to thread
  # outputDir through to count_completed_qcsummary_files() -- would silently
  # report n_done = 0 forever for any run configured with a custom outputDir.
  root <- tempfile("p905_poll_")
  out <- tempfile("p905_out_")
  dir.create(root, recursive = TRUE)
  dir.create(out, recursive = TRUE)
  on.exit(unlink(c(root, out), recursive = TRUE), add = TRUE)

  touch_qcsummary(file.path(root, "sub-01", "a_recording-eyetracking_physio.tsv"), batchName = "run1", outputDir = out)

  result <- poll_batch_progress(root, batchName = "run1", n_expected = 1, outputDir = out)

  expect_equal(result$n_done, 1)
})
