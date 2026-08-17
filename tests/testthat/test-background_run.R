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
