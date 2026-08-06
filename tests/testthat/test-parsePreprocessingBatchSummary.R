# Regression tests for P1-07: parsePreprocessingBatchSummary()'s
# "failedfiles" and "successfulfiles" branches were previously an empty stub
# and entirely missing (respectively). Both now parse the
# "------ Successfully processed files (n = ...)" / "------ Files that failed
# processing (n = ...)" sections written by eyeQualityBatch.R's
# print_or_save() calls.
#
# The fixture text below is a literal, hand-built reproduction of the exact
# byte format print_or_save() writes (each call appends its text followed by
# a single "\n" - see print_or_save() in R/saveFiles.R), rather than a full
# eyeQualityBatch() run, since it's the text format itself being parsed, not
# the pipeline that produces it.

# Builds a preprocessing_batch_summary_desc-*.txt fixture matching
# print_or_save()'s exact output format. `successful` and `failed` are
# character vectors of filepaths (possibly empty) for the two sections.
# `skipped` (P7-03) is the third such section - files eyeQualityBatch()
# skipped because a matching qcsummary output already existed for the given
# batchName/outputDir (resumability). Positioned, like in the real
# eyeQualityBatch() output, before the "number of cores" line (the skip
# check happens before cluster dispatch).
write_batch_summary_fixture <- function(dir,
                                        successful = character(0),
                                        failed = character(0),
                                        skipped = character(0),
                                        filename = "preprocessing_batch_summary_desc-test.txt") {
  lines <- c(
    "-----------------",
    "starting batch run: 2026-01-01 00:00:00",
    paste0("------ Skipped files (already processed, resumability; n = ", length(skipped), "):  "),
    paste(skipped, collapse = "\n"),
    "number of cores = 1",
    paste0("------ Successfully processed files (n = ", length(successful), "):  "),
    paste(successful, collapse = "\n"),
    paste0("------ Files that failed processing (n = ", length(failed), "):  "),
    paste(failed, collapse = "\n"),
    "--- BATCH PROCESSING SUMMARY:  ",
    '"directory": "/data/study", "data size (MB)": "12.3", "n (ET Files)": "3", "n (preprocessed)": "2", "n (failed preprocessing)": "1", "run duration": "0h 0m 5s",  "runtime (s)": "5"'
  )

  filepath <- file.path(dir, filename)
  # Mirror print_or_save(): each element gets its own line, written via cat()
  # with a trailing newline per call - readr::write_lines reproduces this.
  readr::write_lines(lines, filepath)
  filepath
}

test_that("parsePreprocessingBatchSummary returns the successful file list for info_to_extract = 'successfulfiles'", {
  dir <- tempfile("p107_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  successful <- c(
    "/data/sub-01_task-test_recording-eyetracking_physio.tsv",
    "/data/sub-02_task-test_recording-eyetracking_physio.tsv"
  )
  failed <- c("/data/sub-03_task-test_recording-eyetracking_physio.tsv")

  f <- write_batch_summary_fixture(dir, successful = successful, failed = failed)

  result <- parsePreprocessingBatchSummary(f, "successfulfiles")

  expect_equal(result, successful)
})

test_that("parsePreprocessingBatchSummary returns the failed file list for info_to_extract = 'failedfiles'", {
  dir <- tempfile("p107_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  successful <- c(
    "/data/sub-01_task-test_recording-eyetracking_physio.tsv",
    "/data/sub-02_task-test_recording-eyetracking_physio.tsv"
  )
  failed <- c("/data/sub-03_task-test_recording-eyetracking_physio.tsv")

  f <- write_batch_summary_fixture(dir, successful = successful, failed = failed)

  result <- parsePreprocessingBatchSummary(f, "failedfiles")

  expect_equal(result, failed)
})

test_that("parsePreprocessingBatchSummary returns character(0) for an empty successfulfiles section (n = 0)", {
  dir <- tempfile("p107_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  f <- write_batch_summary_fixture(
    dir,
    successful = character(0),
    failed = c("/data/sub-01_task-test_recording-eyetracking_physio.tsv")
  )

  result <- parsePreprocessingBatchSummary(f, "successfulfiles")

  expect_equal(result, character(0))
})

test_that("parsePreprocessingBatchSummary returns character(0) for an empty failedfiles section (n = 0)", {
  dir <- tempfile("p107_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  f <- write_batch_summary_fixture(
    dir,
    successful = c("/data/sub-01_task-test_recording-eyetracking_physio.tsv"),
    failed = character(0)
  )

  result <- parsePreprocessingBatchSummary(f, "failedfiles")

  expect_equal(result, character(0))
})

# P7-03: parsePreprocessingBatchSummary() gained a "skippedfiles"
# info_to_extract branch alongside "failedfiles"/"successfulfiles", parsing
# the new "------ Skipped files (already processed, resumability; n = ...)"
# section eyeQualityBatch() writes for files it skipped because a matching
# qcsummary output already existed (resumability).
test_that("parsePreprocessingBatchSummary returns the skipped file list for info_to_extract = 'skippedfiles'", {
  dir <- tempfile("p703_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  skipped <- c(
    "/data/sub-01_task-test_recording-eyetracking_physio.tsv",
    "/data/sub-02_task-test_recording-eyetracking_physio.tsv"
  )
  successful <- c("/data/sub-03_task-test_recording-eyetracking_physio.tsv")

  f <- write_batch_summary_fixture(dir, successful = successful, skipped = skipped)

  result <- parsePreprocessingBatchSummary(f, "skippedfiles")

  expect_equal(result, skipped)
})

test_that("parsePreprocessingBatchSummary returns character(0) for an empty skippedfiles section (n = 0)", {
  dir <- tempfile("p703_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  f <- write_batch_summary_fixture(
    dir,
    successful = c("/data/sub-01_task-test_recording-eyetracking_physio.tsv"),
    skipped = character(0)
  )

  result <- parsePreprocessingBatchSummary(f, "skippedfiles")

  expect_equal(result, character(0))
})

# Regression test for P1-15: R/parsePreprocessingBatchSummary.R:29 onward
# previously subset the dplyr data pronoun positionally (`.data[[1]]`), which
# errors under this project's installed rlang/dplyr versions ("Must subset
# the data pronoun with a string, not the number 1."). It's now
# `.data[["X1"]]`, the actual auto-assigned column name read_tsv() gives an
# unnamed single-column file (col_names = FALSE).
test_that("parsePreprocessingBatchSummary parses the summary line's fields for info_to_extract = 'summary'", {
  dir <- tempfile("p107_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  successful <- c(
    "/data/sub-01_task-test_recording-eyetracking_physio.tsv",
    "/data/sub-02_task-test_recording-eyetracking_physio.tsv"
  )
  failed <- c("/data/sub-03_task-test_recording-eyetracking_physio.tsv")

  f <- write_batch_summary_fixture(dir, successful = successful, failed = failed)

  result <- parsePreprocessingBatchSummary(f, "summary")

  expect_equal(result$directory, "/data/study")
  expect_equal(result$datasize, 12.3)
  expect_equal(result$nfiles, 3)
  expect_equal(result$nPreprocessed, 2)
  expect_equal(result$nFailed, 1)
  expect_equal(result$runDuration, "0h 0m 5s")
  expect_equal(result$runTime, 5)
})

test_that("parsePreprocessingBatchSummary errors on an unrecognized info_to_extract value", {
  dir <- tempfile("p107_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  f <- write_batch_summary_fixture(
    dir,
    successful = c("/data/sub-01_task-test_recording-eyetracking_physio.tsv"),
    failed = character(0)
  )

  expect_error(
    parsePreprocessingBatchSummary(f, "bogus"),
    regexp = "bogus"
  )
})

test_that("parsePreprocessingBatchSummary errors when the requested section header is missing from the file", {
  dir <- tempfile("p107_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  # A file with no "Files that failed processing" section at all.
  filepath <- file.path(dir, "preprocessing_batch_summary_desc-nosection.txt")
  readr::write_lines(
    c(
      "-----------------",
      "starting batch run: 2026-01-01 00:00:00",
      "------ Successfully processed files (n = 1):  ",
      "/data/sub-01_task-test_recording-eyetracking_physio.tsv"
    ),
    filepath
  )

  expect_error(
    parsePreprocessingBatchSummary(filepath, "failedfiles")
  )
})
