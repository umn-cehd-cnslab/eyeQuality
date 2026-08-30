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
#
# `failed_errors` (P7-04) is an optional character vector, parallel to
# `failed`, of each failed file's error message - the real "Files that
# failed processing" section now writes two lines per failed file (its
# filepath, then an indented "  error: <message>" line) rather than one.
# When `failed` is non-empty and `failed_errors` isn't supplied, a stub
# per-file message is generated, so callers that only care about a nonzero
# failed-file count (not the failedfiles section's error content) don't need
# to pass one.
write_batch_summary_fixture <- function(dir,
                                        successful = character(0),
                                        failed = character(0),
                                        failed_errors = NULL,
                                        skipped = character(0),
                                        filename = "preprocessing_batch_summary_desc-test.txt") {
  if (length(failed) > 0 && is.null(failed_errors)) {
    failed_errors <- paste0("stub error for ", basename(failed))
  }
  failed_lines <- if (length(failed) == 0) {
    character(0)
  } else {
    as.vector(rbind(failed, paste0("  error: ", failed_errors)))
  }

  lines <- c(
    "-----------------",
    "starting batch run: 2026-01-01 00:00:00",
    paste0("------ Skipped files (already processed, resumability; n = ", length(skipped), "):  "),
    paste(skipped, collapse = "\n"),
    "number of cores = 1",
    paste0("------ Successfully processed files (n = ", length(successful), "):  "),
    paste(successful, collapse = "\n"),
    paste0("------ Files that failed processing (n = ", length(failed), "):  "),
    paste(failed_lines, collapse = "\n"),
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

# P7-04: "failedfiles" now returns a file/error tibble rather than a bare
# character vector, so parsePreprocessingBatchSummary() can expose each
# failed file's actual captured error message alongside its filepath.
test_that("parsePreprocessingBatchSummary returns a file/error tibble with error detail for info_to_extract = 'failedfiles'", {
  dir <- tempfile("p107_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  successful <- c(
    "/data/sub-01_task-test_recording-eyetracking_physio.tsv",
    "/data/sub-02_task-test_recording-eyetracking_physio.tsv"
  )
  failed <- c("/data/sub-03_task-test_recording-eyetracking_physio.tsv")
  failed_errors <- c("object 'x' not found")

  f <- write_batch_summary_fixture(
    dir,
    successful = successful,
    failed = failed,
    failed_errors = failed_errors
  )

  result <- parsePreprocessingBatchSummary(f, "failedfiles")

  expect_s3_class(result, "data.frame")
  expect_named(result, c("file", "error"))
  expect_equal(result$file, failed)
  expect_equal(result$error, failed_errors)
})

# P7-04: a caught error's conditionMessage() can itself contain embedded
# newlines (e.g. a multi-sentence message, or one that embeds a nested
# condition's text). eyeQualityBatch() runs every failed file's message
# through R/eyeQualityBatch.R's sanitize_error_message_for_summary() (an
# internal, @noRd function -- accessed here via ':::') before writing it,
# specifically because parsePreprocessingBatchSummary()'s "failedfiles"
# branch above parses that section as a fixed 2-lines-per-entry structure (a
# filepath line, then one indented "  error: ..." line); an unsanitized
# multi-line message would silently corrupt that structure by inserting
# extra lines. This confirms both that the sanitizer actually collapses
# embedded newlines/carriage-returns to a single space (rather than, say,
# dropping them and running words together), and that the collapsed result
# round-trips intact through the batch summary file and back out via
# parsePreprocessingBatchSummary().
test_that("a multi-line error message is collapsed to one line by sanitize_error_message_for_summary() and round-trips intact through the batch summary file", {
  dir <- tempfile("p704_multiline_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  raw_message <- "Error in foo():\n  bad thing happened\r\n  additional detail on a third line"
  sanitized <- eyeQuality:::sanitize_error_message_for_summary(raw_message)

  expect_false(grepl("[\r\n]", sanitized))
  expect_equal(
    sanitized,
    "Error in foo():   bad thing happened   additional detail on a third line"
  )

  failed <- c("/data/sub-01_task-test_recording-eyetracking_physio.tsv")

  f <- write_batch_summary_fixture(
    dir,
    successful = character(0),
    failed = failed,
    failed_errors = sanitized
  )

  result <- parsePreprocessingBatchSummary(f, "failedfiles")

  expect_equal(nrow(result), 1)
  expect_equal(result$file, failed)
  expect_equal(result$error, sanitized)
  expect_false(grepl("[\r\n]", result$error))
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

test_that("parsePreprocessingBatchSummary returns a zero-row file/error tibble for an empty failedfiles section (n = 0)", {
  dir <- tempfile("p107_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  f <- write_batch_summary_fixture(
    dir,
    successful = c("/data/sub-01_task-test_recording-eyetracking_physio.tsv"),
    failed = character(0)
  )

  result <- parsePreprocessingBatchSummary(f, "failedfiles")

  expect_equal(result, tibble::tibble(file = character(0), error = character(0)))
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
