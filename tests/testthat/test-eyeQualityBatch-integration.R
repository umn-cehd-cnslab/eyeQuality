# P2-09: batch integration tests exercising eyeQualityBatch() against a real,
# checked-in 2-subject x 2-session BIDS-like directory tree
# (tests/testthat/fixtures/bids_sample/, sub-01/ses-01, sub-01/ses-02,
# sub-02/ses-01, sub-02/ses-02), plus numberCores edge cases at the actual
# multi-file parallel-run level and a round-trip check of
# parsePreprocessingBatchSummary() against real eyeQualityBatch() output.
#
# eyeQualityBatch() writes a "derivatives/eyeQuality-v1/" folder into every
# session directory it processes and a batch summary file at the root of
# directoryBIDS, so every test below copies the checked-in fixture tree into
# a disposable tempdir before running the batch, leaving the git-tracked
# fixture untouched (same convention used by every other saveData = TRUE /
# eyeQualityBatch() test in this suite - see write_p1_01_fixture() and its
# tempfile()-based tests in test-eyeQualityBatch.R).

# Copies the checked-in bids_sample/ fixture tree into a fresh tempdir and
# returns that tempdir's path. Returns the four raw recording file paths as
# well (sorted by subject/session) for callers that need to compute expected
# output filenames.
copy_bids_sample_fixture <- function() {
  src <- testthat::test_path("fixtures", "bids_sample")
  dest <- tempfile("p209_bids_")
  fs::dir_copy(src, dest)

  raw_files <- sort(list.files(dest, pattern = "\\.tsv$", recursive = TRUE, full.names = TRUE))

  list(dir = dest, raw_files = raw_files)
}

test_that("eyeQualityBatch writes one *_preproc_qcsummary.tsv per recording across a 2-subject x 2-session BIDS directory", {
  skip_on_cran()

  fixture <- copy_bids_sample_fixture()
  on.exit(unlink(fixture$dir, recursive = TRUE), add = TRUE)

  expect_length(fixture$raw_files, 4)

  eyeQualityBatch(fixture$dir, batchName = "p209full", numberCores = 1)

  expected_outputs <- vapply(
    fixture$raw_files,
    function(fp) as.character(create_new_filename(fp, "_desc-p209full_preproc_qcsummary", ".tsv")),
    character(1)
  )

  for (out in expected_outputs) {
    expect_true(file.exists(out))
  }

  qcsummary_files <- list.files(
    fixture$dir,
    pattern = "_desc-p209full_preproc_qcsummary\\.tsv$",
    recursive = TRUE,
    full.names = TRUE
  )

  # exactly one output file per input recording - not fewer (a silently
  # dropped subject/session) and not more (e.g. double-processing a file)
  expect_length(qcsummary_files, 4)
  expect_setequal(normalizePath(qcsummary_files), normalizePath(expected_outputs))
})

test_that("eyeQualityBatch uses 1 core for a single-file batch even with numberCores left at its NULL/auto-detect default", {
  skip_on_cran()

  # A single-recording BIDS directory (one subject, one session) built from
  # just the sub-01/ses-01 fixture. With numberCores = NULL, eyeQualityBatch()
  # computes numcores as min(floor(detectCores() * 0.85), n files) subject to
  # a floor of 1 - for exactly 1 file, that expression evaluates to 1
  # regardless of how many cores the host machine actually has (this is what
  # makes the assertion below deterministic across CI/dev machines rather
  # than tied to a specific detectCores() value).
  src_file <- testthat::test_path(
    "fixtures", "bids_sample", "sub-01", "ses-01",
    "sub-01_ses-01_task-test_recording-eyetracking_physio.tsv"
  )

  dir <- tempfile("p209_onefile_")
  session_dir <- file.path(dir, "sub-01", "ses-01")
  dir.create(session_dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  dest_file <- file.path(session_dir, basename(src_file))
  file.copy(src_file, dest_file)

  eyeQualityBatch(dir, batchName = "p209onefile", numberCores = NULL)

  summary_file <- file.path(dir, "preprocessing_batch_summary_desc-p209onefile.txt")
  expect_true(file.exists(summary_file))

  lines <- readr::read_lines(summary_file)
  cores_line <- lines[grepl("^number of cores", lines)]

  expect_equal(cores_line, "number of cores = 1")

  out <- create_new_filename(dest_file, "_desc-p209onefile_preproc_qcsummary", ".tsv")
  expect_true(file.exists(out))
})

test_that("eyeQualityBatch falls back to 1 core (not 0 or a negative count) when detectCores() itself returns 0", {
  skip_on_cran()

  # R/eyeQualityBatch.R's auto-detect branch computes
  # floor(detectCores() * 0.85), then explicitly floors that at 1 core via
  # `ifelse(floor(detectCores() * 0.85) <= 0, 1, ...)`. This is unreachable
  # through the numberCores argument itself (numberCores < 1 is rejected by
  # the input-validation guard covered in test-eyeQualityBatch.R's P1-02
  # tests), so the only way to actually exercise it is a degenerate
  # detectCores() return value - mocked here rather than relying on a real
  # machine happening to report 0 or 1 cores.
  local_mocked_bindings(detectCores = function(...) 0)

  fixture <- copy_bids_sample_fixture()
  on.exit(unlink(fixture$dir, recursive = TRUE), add = TRUE)

  eyeQualityBatch(fixture$dir, batchName = "p209zerocores", numberCores = NULL)

  summary_file <- file.path(fixture$dir, "preprocessing_batch_summary_desc-p209zerocores.txt")
  lines <- readr::read_lines(summary_file)
  cores_line <- lines[grepl("^number of cores", lines)]

  expect_equal(cores_line, "number of cores = 1")

  # confirm the degenerate core count didn't stop the batch from actually
  # completing - all four recordings should still have been processed on the
  # single fallback core
  qcsummary_files <- list.files(
    fixture$dir,
    pattern = "_desc-p209zerocores_preproc_qcsummary\\.tsv$",
    recursive = TRUE,
    full.names = TRUE
  )
  expect_length(qcsummary_files, 4)
})

test_that("parsePreprocessingBatchSummary() round-trips a real eyeQualityBatch() run's summary, successful files, and failed files", {
  skip_on_cran()

  fixture <- copy_bids_sample_fixture()
  on.exit(unlink(fixture$dir, recursive = TRUE), add = TRUE)

  eyeQualityBatch(fixture$dir, batchName = "p209roundtrip", numberCores = 1)

  summary_file <- file.path(fixture$dir, "preprocessing_batch_summary_desc-p209roundtrip.txt")
  expect_true(file.exists(summary_file))

  # "summary" section is covered separately below (P1-15 fixed
  # R/parsePreprocessingBatchSummary.R:29's `.data[[1]]` positional pronoun
  # subsetting, which previously errored under this project's rlang/dplyr
  # versions).

  # "successfulfiles" section - eyeQualityBatch() writes the *_qcsummary.tsv
  # output paths (not the raw input paths) into this section, so the
  # expected list is recomputed from the four raw fixture files the same way
  # eyeQualityBatch() itself locates completed output files.
  expected_qcsummary_files <- list.files(
    fixture$dir,
    pattern = "_desc-p209roundtrip_preproc_qcsummary\\.tsv$",
    recursive = TRUE,
    full.names = TRUE
  )
  expect_length(expected_qcsummary_files, 4)

  successful_result <- parsePreprocessingBatchSummary(summary_file, "successfulfiles")
  expect_setequal(normalizePath(successful_result), normalizePath(expected_qcsummary_files))

  # "failedfiles" section - every recording succeeded, so this should
  # round-trip to an empty character vector
  failed_result <- parsePreprocessingBatchSummary(summary_file, "failedfiles")
  expect_equal(failed_result, character(0))
})

test_that("parsePreprocessingBatchSummary(info_to_extract = 'summary') round-trips a real eyeQualityBatch() run's summary file", {
  skip_on_cran()

  fixture <- copy_bids_sample_fixture()
  on.exit(unlink(fixture$dir, recursive = TRUE), add = TRUE)

  eyeQualityBatch(fixture$dir, batchName = "p209summary", numberCores = 1)

  summary_file <- file.path(fixture$dir, "preprocessing_batch_summary_desc-p209summary.txt")
  expect_true(file.exists(summary_file))

  summary_result <- parsePreprocessingBatchSummary(summary_file, "summary")

  expect_equal(summary_result$nfiles, 4)
  expect_equal(summary_result$nPreprocessed, 4)
  expect_equal(summary_result$nFailed, 0)
  expect_equal(summary_result$directory, fixture$dir)
})

# P7-03 regression: parsePreprocessingBatchSummary()'s "successfulfiles" /
# "failedfiles" / "skippedfiles" branches used to take the FIRST matching
# section header (header_idx[1]) rather than the last. eyeQualityBatch()
# appends to its batch_run_summary text file rather than overwriting it, so
# calling it more than once against the same directory/batchName - exactly
# P7-03's resumability scenario - leaves multiple occurrences of each
# section header in the file, one per run. Taking the first occurrence
# silently returns stale data from an earlier run rather than what's
# actually true on disk after the latest run. Fixed to take the last match.
#
# Same nested sub-XX/ses-XX fixture layout as write_p703_fixture() in
# test-eyeQualityBatch.R, for the same reason documented there: a flat,
# non-nested raw file would make the second eyeQualityBatch() call against
# the same directory silently discover zero candidate files, since the
# "derivatives" folder the first call creates as a sibling of a flat raw
# file confuses listBidsFiles()'s subject-directory detection.
write_p703_regression_fixture <- function(dir, subject) {
  n <- 200
  dt_ms <- 17
  ts <- seq(0, by = dt_ms, length.out = n)

  d <- data.frame(
    "Recording software version" = rep("1.90.0", n),
    "Sensor" = rep("Eye Tracker", n),
    "Event" = rep(NA_character_, n),
    "Event value" = rep(NA_character_, n),
    "Recording duration" = rep(NA_real_, n),
    "Recording resolution height" = rep(1080, n),
    "Recording resolution width" = rep(1920, n),
    "Eyetracker timestamp" = ts,
    "Recording timestamp" = ts,
    "Gaze point left X" = rep(1400, n),
    "Gaze point left Y" = rep(800, n),
    "Gaze point right X" = rep(1400, n),
    "Gaze point right Y" = rep(800, n),
    "Eye position left Z (DACSmm)" = rep(600, n),
    "Eye position right Z (DACSmm)" = rep(600, n),
    "Pupil diameter left" = rep(3.5, n),
    "Pupil diameter right" = rep(3.5, n),
    "Validity left" = rep("Valid", n),
    "Validity right" = rep("Valid", n),
    check.names = FALSE
  )

  session_dir <- file.path(dir, subject, "ses-01")
  dir.create(session_dir, recursive = TRUE, showWarnings = FALSE)
  filepath <- file.path(
    session_dir,
    paste0(subject, "_ses-01_task-test_recording-eyetracking_physio.tsv")
  )
  readr::write_tsv(d, filepath)
  filepath
}

test_that("parsePreprocessingBatchSummary reflects the latest of two eyeQualityBatch() runs against the same directory/batchName, not the first (header_idx regression)", {
  skip_on_cran()

  dir <- tempfile("p703_regression_")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  fp1 <- write_p703_regression_fixture(dir, "sub-01")

  # first run: only fp1 present. Its "successfulfiles" section reports
  # n = 1 (fp1's output); its "skippedfiles" section reports n = 0 (nothing
  # to skip yet).
  eyeQualityBatch(dir, batchName = "p703regress", numberCores = 1)

  fp2 <- write_p703_regression_fixture(dir, "sub-02")

  # second run: fp1 already has a qcsummary output and is skipped; fp2 is
  # new and gets processed. This run's "successfulfiles" section (which
  # reports every candidate file with a qcsummary output, not just what was
  # dispatched this run) now reports n = 2, and its "skippedfiles" section
  # reports n = 1 (fp1) - both counts differ from the first run's sections,
  # which is what makes this fixture able to detect a header_idx[1]
  # regression: taking the first match would silently return the first
  # run's stale n = 1 / n = 0 sections instead.
  eyeQualityBatch(dir, batchName = "p703regress", numberCores = 1)

  summary_file <- file.path(dir, "preprocessing_batch_summary_desc-p703regress.txt")
  lines <- readr::read_lines(summary_file)

  successful_header_idx <- grep("^------ Successfully processed files", lines)
  expect_length(successful_header_idx, 2)
  expect_match(lines[successful_header_idx[1]], "n = 1\\)", fixed = FALSE)
  expect_match(lines[successful_header_idx[2]], "n = 2\\)", fixed = FALSE)

  skipped_header_idx <- grep("^------ Skipped files", lines)
  expect_length(skipped_header_idx, 2)
  expect_match(lines[skipped_header_idx[1]], "n = 0\\)", fixed = FALSE)
  expect_match(lines[skipped_header_idx[2]], "n = 1\\)", fixed = FALSE)

  out1 <- create_new_filename(fp1, "_desc-p703regress_preproc_qcsummary", ".tsv")
  out2 <- create_new_filename(fp2, "_desc-p703regress_preproc_qcsummary", ".tsv")

  # if header_idx[1] regressed, this would return only out1
  successful_result <- parsePreprocessingBatchSummary(summary_file, "successfulfiles")
  expect_setequal(normalizePath(successful_result), normalizePath(c(out1, out2)))

  # if header_idx[1] regressed, this would return character(0)
  skipped_result <- parsePreprocessingBatchSummary(summary_file, "skippedfiles")
  expect_equal(normalizePath(skipped_result), normalizePath(fp1))
})
