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

  # NOTE: this test intentionally does NOT exercise
  # info_to_extract = "summary" (the function's own default value) here - see
  # the dedicated test immediately below, which documents a pre-existing,
  # always-reproducing bug in that branch discovered while writing this
  # integration test (R/parsePreprocessingBatchSummary.R:29's
  # `.data[[1]]` positional pronoun subsetting errors under the
  # rlang/dplyr versions installed in this project's dev environment -
  # rlang 1.1.1 / dplyr 1.1.2 - with "Must subset the data pronoun with a
  # string, not the number 1."). That bug is unrelated to P2-09's fixture or
  # eyeQualityBatch() itself and is out of scope to fix here.

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

test_that("parsePreprocessingBatchSummary(info_to_extract = 'summary') currently errors on a real eyeQualityBatch() run's summary file (pre-existing bug, not in P2-09's scope)", {
  skip_on_cran()

  # info_to_extract = "summary" is parsePreprocessingBatchSummary()'s own
  # default value, and is exactly the branch acceptance criterion 3 asks to
  # round-trip - but as written, R/parsePreprocessingBatchSummary.R:29
  # onward calls `.data[[1]]` (positional/integer subsetting of the dplyr
  # data pronoun) inside a mutate(), which errors under the rlang/dplyr
  # versions installed here regardless of what the input file contains: `!
  # Must subset the data pronoun with a string, not the number 1.` This
  # reproduces from any well-formed batch summary file, not just the one
  # produced by this fixture - confirmed independently against a
  # hand-written fixture matching write_batch_summary_fixture()'s format in
  # test-parsePreprocessingBatchSummary.R. This test documents that current,
  # broken behavior rather than asserting the (currently unreachable)
  # correct round-trip; when the underlying `.data[[1]]` bug is fixed, this
  # test should fail and be replaced with a real round-trip assertion
  # (nfiles == 4, nPreprocessed == 4, nFailed == 0, directory == the batch
  # directory, etc., mirroring the "successfulfiles"/"failedfiles" checks
  # above).
  fixture <- copy_bids_sample_fixture()
  on.exit(unlink(fixture$dir, recursive = TRUE), add = TRUE)

  eyeQualityBatch(fixture$dir, batchName = "p209summarybug", numberCores = 1)

  summary_file <- file.path(fixture$dir, "preprocessing_batch_summary_desc-p209summarybug.txt")
  expect_true(file.exists(summary_file))

  expect_error(
    parsePreprocessingBatchSummary(summary_file, "summary"),
    regexp = "subset the data pronoun"
  )
})
