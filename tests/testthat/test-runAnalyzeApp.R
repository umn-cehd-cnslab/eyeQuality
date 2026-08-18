# P10-01: regression tests for the Analyze / QC Explorer app's Shiny-free
# helpers (inst/shiny-apps/analyze/helpers.R) -- discover_qcsummary_files(),
# derive_recording_label(), derive_batch_name(), build_zero_match_diagnostic(),
# read_one_qcsummary(), and load_qcsummary_table(). Sourced via system.file()
# rather than a relative path, same convention as test-runSetupApp.R, both
# because that's the only way to reach inst/ files portably from tests and
# because it doubles as a check that the app is packaged where
# runAnalyzeApp() (R/runAnalyzeApp.R) expects to find it.
#
# Exercised against real eyeQualityBatch() output (both the default nested
# derivatives/eyeQuality-v1/ layout and a centralized outputDir) for the
# discovery/combination paths, plus hand-built synthetic qcsummary.tsv files
# for the column-mismatch and corrupted-file edge cases that would be slow
# or unreliable to reproduce through a real batch run.

helpers_path <- system.file("shiny-apps", "analyze", "helpers.R", package = "eyeQuality")
if (!nzchar(helpers_path)) {
  stop("test-runAnalyzeApp.R: could not locate inst/shiny-apps/analyze/helpers.R via system.file()")
}
source(helpers_path, local = TRUE)

# Copies the checked-in fixtures/bids/ tree (2 subjects x 2 sessions) into a
# fresh tempdir, same convention as copy_bids_sample_fixture() in
# test-eyeQualityBatch-integration.R -- duplicated locally (rather than
# reused across files) so this file has no load-order dependency on another
# test file having already sourced it.
copy_bids_fixture_tree <- function() {
  src <- testthat::test_path("fixtures", "bids")
  dest <- tempfile("p1001_bids_")
  fs::dir_copy(src, dest)
  dest
}

# ---------------------------------------------------------------------------
# discover_qcsummary_files()
# ---------------------------------------------------------------------------

test_that("discover_qcsummary_files finds every output under a real batch run's default nested derivatives/eyeQuality-v1/ layout", {
  skip_on_cran()

  dir <- copy_bids_fixture_tree()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  eyeQualityBatch(dir, batchName = "p1001nested", numberCores = 1)

  found <- discover_qcsummary_files(dir, recursive = TRUE)
  expect_length(found, 4)
  expect_true(all(grepl("derivatives.eyeQuality-v1", found)))
  expect_true(all(grepl("_desc-p1001nested_preproc_qcsummary\\.tsv$", found)))
})

test_that("discover_qcsummary_files finds every output under a centralized outputDir without needing recursion", {
  skip_on_cran()

  inputDir <- tempfile("p1001_centralized_in_")
  outDir <- tempfile("p1001_centralized_out_")
  dir.create(inputDir)
  dir.create(outDir)
  on.exit(unlink(c(inputDir, outDir), recursive = TRUE), add = TRUE)

  fs::dir_copy(testthat::test_path("fixtures", "bids"), file.path(inputDir, "bids"))

  eyeQualityBatch(file.path(inputDir, "bids"), batchName = "p1001central", numberCores = 1, outputDir = outDir)

  # outputDir writes every file flat, directly under outDir (no nested
  # per-subject derivatives/ subfolders -- see get_qcsummary_output_path() in
  # R/eyeQualityBatch.R), so a non-recursive search should already find all
  # four -- the "centralized outputDir" alternative to the default nested
  # layout that helpers.R's docs describe.
  found <- discover_qcsummary_files(outDir, recursive = FALSE)
  expect_length(found, 4)
  expect_false(dir.exists(file.path(outDir, "derivatives")))
  expect_true(all(normalizePath(dirname(found)) == normalizePath(outDir)))
})

test_that("discover_qcsummary_files finds, and derive_recording_label/derive_batch_name correctly parse, a real eyeQuality() single-file output where batchName is left NULL", {
  skip_on_cran()

  # eyeQuality()'s single-file (non-batch) entry point defaults batchName =
  # NULL, producing "<...>_desc-preproc_qcsummary.tsv" (no batchName segment,
  # no underscore directly before "preproc") rather than
  # eyeQualityBatch()'s "<...>_desc-<batchName>_preproc_qcsummary.tsv" form.
  # A pattern requiring a literal underscore before "preproc" (the pre-fix
  # bug) matches the batchName-present form but silently misses this one --
  # this test exercises that specific gap end to end against a real
  # eyeQuality() output, not a hand-built filename.
  out_dir <- tempfile("p1001_batchname_null_")
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  raw_file <- system.file("extdata", "tobii_studio_sample.tsv", package = "eyeQuality")
  eyeQuality(
    raw_file,
    displayDimensionX_mm = 594,
    displayDimensionY_mm = 344,
    saveData = TRUE,
    outputDir = out_dir
  )

  found <- discover_qcsummary_files(out_dir, recursive = FALSE)
  expect_length(found, 1)
  expect_match(found, "tobii_studio_sample_desc-preproc_qcsummary\\.tsv$")

  expect_equal(derive_recording_label(found), "tobii_studio_sample")
  expect_true(is.na(derive_batch_name(found)))
})

test_that("discover_qcsummary_files errors clearly when the directory does not exist", {
  missing_dir <- file.path(tempdir(), "p1001_does_not_exist_xyz")
  expect_error(discover_qcsummary_files(missing_dir), "does not exist")
})

test_that("discover_qcsummary_files errors clearly on an empty-string directory argument", {
  expect_error(discover_qcsummary_files(""), "non-empty single path")
})

# ---------------------------------------------------------------------------
# derive_recording_label() / derive_batch_name()
# ---------------------------------------------------------------------------

test_that("derive_recording_label strips the _desc-<batchName>_preproc_qcsummary.tsv suffix, leaving the recording identifier", {
  label <- derive_recording_label("sub-01_ses-1_recording-eyetracking_physio_desc-mybatch_preproc_qcsummary.tsv")
  expect_equal(label, "sub-01_ses-1_recording-eyetracking_physio")
})

test_that("derive_batch_name recovers the batchName from a qcsummary output's filename", {
  expect_equal(
    derive_batch_name("sub-01_ses-1_recording-eyetracking_physio_desc-mybatch_preproc_qcsummary.tsv"),
    "mybatch"
  )
})

test_that("derive_batch_name returns NA for the batchName == NULL naming form", {
  expect_true(is.na(derive_batch_name("sub-01_ses-1_recording-eyetracking_physio_desc-preproc_qcsummary.tsv")))
})

# ---------------------------------------------------------------------------
# build_zero_match_diagnostic()
# ---------------------------------------------------------------------------

test_that("build_zero_match_diagnostic reports 'no .tsv files at all' when the directory has none", {
  empty_dir <- tempfile("p1001_empty_")
  dir.create(empty_dir)
  on.exit(unlink(empty_dir, recursive = TRUE), add = TRUE)

  msg <- build_zero_match_diagnostic(empty_dir, recursive = TRUE)
  expect_match(msg, "No .tsv files of any kind", fixed = TRUE)
})

test_that("build_zero_match_diagnostic reports '.tsv files present but none matched' when unrelated .tsv files exist", {
  dir <- tempfile("p1001_unrelated_tsv_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  file.create(file.path(dir, "raw_recording.tsv"))
  file.create(file.path(dir, "another_file.tsv"))

  msg <- build_zero_match_diagnostic(dir, recursive = TRUE)
  expect_match(msg, "Found 2 .tsv file(s)", fixed = TRUE)
  expect_match(msg, "none matched the qcsummary output naming convention", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# load_qcsummary_table() -- zero-match propagation
# ---------------------------------------------------------------------------

test_that("load_qcsummary_table returns n_files = 0, a NULL table, and the zero-match diagnostic when nothing is found", {
  empty_dir <- tempfile("p1001_load_empty_")
  dir.create(empty_dir)
  on.exit(unlink(empty_dir, recursive = TRUE), add = TRUE)

  result <- load_qcsummary_table(empty_dir)

  expect_equal(result$n_files, 0L)
  expect_null(result$table)
  expect_match(result$diagnostic_message, "No .tsv files of any kind", fixed = TRUE)
  expect_length(result$read_errors, 0)
})

test_that("load_qcsummary_table's diagnostic distinguishes unrelated .tsv files from a real zero-match dry hole", {
  dir <- tempfile("p1001_load_unrelated_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  file.create(file.path(dir, "raw_recording.tsv"))

  result <- load_qcsummary_table(dir)

  expect_equal(result$n_files, 0L)
  expect_null(result$table)
  expect_match(result$diagnostic_message, "Found 1 .tsv file(s)", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# load_qcsummary_table() -- real multi-file combination
# ---------------------------------------------------------------------------

test_that("load_qcsummary_table combines a real 4-file batch run into one table with correct recording/batch_name/source_file columns", {
  skip_on_cran()

  dir <- copy_bids_fixture_tree()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  eyeQualityBatch(dir, batchName = "p1001combine", numberCores = 1)

  result <- load_qcsummary_table(dir, recursive = TRUE)

  expect_equal(result$n_files, 4L)
  expect_length(result$read_errors, 0)
  expect_null(result$diagnostic_message)

  # calculateOutputMetrics() produces one row per qc_metric (32 metric rows,
  # confirmed directly against R/calculateOutputMetrics.R's summary_df row
  # labels: 28 declared up front plus 4 more -- ivt_blinks and the three
  # robustness_* rows -- assigned dynamically further down), not one row per
  # file, so 4 files combine to 4 * 32 = 128 rows.
  expect_equal(nrow(result$table), 128)
  expect_equal(colnames(result$table)[1:3], c("recording", "batch_name", "source_file"))

  expect_equal(
    sort(unique(result$table$recording)),
    c(
      "sub-1_ses-1_recording-eyetracking_physio",
      "sub-1_ses-2_recording-eyetracking_physio",
      "sub-2_ses-1_recording-eyetracking_physio",
      "sub-2_ses-2_recording-eyetracking_physio"
    )
  )
  expect_true(all(result$table$batch_name == "p1001combine"))
  expect_length(unique(result$table$source_file), 4)

  # exactly one qc_metric row set (32 rows) per recording -- not merged
  # across files and not duplicated
  rows_per_recording <- table(result$table$recording)
  expect_true(all(rows_per_recording == 32))
})

# ---------------------------------------------------------------------------
# load_qcsummary_table() -- bind_rows() column-mismatch tolerance
# ---------------------------------------------------------------------------

test_that("load_qcsummary_table tolerates two qcsummary.tsv files with different column sets, NA-filling the missing column", {
  dir <- tempfile("p1001_mismatch_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  # file A: no extra_metric_col
  readr::write_tsv(
    data.frame(qc_metric = c("a", "b"), n = c(1, 2), percent = c(0.1, 0.2)),
    file.path(dir, "sub-a_ses-1_desc-mismatch_preproc_qcsummary.tsv")
  )
  # file B: has an extra column file A doesn't
  readr::write_tsv(
    data.frame(qc_metric = c("c", "d"), n = c(3, 4), percent = c(0.3, 0.4), extra_metric_col = c("x", "y")),
    file.path(dir, "sub-b_ses-1_desc-mismatch_preproc_qcsummary.tsv")
  )

  result <- load_qcsummary_table(dir)

  expect_equal(result$n_files, 2L)
  expect_length(result$read_errors, 0)
  expect_equal(nrow(result$table), 4)
  expect_true("extra_metric_col" %in% colnames(result$table))

  # rbind() would have errored outright on the mismatched column sets;
  # bind_rows() instead NA-fills extra_metric_col for file A's two rows
  a_rows <- result$table[result$table$recording == "sub-a_ses-1", ]
  b_rows <- result$table[result$table$recording == "sub-b_ses-1", ]
  expect_true(all(is.na(a_rows$extra_metric_col)))
  expect_equal(b_rows$extra_metric_col, c("x", "y"))
})

# ---------------------------------------------------------------------------
# load_qcsummary_table() -- corrupted/unreadable file handling
# ---------------------------------------------------------------------------

test_that("read_one_qcsummary raises an error for a file that can't be parsed", {
  missing_path <- file.path(tempdir(), "p1001_does_not_exist_desc-x_preproc_qcsummary.tsv")
  expect_error(read_one_qcsummary(missing_path))
})

test_that("load_qcsummary_table excludes an unreadable matched file from table and surfaces it in read_errors instead of aborting the whole load", {
  dir <- tempfile("p1001_corrupt_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  # A directory whose name matches the qcsummary naming convention: it's
  # picked up by discover_qcsummary_files() (a real filesystem entry, not a
  # phantom), but attempting to parse it as a delimited text file via
  # readr::read_tsv() reliably raises "cannot open the connection" -- a
  # deterministic, cross-run stand-in for a genuinely corrupted/truncated
  # qcsummary.tsv, which readr's own lenient TSV parser otherwise tolerates
  # (mismatched delimiters, embedded NULs, invalid UTF-8) without erroring.
  dir.create(file.path(dir, "sub-c_ses-1_desc-corrupt_preproc_qcsummary.tsv"))

  # a genuinely valid file alongside it, to confirm one bad file doesn't
  # take down the whole load
  readr::write_tsv(
    data.frame(qc_metric = c("a", "b"), n = c(1, 2)),
    file.path(dir, "sub-a_ses-1_desc-corrupt_preproc_qcsummary.tsv")
  )

  result <- suppressWarnings(load_qcsummary_table(dir, recursive = FALSE))

  expect_equal(result$n_files, 2L)
  expect_null(result$diagnostic_message)

  expect_length(result$read_errors, 1)
  expect_true(grepl("sub-c_ses-1_desc-corrupt_preproc_qcsummary\\.tsv$", names(result$read_errors)))

  # only the valid file's rows made it into the combined table
  expect_equal(nrow(result$table), 2)
  expect_true(all(result$table$recording == "sub-a_ses-1"))
})
