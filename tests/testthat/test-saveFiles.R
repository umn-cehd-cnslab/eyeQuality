# Regression tests for P1-06: eyeQuality() gains an `outputDir = NULL`
# parameter threaded through saveFiles() to create_new_filename(). When NULL
# (the default), output location is unchanged
# (`<input_dir>/derivatives/eyeQuality-v1/`); when supplied, output is written
# directly under that directory instead (no nested derivatives/eyeQuality-v1
# subpath), with the directory created if it doesn't already exist.

# Same minimal single-fixation TobiiPro-format fixture shape as
# write_p1_01_fixture() in test-eyeQualityBatch.R: one participant staring at
# one fixed, off-center screen location for the whole recording, so the file
# passes cleanly through the full eyeQuality() pipeline without hitting any
# edge cases. All that matters here is that the pipeline completes and
# saveData = TRUE actually writes files, not the specific gaze values.
write_saveFiles_fixture <- function(dir, filename = "sub-01_task-test_recording-eyetracking_physio.tsv") {
  n <- 50
  dt_ms <- 17 # ~58.8 Hz, arbitrary but realistic
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

  filepath <- file.path(dir, filename)
  readr::write_tsv(d, filepath)
  filepath
}

## ---- create_new_filename() unit tests ----------------------------------

test_that("create_new_filename() with outputDir = NULL preserves the default derivatives/eyeQuality-v1 location", {
  base_dir <- tempfile("p106_unit_default_")
  dir.create(base_dir)
  on.exit(unlink(base_dir, recursive = TRUE), add = TRUE)

  inputfile <- file.path(base_dir, "sub-01_task-test_physio.tsv")

  result <- create_new_filename(inputfile, "_desc-preproc", ".tsv")

  expected <- fs::path(base_dir, "derivatives", "eyeQuality-v1", "sub-01_task-test_physio_desc-preproc.tsv")
  expect_equal(fs::path(result), expected)
  # the default derivatives directory should have been created as a side effect
  expect_true(fs::dir_exists(fs::path(base_dir, "derivatives", "eyeQuality-v1")))
})

test_that("create_new_filename() with outputDir = <path> writes directly under that directory, not nested under derivatives/eyeQuality-v1", {
  base_dir <- tempfile("p106_unit_input_")
  out_dir <- tempfile("p106_unit_output_")
  dir.create(base_dir)
  on.exit(unlink(c(base_dir, out_dir), recursive = TRUE), add = TRUE)

  inputfile <- file.path(base_dir, "sub-01_task-test_physio.tsv")

  # out_dir does not exist yet -- create_new_filename() must create it
  expect_false(fs::dir_exists(out_dir))

  result <- create_new_filename(inputfile, "_desc-preproc", ".tsv", outputDir = out_dir)

  expected <- fs::path(out_dir, "sub-01_task-test_physio_desc-preproc.tsv")
  expect_equal(fs::path(result), expected)

  # the outputDir itself was created...
  expect_true(fs::dir_exists(out_dir))
  # ...and no derivatives/eyeQuality-v1 subdirectory was nested inside it, and
  # none was created next to the (untouched) input file either -- this is the
  # core regression this test protects against: outputDir being treated as
  # just another "base directory" that still gets a derivatives/eyeQuality-v1
  # subpath appended.
  expect_false(fs::dir_exists(fs::path(out_dir, "derivatives")))
  expect_false(fs::dir_exists(fs::path(base_dir, "derivatives")))
})

## ---- eyeQuality() end-to-end tests --------------------------------------

# Both tests below pass an explicit batchName. This is NOT testing an
# unrelated behavior for its own sake: saveFiles()'s
# `ifelse(is.null(batchName), NULL, paste0(batchName, "_"))` pattern throws
# "replacement has length zero" whenever batchName is left at its documented
# NULL default (base::ifelse(TRUE, NULL, x) always errors this way -- this
# reproduces in plain `Rscript --vanilla` with no packages loaded, so it is
# not a version/environment artifact). That bug predates and is unrelated to
# P1-06's outputDir plumbing (confirmed via git log -p on R/saveFiles.R -- the
# ifelse() lines are untouched by the outputDir change), but it does mean
# eyeQuality(saveData = TRUE) with the literal batchName = NULL default
# currently cannot complete at all. Supplying batchName here (matching every
# other saveData = TRUE test already in this suite, e.g.
# test-eyeQualityBatch.R) routes around that separate, pre-existing bug so
# these tests can isolate and verify what P1-06 actually changed: where
# create_new_filename() writes files based on outputDir.

test_that("eyeQuality(saveData = TRUE, outputDir = <path>) writes outputs under that directory instead of <input_dir>/derivatives/eyeQuality-v1/", {
  skip_on_cran()

  input_dir <- tempfile("p106_e2e_input_")
  out_dir <- tempfile("p106_e2e_output_")
  dir.create(input_dir)
  on.exit(unlink(c(input_dir, out_dir), recursive = TRUE), add = TRUE)

  fp <- write_saveFiles_fixture(input_dir)

  eyeQuality(
    fp,
    displayDimensionX_mm = 594,
    displayDimensionY_mm = 344,
    saveData = TRUE,
    batchName = "p106custom",
    outputDir = out_dir
  )
  sinkReset()

  preproc_out <- create_new_filename(fp, "_desc-p106custom_preproc", ".tsv", outputDir = out_dir)

  # Compute what the default (no outputDir) output path *would* have been,
  # without calling create_new_filename() itself -- that function has the
  # side effect of unconditionally fs::dir_create()-ing its target directory,
  # which would falsely create <input_dir>/derivatives/eyeQuality-v1 here and
  # invalidate the very check below that asserts it was never created. Build
  # the expected path directly instead, mirroring create_new_filename()'s
  # naming logic (basename with extension stripped, plus appendname, plus
  # extension) without invoking it.
  default_out <- fs::path(
    input_dir, "derivatives", "eyeQuality-v1",
    paste0(basename(fs::path_ext_remove(fp)), "_desc-p106custom_preproc.tsv")
  )

  expect_true(file.exists(preproc_out))
  # the default location must NOT have been written to at all
  expect_false(file.exists(default_out))
  expect_false(fs::dir_exists(fs::path(input_dir, "derivatives")))
})

test_that("eyeQuality(saveData = TRUE) with no outputDir still writes to the original <input_dir>/derivatives/eyeQuality-v1/ location", {
  skip_on_cran()

  input_dir <- tempfile("p106_e2e_default_")
  dir.create(input_dir)
  on.exit(unlink(input_dir, recursive = TRUE), add = TRUE)

  fp <- write_saveFiles_fixture(input_dir)

  eyeQuality(
    fp,
    displayDimensionX_mm = 594,
    displayDimensionY_mm = 344,
    saveData = TRUE,
    batchName = "p106default"
  )
  sinkReset()

  default_out <- create_new_filename(fp, "_desc-p106default_preproc", ".tsv")

  expect_true(file.exists(default_out))
  expect_equal(
    fs::path(default_out),
    fs::path(input_dir, "derivatives", "eyeQuality-v1", "sub-01_task-test_recording-eyetracking_physio_desc-p106default_preproc.tsv")
  )
})

## ---- P1-12 regression tests: batchName left at its NULL default ---------

# saveFiles()'s four `_desc-` descriptor lines used to build the batch-label
# infix with `ifelse(is.null(batchName), NULL, paste0(batchName, "_"))`.
# base::ifelse() cannot return NULL for a branch, so this threw "replacement
# has length zero" whenever batchName was left at its documented NULL
# default -- i.e. the plainest, most default eyeQuality(saveData = TRUE) call
# could never complete. This block confirms that call now succeeds, and that
# the resulting filenames have no batch-label infix at all (e.g.
# "..._desc-preproc.tsv", not "..._desc-NULL_preproc.tsv" or
# "..._desc-_preproc.tsv").

test_that("saveFiles() with batchName = NULL does not error and writes files without a batch-label infix", {
  skip_on_cran()

  input_dir <- tempfile("p112_unit_input_")
  dir.create(input_dir)
  on.exit(unlink(input_dir, recursive = TRUE), add = TRUE)

  inputFile <- file.path(input_dir, "sub-01_task-test_physio.tsv")

  minimal_data <- data.frame(x = 1:3, y = 4:6)
  minimal_events <- data.frame(event = c("start", "end"))
  minimal_timing <- list(step1 = 0.1, step2 = 0.2)
  minimal_summary <- data.frame(metric = c("n_rows", "pct_valid"), value = c(3, 1))

  expect_no_error(
    saveFiles(
      inputFile,
      minimal_data,
      minimal_events,
      minimal_timing,
      minimal_summary,
      batchName = NULL
    )
  )

  expected_preproc <- create_new_filename(inputFile, "_desc-preproc", ".tsv")
  expected_events <- create_new_filename(inputFile, "_desc-events", ".tsv")
  expected_runtimes <- create_new_filename(inputFile, "_desc-preproc_runtimes", ".tsv")
  expected_qcsummary <- create_new_filename(inputFile, "_desc-preproc_qcsummary", ".tsv")

  expect_true(file.exists(expected_preproc))
  expect_true(file.exists(expected_events))
  expect_true(file.exists(expected_runtimes))
  expect_true(file.exists(expected_qcsummary))

  # none of the written filenames should contain a batch-label infix
  # (e.g. "_desc-NULL_" or a stray "_desc-_") anywhere in the basename
  written <- basename(c(expected_preproc, expected_events, expected_runtimes, expected_qcsummary))
  expect_false(any(grepl("_desc-NULL_", written)))
  expect_false(any(grepl("_desc-_", written)))
})

test_that("eyeQuality(saveData = TRUE) with no batchName argument succeeds and writes output files without the batch-label infix", {
  skip_on_cran()

  input_dir <- tempfile("p112_e2e_input_")
  dir.create(input_dir)
  on.exit(unlink(input_dir, recursive = TRUE), add = TRUE)

  fp <- write_saveFiles_fixture(input_dir)

  expect_no_error(
    eyeQuality(
      fp,
      displayDimensionX_mm = 594,
      displayDimensionY_mm = 344,
      saveData = TRUE
    )
  )
  sinkReset()

  preproc_out <- create_new_filename(fp, "_desc-preproc", ".tsv")

  expect_true(file.exists(preproc_out))
  expect_equal(
    fs::path(preproc_out),
    fs::path(
      input_dir, "derivatives", "eyeQuality-v1",
      "sub-01_task-test_recording-eyetracking_physio_desc-preproc.tsv"
    )
  )
  # no batch-label infix should appear anywhere in the written filename
  expect_false(grepl("_desc-NULL_", basename(preproc_out)))
})

## ---- P1-13 regression tests: create_new_filename() honoring newFileExtension ----

# create_new_filename() used to set file_extension from the *input* file's own
# extension via fs::path_ext(inputfile), then only override it with
# newFileExtension when newFileExtension did NOT start with a dot:
#   if (!grepl("^\\.", newFileExtension)) { file_extension <- paste0(".", newFileExtension) }
# Every real call site passes a dot-prefixed value (".txt", ".tsv"), so the
# override never fired and the input file's own extension was silently kept
# -- most visibly, the run-log call in eyeQuality() requests ".txt" on a
# ".tsv" input and got ".tsv" back instead. A second, latent bug: even when
# the override DID fire (non-dot-prefixed newFileExtension), the old code
# produced a leading-dot value ("." + "txt") that combined with
# fs::path(ext = ...)'s own dot-prepending to build a double-dot filename
# like "bar..txt". The fix normalizes unconditionally:
# `file_extension <- sub("^\\.", "", newFileExtension)`.

test_that("create_new_filename() overrides a dot-prefixed newFileExtension that differs from the input's own extension", {
  base_dir <- tempfile("p113_dotprefixed_")
  dir.create(base_dir)
  on.exit(unlink(base_dir, recursive = TRUE), add = TRUE)

  inputfile <- file.path(base_dir, "foo.tsv")

  result <- create_new_filename(inputfile, "_desc-runlog", ".txt")

  expect_true(grepl("\\.txt$", result))
  expect_false(grepl("\\.tsv$", result))
})

test_that("create_new_filename() with a non-dot-prefixed newFileExtension does not produce a double-dot extension", {
  base_dir <- tempfile("p113_nodot_")
  dir.create(base_dir)
  on.exit(unlink(base_dir, recursive = TRUE), add = TRUE)

  inputfile <- file.path(base_dir, "foo.tsv")

  result <- create_new_filename(inputfile, "_desc-x", "txt")

  expect_true(grepl("\\.txt$", result))
  expect_false(grepl("\\.\\.txt$", result))
})

test_that("create_new_filename() with newFileExtension omitted still preserves the input file's own extension", {
  base_dir <- tempfile("p113_omitted_")
  dir.create(base_dir)
  on.exit(unlink(base_dir, recursive = TRUE), add = TRUE)

  inputfile <- file.path(base_dir, "foo.tsv")

  result <- create_new_filename(inputfile, "_desc-x")

  expect_true(grepl("\\.tsv$", result))
})

test_that("eyeQuality(saveData = TRUE) writes its run log with a .txt extension, not the input file's .tsv extension", {
  skip_on_cran()

  input_dir <- tempfile("p113_e2e_input_")
  dir.create(input_dir)
  on.exit(unlink(input_dir, recursive = TRUE), add = TRUE)

  fp <- write_saveFiles_fixture(input_dir)

  eyeQuality(
    fp,
    displayDimensionX_mm = 594,
    displayDimensionY_mm = 344,
    saveData = TRUE,
    batchName = "p113runlog"
  )
  sinkReset()

  # Mirrors the run-log path construction in eyeQuality.R exactly.
  runlog_txt <- create_new_filename(fp, "_desc-p113runlog_preproc_runlog", ".txt")
  runlog_wrong_tsv <- create_new_filename(fp, "_desc-p113runlog_preproc_runlog", ".tsv")

  expect_true(file.exists(runlog_txt))
  expect_false(file.exists(runlog_wrong_tsv))
})

test_that("no ifelse(is.null(batchName)... pattern survives anywhere in R/", {
  # P1-12's acceptance criteria (2026-08-03 update) explicitly includes:
  # `grep -rn "ifelse(is.null(batchName)" R/` returns no matches anywhere in
  # the repo. This is a repo-wide guard against any of the four fixed sites
  # (R/saveFiles.R x4, R/eyeQuality.R:68, R/eyeQualityBatch.R:169,
  # R/getFileRunLogName.R:26) regressing back to the broken
  # `ifelse(is.null(batchName), NULL, paste0(batchName, "_"))` pattern, and
  # against the same broken pattern being reintroduced anywhere new.
  pkg_root <- testthat::test_path("..", "..")
  r_dir <- file.path(pkg_root, "R")
  skip_if_not(dir.exists(r_dir), "R/ directory not found relative to test file")

  r_files <- list.files(r_dir, pattern = "\\.[Rr]$", full.names = TRUE)
  hits <- unlist(lapply(r_files, function(f) {
    lines <- readLines(f, warn = FALSE)
    matched <- grep("ifelse(is.null(batchName)", lines, fixed = TRUE, value = TRUE)
    if (length(matched) > 0) paste0(basename(f), ": ", matched) else character(0)
  }))

  expect_length(hits, 0)
})
