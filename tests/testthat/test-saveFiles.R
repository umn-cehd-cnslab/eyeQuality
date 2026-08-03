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
