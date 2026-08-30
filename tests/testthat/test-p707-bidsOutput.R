# P7-07: opt-in BIDS-structured output layout, with optional raw-file copy.
#
# outputStructure = c("flat", "bids") (default "flat") on
# create_new_filename()/saveFiles()/eyeQuality()/eyeQualityBatch(), plus
# eyeQualityBatch()'s copyRawFile = FALSE. "flat" is byte-identical to this
# package's pre-P7-07 output layout; "bids" (only meaningful when outputDir
# is also set) parses sub-<label>/ses-<label> straight out of each input
# file's own basename (not its folder location) and writes derivative
# output to <outputDir>/derivatives/eyeQuality-v1/sub-<label>/ses-<label>/
# instead of flat, falling back to flat placement with a warning (not an
# error) for a non-BIDS-named file. copyRawFile additionally copies the
# matched raw input file, unmodified, to <outputDir>/sub-<label>/ses-<label>/.

# Same minimal single-fixation TobiiPro-format fixture shape used throughout
# this suite (see write_p1_01_fixture()/write_p703_fixture() in
# test-eyeQualityBatch.R) -- one participant staring at one fixed,
# off-center screen location for the whole recording, so the file passes
# cleanly through the full eyeQuality() pipeline. All that matters here is
# where the output lands, not the specific gaze values.
write_p707_fixture <- function(dir, filename) {
  n <- 50
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

  filepath <- file.path(dir, filename)
  readr::write_tsv(d, filepath)
  filepath
}

## ---- create_new_filename() unit tests: outputStructure = "flat" (regression) ----

test_that("create_new_filename() with outputStructure omitted defaults to 'flat' and matches the explicit 'flat' call exactly", {
  base_dir <- tempfile("p707_flat_default_")
  out_dir <- tempfile("p707_flat_default_out_")
  dir.create(base_dir)
  on.exit(unlink(c(base_dir, out_dir), recursive = TRUE), add = TRUE)

  inputfile <- file.path(base_dir, "sub-01_ses-01_task-test_physio.tsv")

  result_omitted <- create_new_filename(inputfile, "_desc-preproc", ".tsv", outputDir = out_dir)
  result_explicit_flat <- create_new_filename(inputfile, "_desc-preproc", ".tsv", outputDir = out_dir, outputStructure = "flat")

  expect_equal(fs::path(result_omitted), fs::path(result_explicit_flat))
  expect_equal(fs::path(result_omitted), fs::path(out_dir, "sub-01_ses-01_task-test_physio_desc-preproc.tsv"))
  # no nested sub-/ses- structure under out_dir in flat mode
  expect_false(fs::dir_exists(fs::path(out_dir, "derivatives")))
})

test_that("create_new_filename() with outputStructure = 'bids' but outputDir = NULL falls back to the default derivatives location unchanged", {
  base_dir <- tempfile("p707_bids_nooutdir_")
  dir.create(base_dir)
  on.exit(unlink(base_dir, recursive = TRUE), add = TRUE)

  inputfile <- file.path(base_dir, "sub-01_ses-01_task-test_physio.tsv")

  expect_no_warning(
    result <- create_new_filename(inputfile, "_desc-preproc", ".tsv", outputStructure = "bids")
  )

  expect_equal(
    fs::path(result),
    fs::path(base_dir, "derivatives", "eyeQuality-v1", "sub-01_ses-01_task-test_physio_desc-preproc.tsv")
  )
})

## ---- create_new_filename() unit tests: outputStructure = "bids" -----------

test_that("create_new_filename() with outputStructure = 'bids' and a properly-named file writes under derivatives/eyeQuality-v1/sub-<label>/ses-<label>/", {
  base_dir <- tempfile("p707_bids_in_")
  out_dir <- tempfile("p707_bids_out_")
  dir.create(base_dir)
  on.exit(unlink(c(base_dir, out_dir), recursive = TRUE), add = TRUE)

  inputfile <- file.path(base_dir, "sub-07_ses-02_task-test_recording-eyetracking_physio.tsv")

  result <- create_new_filename(inputfile, "_desc-preproc", ".tsv", outputDir = out_dir, outputStructure = "bids")

  expected <- fs::path(
    out_dir, "derivatives", "eyeQuality-v1", "sub-07", "ses-02",
    "sub-07_ses-02_task-test_recording-eyetracking_physio_desc-preproc.tsv"
  )
  expect_equal(fs::path(result), expected)
  expect_true(fs::dir_exists(fs::path(out_dir, "derivatives", "eyeQuality-v1", "sub-07", "ses-02")))
})

test_that("create_new_filename() bids-mode sub-/ses- parsing reads straight from the basename, ignoring the input file's own directory location", {
  # inputfile sits in a directory tree with NO sub-/ses- folders at all --
  # confirms the parse is basename-only, not directory-derived (this is
  # exactly what makes it work the same way for both listBidsFiles()
  # layout = "bids" and layout = "glob" discovery).
  base_dir <- tempfile("p707_bids_flatinput_")
  nested_dir <- file.path(base_dir, "some", "arbitrary", "glob", "path")
  out_dir <- tempfile("p707_bids_flatinput_out_")
  dir.create(nested_dir, recursive = TRUE)
  on.exit(unlink(c(base_dir, out_dir), recursive = TRUE), add = TRUE)

  inputfile <- file.path(nested_dir, "sub-09_ses-03_task-test_physio.tsv")

  result <- create_new_filename(inputfile, "_desc-preproc", ".tsv", outputDir = out_dir, outputStructure = "bids")

  expected <- fs::path(
    out_dir, "derivatives", "eyeQuality-v1", "sub-09", "ses-03",
    "sub-09_ses-03_task-test_physio_desc-preproc.tsv"
  )
  expect_equal(fs::path(result), expected)
})

test_that("create_new_filename() with outputStructure = 'bids' and a non-BIDS-named file falls back to flat placement with a warning, not an error", {
  base_dir <- tempfile("p707_bids_badname_")
  out_dir <- tempfile("p707_bids_badname_out_")
  dir.create(base_dir)
  on.exit(unlink(c(base_dir, out_dir), recursive = TRUE), add = TRUE)

  inputfile <- file.path(base_dir, "not_a_bids_filename.tsv")

  expect_warning(
    result <- create_new_filename(inputfile, "_desc-preproc", ".tsv", outputDir = out_dir, outputStructure = "bids"),
    regexp = "does not match the expected"
  )

  # falls back to the same flat placement outputStructure = "flat" would use
  expected <- fs::path(out_dir, "not_a_bids_filename_desc-preproc.tsv")
  expect_equal(fs::path(result), expected)
  expect_false(fs::dir_exists(fs::path(out_dir, "derivatives")))
})

test_that("create_new_filename() with outputStructure = 'bids' falls back to flat with a warning when only one of sub-/ses- is present", {
  base_dir <- tempfile("p707_bids_partial_")
  out_dir <- tempfile("p707_bids_partial_out_")
  dir.create(base_dir)
  on.exit(unlink(c(base_dir, out_dir), recursive = TRUE), add = TRUE)

  # has sub- but no ses-
  inputfile <- file.path(base_dir, "sub-01_task-test_physio.tsv")

  expect_warning(
    result <- create_new_filename(inputfile, "_desc-preproc", ".tsv", outputDir = out_dir, outputStructure = "bids"),
    regexp = "does not match the expected"
  )

  expected <- fs::path(out_dir, "sub-01_task-test_physio_desc-preproc.tsv")
  expect_equal(fs::path(result), expected)
})

## ---- eyeQuality()/saveFiles() end-to-end: outputStructure = "bids" --------

test_that("eyeQuality(saveData = TRUE, outputDir = <path>, outputStructure = 'bids') writes outputs under <outputDir>/derivatives/eyeQuality-v1/sub-<label>/ses-<label>/", {
  skip_on_cran()

  input_dir <- tempfile("p707_e2e_input_")
  out_dir <- tempfile("p707_e2e_output_")
  dir.create(input_dir)
  on.exit(unlink(c(input_dir, out_dir), recursive = TRUE), add = TRUE)

  fp <- write_p707_fixture(input_dir, "sub-03_ses-01_task-test_recording-eyetracking_physio.tsv")

  eyeQuality(
    fp,
    displayDimensionX_mm = 594,
    displayDimensionY_mm = 344,
    saveData = TRUE,
    batchName = "p707bids",
    outputDir = out_dir,
    outputStructure = "bids"
  )
  sinkReset()

  preproc_out <- create_new_filename(fp, "_desc-p707bids_preproc", ".tsv", outputDir = out_dir, outputStructure = "bids")
  expect_true(file.exists(preproc_out))
  expect_equal(
    fs::path(preproc_out),
    fs::path(
      out_dir, "derivatives", "eyeQuality-v1", "sub-03", "ses-01",
      "sub-03_ses-01_task-test_recording-eyetracking_physio_desc-p707bids_preproc.tsv"
    )
  )
  # not written flat directly under out_dir
  expect_false(file.exists(fs::path(out_dir, "sub-03_ses-01_task-test_recording-eyetracking_physio_desc-p707bids_preproc.tsv")))
})

test_that("eyeQuality(saveData = TRUE, outputDir = <path>, outputStructure = 'flat' or omitted) is byte-identical in output location to pre-P7-07 behavior", {
  skip_on_cran()

  input_dir <- tempfile("p707_e2e_flatregress_input_")
  out_dir <- tempfile("p707_e2e_flatregress_output_")
  dir.create(input_dir)
  on.exit(unlink(c(input_dir, out_dir), recursive = TRUE), add = TRUE)

  fp <- write_p707_fixture(input_dir, "sub-01_ses-01_task-test_recording-eyetracking_physio.tsv")

  eyeQuality(
    fp,
    displayDimensionX_mm = 594,
    displayDimensionY_mm = 344,
    saveData = TRUE,
    batchName = "p707flat",
    outputDir = out_dir
  )
  sinkReset()

  preproc_out <- create_new_filename(fp, "_desc-p707flat_preproc", ".tsv", outputDir = out_dir)
  expect_true(file.exists(preproc_out))
  expect_equal(
    fs::path(preproc_out),
    fs::path(out_dir, "sub-01_ses-01_task-test_recording-eyetracking_physio_desc-p707flat_preproc.tsv")
  )
  expect_false(fs::dir_exists(fs::path(out_dir, "derivatives")))
})

## ---- eyeQualityBatch() end-to-end: outputStructure = "bids" ---------------

test_that("eyeQualityBatch(outputStructure = 'bids', outputDir = <path>) writes every properly-named file's output under derivatives/eyeQuality-v1/sub-<label>/ses-<label>/", {
  skip_on_cran()

  in_dir <- tempfile("p707_batch_bids_in_")
  out_dir <- tempfile("p707_batch_bids_out_")
  dir.create(in_dir)
  dir.create(out_dir)
  on.exit(unlink(c(in_dir, out_dir), recursive = TRUE), add = TRUE)

  fp <- write_p707_fixture(in_dir, "sub-05_ses-02_task-test_recording-eyetracking_physio.tsv")

  eyeQualityBatch(
    in_dir,
    batchName = "p707batchbids",
    numberCores = 1,
    outputDir = out_dir,
    outputStructure = "bids"
  )

  out <- create_new_filename(fp, "_desc-p707batchbids_preproc_qcsummary", ".tsv", outputDir = out_dir, outputStructure = "bids")
  expect_true(file.exists(out))
  expect_equal(
    fs::path(out),
    fs::path(out_dir, "derivatives", "eyeQuality-v1", "sub-05", "ses-02", "sub-05_ses-02_task-test_recording-eyetracking_physio_desc-p707batchbids_preproc_qcsummary.tsv")
  )
})

test_that("eyeQualityBatch(outputStructure = 'bids') with a non-BIDS-named input file falls back to flat placement with a warning, and does not fail the batch", {
  skip_on_cran()

  in_dir <- tempfile("p707_batch_badname_in_")
  out_dir <- tempfile("p707_batch_badname_out_")
  dir.create(in_dir)
  dir.create(out_dir)
  on.exit(unlink(c(in_dir, out_dir), recursive = TRUE), add = TRUE)

  # deliberately no sub-/ses- identifiers in the filename
  fp <- write_p707_fixture(in_dir, "not_bids_named_recording.tsv")

  # Each candidate file's actual eyeQuality() call runs inside a parLapply()
  # worker -- a genuinely separate OS process (PSOCK on Windows, this
  # package's authoritative test platform). create_new_filename()'s
  # warning() is real and does fire there (confirmed directly: it appears in
  # the worker's console output, multiple times per file, once per
  # create_new_filename() call site in saveFiles()/eyeQuality()'s runlog),
  # but a warning signaled inside a separate process is never delivered back
  # to this (the master/test) process as an R warning condition the way a
  # same-process warning would be -- so expect_warning() here would only
  # ever pass by accident, not by actually observing the cross-process
  # warning. What IS verified, both here and by the unit-level
  # create_new_filename() tests above (which run the warning-raising branch
  # in-process, where expect_warning() genuinely can observe it): the batch
  # completes without error/failure and the file's output actually falls
  # back to flat placement.
  expect_no_error(
    eyeQualityBatch(
      in_dir,
      batchName = "p707badname",
      numberCores = 1,
      outputDir = out_dir,
      outputStructure = "bids"
    )
  )

  # fell back to flat placement, directly under out_dir
  out_flat <- create_new_filename(fp, "_desc-p707badname_preproc_qcsummary", ".tsv", outputDir = out_dir)
  expect_true(file.exists(out_flat))

  summary_file <- file.path(in_dir, "preprocessing_batch_summary_desc-p707badname.txt")
  summary_result <- parsePreprocessingBatchSummary(summary_file, "summary")
  expect_equal(summary_result$nPreprocessed, 1)
  expect_equal(summary_result$nFailed, 0)
})

test_that("eyeQualityBatch resumability honors outputStructure = 'bids': a resumed run skips an already-completed file's bids-placed output", {
  skip_on_cran()

  in_dir <- tempfile("p707_batch_resume_in_")
  out_dir <- tempfile("p707_batch_resume_out_")
  dir.create(in_dir)
  dir.create(out_dir)
  on.exit(unlink(c(in_dir, out_dir), recursive = TRUE), add = TRUE)

  fp <- write_p707_fixture(in_dir, "sub-08_ses-01_task-test_recording-eyetracking_physio.tsv")

  eyeQualityBatch(in_dir, batchName = "p707resumebids", numberCores = 1, outputDir = out_dir, outputStructure = "bids")

  out <- create_new_filename(fp, "_desc-p707resumebids_preproc_qcsummary", ".tsv", outputDir = out_dir, outputStructure = "bids")
  expect_true(file.exists(out))
  mtime1 <- file.mtime(out)
  Sys.sleep(1.1)

  eyeQualityBatch(in_dir, batchName = "p707resumebids", numberCores = 1, outputDir = out_dir, outputStructure = "bids")

  expect_equal(file.mtime(out), mtime1)

  summary_file <- file.path(in_dir, "preprocessing_batch_summary_desc-p707resumebids.txt")
  lines <- readr::read_lines(summary_file)
  skipped_headers <- lines[grepl("^------ Skipped files", lines)]
  expect_equal(
    skipped_headers[length(skipped_headers)],
    "------ Skipped files (already processed, resumability; n = 1):  "
  )
})

## ---- eyeQualityBatch() copyRawFile ----------------------------------------

test_that("eyeQualityBatch(outputStructure = 'bids', copyRawFile = TRUE) copies the raw input file unmodified to <outputDir>/sub-<label>/ses-<label>/", {
  skip_on_cran()

  in_dir <- tempfile("p707_copyraw_in_")
  out_dir <- tempfile("p707_copyraw_out_")
  dir.create(in_dir)
  dir.create(out_dir)
  on.exit(unlink(c(in_dir, out_dir), recursive = TRUE), add = TRUE)

  fp <- write_p707_fixture(in_dir, "sub-04_ses-01_task-test_recording-eyetracking_physio.tsv")
  original_content <- readLines(fp)

  eyeQualityBatch(
    in_dir,
    batchName = "p707copyraw",
    numberCores = 1,
    outputDir = out_dir,
    outputStructure = "bids",
    copyRawFile = TRUE
  )

  raw_copy_path <- fs::path(out_dir, "sub-04", "ses-01", "sub-04_ses-01_task-test_recording-eyetracking_physio.tsv")
  expect_true(file.exists(raw_copy_path))
  expect_equal(readLines(raw_copy_path), original_content)

  # derivative output still lands under derivatives/eyeQuality-v1/, sibling
  # to the raw copy directory, not inside it
  qcsummary_out <- create_new_filename(fp, "_desc-p707copyraw_preproc_qcsummary", ".tsv", outputDir = out_dir, outputStructure = "bids")
  expect_true(file.exists(qcsummary_out))
})

test_that("eyeQualityBatch(copyRawFile = TRUE) is a no-op when outputStructure = 'flat' (the default)", {
  skip_on_cran()

  in_dir <- tempfile("p707_copyraw_noop_flat_in_")
  out_dir <- tempfile("p707_copyraw_noop_flat_out_")
  dir.create(in_dir)
  dir.create(out_dir)
  on.exit(unlink(c(in_dir, out_dir), recursive = TRUE), add = TRUE)

  write_p707_fixture(in_dir, "sub-01_ses-01_task-test_recording-eyetracking_physio.tsv")

  eyeQualityBatch(
    in_dir,
    batchName = "p707copyrawflatnoop",
    numberCores = 1,
    outputDir = out_dir,
    copyRawFile = TRUE # outputStructure left at its "flat" default
  )

  # no per-subject raw copy directory should have been created at all
  expect_false(fs::dir_exists(fs::path(out_dir, "sub-01")))
})

test_that("eyeQualityBatch(copyRawFile = TRUE) is a no-op when outputDir is NULL", {
  skip_on_cran()

  in_dir <- tempfile("p707_copyraw_noop_nooutdir_in_")
  dir.create(in_dir)
  on.exit(unlink(in_dir, recursive = TRUE), add = TRUE)

  write_p707_fixture(in_dir, "sub-01_ses-01_task-test_recording-eyetracking_physio.tsv")

  expect_error(
    eyeQualityBatch(
      in_dir,
      batchName = "p707copyrawnooutdir",
      numberCores = 1,
      outputStructure = "bids",
      copyRawFile = TRUE
    ),
    NA
  )
  # nothing copied anywhere outside the default derivatives location
  expect_false(fs::dir_exists(fs::path(in_dir, "sub-01")))
})

test_that("eyeQualityBatch(outputStructure = 'bids', copyRawFile = TRUE) with a non-BIDS-named file warns and skips only the raw-copy step, without failing the batch", {
  skip_on_cran()

  in_dir <- tempfile("p707_copyraw_badname_in_")
  out_dir <- tempfile("p707_copyraw_badname_out_")
  dir.create(in_dir)
  dir.create(out_dir)
  on.exit(unlink(c(in_dir, out_dir), recursive = TRUE), add = TRUE)

  fp <- write_p707_fixture(in_dir, "not_bids_named_recording2.tsv")

  expect_warning(
    eyeQualityBatch(
      in_dir,
      batchName = "p707copyrawbadname",
      numberCores = 1,
      outputDir = out_dir,
      outputStructure = "bids",
      copyRawFile = TRUE
    ),
    regexp = "does not match the expected"
  )

  # derivative output still completed via the flat fallback
  out_flat <- create_new_filename(fp, "_desc-p707copyrawbadname_preproc_qcsummary", ".tsv", outputDir = out_dir)
  expect_true(file.exists(out_flat))
})

## ---- batch_config.yaml schema: outputStructure / copyRawFile round-trip ---

test_that("default_batch_config includes outputStructure = 'flat' and copyRawFile = FALSE", {
  cfg <- default_batch_config()
  expect_equal(cfg$outputStructure, "flat")
  expect_false(cfg$copyRawFile)
})

test_that("write_batch_config/read_batch_config round-trips outputStructure = 'bids' and copyRawFile = TRUE", {
  path <- tempfile("batchconfig_p707_bids_", fileext = ".yaml")
  on.exit(unlink(path), add = TRUE)

  cfg <- modifyList(default_batch_config(), list(
    batchName = "p707_roundtrip",
    directoryBIDS = "/data/p707",
    displayDimensionX_mm = 500,
    displayDimensionY_mm = 300,
    outputDir = "/data/p707_out",
    outputStructure = "bids",
    copyRawFile = TRUE
  ))

  write_batch_config(cfg, path)
  loaded <- read_batch_config(path)

  expect_equal(loaded$outputStructure, "bids")
  expect_true(loaded$copyRawFile)
})

test_that("read_batch_config on a config predating outputStructure/copyRawFile still loads with the flat/no-copy defaults", {
  path <- tempfile("batchconfig_p707_predate_", fileext = ".yaml")
  on.exit(unlink(path), add = TRUE)

  # a config saved with no outputStructure/copyRawFile keys at all -- as
  # every config written before this task existed would be
  yaml::write_yaml(list(
    schemaVersion = 1,
    batchName = "predates_p707",
    directoryBIDS = "/data/pre_p707",
    displayDimensionX_mm = 200,
    displayDimensionY_mm = 200
  ), path)

  loaded <- read_batch_config(path)
  expect_equal(loaded$outputStructure, "flat")
  expect_false(loaded$copyRawFile)
  expect_true(validate_batch_config(loaded))
})

test_that("validate_batch_config errors on an invalid outputStructure value", {
  cfg <- modifyList(default_batch_config(), list(
    batchName = "bad_outputstructure",
    directoryBIDS = "/data/x",
    displayDimensionX_mm = 500,
    displayDimensionY_mm = 300,
    outputStructure = "nested"
  ))

  expect_error(validate_batch_config(cfg), "`outputStructure` must be one of")
})

test_that("validate_batch_config errors on a non-logical copyRawFile value", {
  cfg <- modifyList(default_batch_config(), list(
    batchName = "bad_copyrawfile",
    directoryBIDS = "/data/x",
    displayDimensionX_mm = 500,
    displayDimensionY_mm = 300,
    copyRawFile = "yes"
  ))

  expect_error(validate_batch_config(cfg), "`copyRawFile` must be a single TRUE/FALSE")
})

test_that("validate_batch_config accepts both valid outputStructure values", {
  base_cfg <- modifyList(default_batch_config(), list(
    batchName = "valid_outputstructure",
    directoryBIDS = "/data/x",
    displayDimensionX_mm = 500,
    displayDimensionY_mm = 300
  ))

  expect_true(validate_batch_config(modifyList(base_cfg, list(outputStructure = "flat"))))
  expect_true(validate_batch_config(modifyList(base_cfg, list(outputStructure = "bids"))))
})

test_that("the bundled batch_config_template.yaml includes outputStructure = 'flat' and copyRawFile = FALSE and validates cleanly", {
  template_path <- system.file("templates", "batch_config_template.yaml", package = "eyeQuality")
  expect_true(nzchar(template_path))

  loaded <- read_batch_config(template_path)
  expect_equal(loaded$outputStructure, "flat")
  expect_false(loaded$copyRawFile)
})
