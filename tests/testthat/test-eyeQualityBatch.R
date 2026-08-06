# Builds a minimal, self-contained synthetic TobiiPro-format recording: a
# single simulated participant staring at one fixed, off-center screen
# location for the whole recording. Every eye is always valid and no gaze
# data is missing, so the file passes through every pipeline stage
# (interpolation, blink detection, eye selection, smoothing, IVT
# classification) as a single continuous fixation without hitting any of
# their edge-case branches - the only thing this fixture is meant to probe
# is whether displayDimensionX_mm/displayDimensionY_mm actually reach the
# visual-angle calculation.
write_p1_01_fixture <- function(dir, filename = "sub-01_task-test_recording-eyetracking_physio.tsv") {
  n <- 200
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
    # off-center on purpose: a gazepoint at the exact pixel center would
    # produce a 0 VA angle regardless of displayDimension_mm, which would
    # defeat the point of this fixture
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

test_that("eyeQualityBatch passes caller-supplied displayDimensionX_mm/Y_mm through to eyeQuality instead of the old hardcoded 594x344 literals", {
  skip_on_cran()

  # Two separate BIDS-like directories, each holding an identical copy of
  # the same input file - eyeQualityBatch() writes a "derivatives" folder
  # into the directory it processes, and a second run against a directory
  # that already contains one confuses listBidsFiles()'s subject-directory
  # detection, so each run gets its own directory rather than re-running
  # against the same one twice.
  dir_default <- tempfile("p101_default_")
  dir_custom <- tempfile("p101_custom_")
  dir.create(dir_default)
  dir.create(dir_custom)
  on.exit(unlink(c(dir_default, dir_custom), recursive = TRUE), add = TRUE)

  fp_default <- write_p1_01_fixture(dir_default)
  fp_custom <- write_p1_01_fixture(dir_custom)

  # default display dimensions (594mm x 344mm)
  eyeQualityBatch(
    dir_default,
    batchName = "p101default",
    numberCores = 1,
    displayDimensionX_mm = 594,
    displayDimensionY_mm = 344
  )

  # explicitly different display dimensions
  eyeQualityBatch(
    dir_custom,
    batchName = "p101custom",
    numberCores = 1,
    displayDimensionX_mm = 344,
    displayDimensionY_mm = 344
  )

  out_default <- create_new_filename(fp_default, "_desc-p101default_preproc", ".tsv")
  out_custom <- create_new_filename(fp_custom, "_desc-p101custom_preproc", ".tsv")

  expect_true(file.exists(out_default))
  expect_true(file.exists(out_custom))

  data_default <- readr::read_tsv(out_default, show_col_types = FALSE)
  data_custom <- readr::read_tsv(out_custom, show_col_types = FALSE)

  # the fixture holds gaze fixed at one off-center screen location for the
  # whole recording, so gazeX.preprocessed_va is constant within each run;
  # values below are calculateVisualAngle(1400, 600, 1920, X_mm) rounded to
  # 2 decimal places, i.e. exactly what eyeQuality() would have computed had
  # it been called directly with these displayDimensionX_mm values
  expect_equal(unique(data_default$gazeX.preprocessed_va), 12.77, tolerance = 1e-6)
  expect_equal(unique(data_custom$gazeX.preprocessed_va), 7.48, tolerance = 1e-6)

  # the regression this guards against: eyeQualityBatch() used to hardcode
  # displayDimensionX_mm = 594, displayDimensionY_mm = 344 literally inside
  # the parLapply() call, ignoring whatever the caller passed in. If that
  # regressed, both runs above would silently produce identical output.
  expect_false(isTRUE(all.equal(
    data_default$gazeX.preprocessed_va,
    data_custom$gazeX.preprocessed_va
  )))
})

# P1-02: eyeQualityBatch() must validate batchName/numberCores before ever
# touching the filesystem, so a nonexistent directory is fine for the
# error-path tests below - the guard checks run before listBidsFiles() or
# any output file is written.

test_that("eyeQualityBatch errors on NULL batchName instead of failing deep inside parLapply()", {
  bogus_dir <- tempfile("p102_null_")

  expect_error(
    eyeQualityBatch(bogus_dir, batchName = NULL),
    regexp = "batchName"
  )
})

test_that("eyeQualityBatch errors on empty-string batchName instead of failing deep inside parLapply()", {
  bogus_dir <- tempfile("p102_empty_")

  expect_error(
    eyeQualityBatch(bogus_dir, batchName = ""),
    regexp = "batchName"
  )
})

test_that("eyeQualityBatch errors on negative numberCores instead of failing deep inside parLapply()", {
  bogus_dir <- tempfile("p102_negcores_")

  expect_error(
    eyeQualityBatch(bogus_dir, batchName = "x", numberCores = -1),
    regexp = "numberCores"
  )
})

test_that("eyeQualityBatch errors on non-integer numberCores instead of failing deep inside parLapply()", {
  bogus_dir <- tempfile("p102_fractionalcores_")

  expect_error(
    eyeQualityBatch(bogus_dir, batchName = "x", numberCores = 2.5),
    regexp = "numberCores"
  )
})

# P1-12 (second scope note, 2026-08-03): R/eyeQualityBatch.R:169 has the same
# `if (is.null(batchName)) "" else paste0(batchName, "_")` NULL-safe pattern
# used to build `qcsummaryPattern`. Unlike the other three P1-12 sites, this
# one is not directly reachable with a live bug today: the guard tested above
# ("eyeQualityBatch errors on NULL batchName instead of failing deep inside
# parLapply()") stops execution at line ~27-32, before line 169 ever runs, so
# batchName can never actually be NULL by the time this expression executes
# through the normal eyeQualityBatch() call path. There is no way to reach
# line 169 with batchName == NULL without bypassing that earlier validation
# (e.g. by editing the guard out), so a test that calls eyeQualityBatch()
# itself cannot exercise the fixed branch specifically -- it can only ever
# hit the guard first. The fix is still correct defense-in-depth (protects
# against a future refactor loosening or reordering the guard), and is
# covered by the repo-wide grep regression test below
# ("no ifelse(is.null(batchName)... pattern survives anywhere in R/"), which
# would fail if this exact broken pattern were ever reintroduced here.

test_that("eyeQualityBatch does not false-positive its batchName/numberCores guards on valid input", {
  skip_on_cran()

  # A valid batchName with an explicit, valid positive-integer numberCores,
  # and a valid batchName with numberCores omitted (defaulting to NULL),
  # should both sail past the new P1-02 guard checks and run the real
  # pipeline to completion - reusing the P1-01 fixture is the cheapest way
  # to prove that, since a successful run with an output file on disk is
  # only possible if neither guard tripped.
  dir_explicit_cores <- tempfile("p102_valid_explicit_")
  dir_default_cores <- tempfile("p102_valid_default_")
  dir.create(dir_explicit_cores)
  dir.create(dir_default_cores)
  on.exit(unlink(c(dir_explicit_cores, dir_default_cores), recursive = TRUE), add = TRUE)

  fp_explicit <- write_p1_01_fixture(dir_explicit_cores)
  fp_default <- write_p1_01_fixture(dir_default_cores)

  expect_error(
    eyeQualityBatch(dir_explicit_cores, batchName = "p102explicit", numberCores = 1),
    NA
  )

  expect_error(
    eyeQualityBatch(dir_default_cores, batchName = "p102default"),
    NA
  )

  out_explicit <- create_new_filename(fp_explicit, "_desc-p102explicit_preproc", ".tsv")
  out_default <- create_new_filename(fp_default, "_desc-p102default_preproc", ".tsv")

  expect_true(file.exists(out_explicit))
  expect_true(file.exists(out_default))
})

# P3-10: eyeQualityBatch() gained a verbose = FALSE argument, forwarded
# through to eyeQuality() for every file in the batch. eyeQualityBatch()
# always calls eyeQuality(..., saveData = TRUE, ...) internally, which sinks
# eyeQuality()'s console output (including any "[verbose]"-tagged diagnostic
# lines) to a per-file run log at
# "<inputDir>/derivatives/eyeQuality-v1/<basename>_desc-<batchName>_preproc_runlog.txt"
# (R/getFileRunLogName.R's naming convention, minus its unused "2" suffix --
# R/eyeQuality.R builds this path directly via create_new_filename()). Since
# each file is actually processed inside a parallel cluster worker (a
# separate process), reading that on-disk log back is the only reliable way
# to confirm verbose really reached the per-file eyeQuality() call, as
# opposed to capturing this test process's own stdout.
write_p310_fixture_with_invalid_run <- function(dir, filename = "sub-01_task-test_recording-eyetracking_physio.tsv") {
  n <- 200
  dt_ms <- 17
  ts <- seq(0, by = dt_ms, length.out = n)

  validityLeft <- rep("Valid", n)
  validityRight <- rep("Valid", n)
  # 21 consecutive Invalid rows -- comfortably above
  # .diagnose_consecutive_runs()'s default min_run_length (5), so a run-range
  # verbose line is guaranteed to be emitted.
  invalidRows <- 100:120
  validityLeft[invalidRows] <- "Invalid"
  validityRight[invalidRows] <- "Invalid"

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
    "Validity left" = validityLeft,
    "Validity right" = validityRight,
    check.names = FALSE
  )

  filepath <- file.path(dir, filename)
  readr::write_tsv(d, filepath)
  filepath
}

p310_run_log_path <- function(fp, batchName) {
  fs::path(
    dirname(fp), "derivatives", "eyeQuality-v1",
    paste0(fs::path_ext_remove(basename(fp)), "_desc-", batchName, "_preproc_runlog.txt")
  )
}

test_that("eyeQualityBatch(verbose = TRUE) forwards verbose through to each file's eyeQuality() call, reaching the per-file run log", {
  skip_on_cran()

  dir <- tempfile("p310_batch_verbose_true_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  fp <- write_p310_fixture_with_invalid_run(dir)

  eyeQualityBatch(
    dir,
    batchName = "p310verbosetrue",
    numberCores = 1,
    displayDimensionX_mm = 594,
    displayDimensionY_mm = 344,
    verbose = TRUE
  )

  logfile <- p310_run_log_path(fp, "p310verbosetrue")
  expect_true(file.exists(logfile))

  logLines <- readr::read_lines(logfile)
  expect_true(any(grepl("\\[verbose\\]", logLines)))
  expect_true(any(grepl(
    "consecutive samples flagged for: left eye validity", logLines
  )))
})

test_that("eyeQualityBatch(verbose = FALSE) (default) forwards FALSE through to each file's eyeQuality() call, with no diagnostics in the run log", {
  skip_on_cran()

  dir <- tempfile("p310_batch_verbose_false_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  fp <- write_p310_fixture_with_invalid_run(dir)

  eyeQualityBatch(
    dir,
    batchName = "p310verbosefalse",
    numberCores = 1,
    displayDimensionX_mm = 594,
    displayDimensionY_mm = 344
  )

  logfile <- p310_run_log_path(fp, "p310verbosefalse")
  expect_true(file.exists(logfile))

  logLines <- readr::read_lines(logfile)
  expect_false(any(grepl("\\[verbose\\]", logLines)))
})

# P7-03: eyeQualityBatch() gained resumability (a `force = FALSE` default
# argument) and promoted `outputDir` to an explicit formal argument.
#
# Same minimal TobiiPro-format content as write_p1_01_fixture() above, but
# nested under a sub-XX/ses-XX directory pair rather than written directly
# into `dir`. This matters specifically for the tests below that call
# eyeQualityBatch() twice against the *same* directory: eyeQualityBatch()
# writes a "derivatives" folder as a sibling of whatever raw file(s) it
# processes (default outputDir = NULL), and listBidsFiles()'s bids-layout
# subject-directory detection treats *any* subdirectory of `directory` as a
# subject directory to be matched against subjectPattern_regex. If the raw
# file sits directly in `dir` (write_p1_01_fixture's layout), the very act of
# processing it once creates a "derivatives" subdirectory of `dir` itself; on
# a second call, listBidsFiles() then sees `dir` as containing a
# subdirectory, switches out of its "no subject subdirs, treat `directory`
# itself as one session" branch, tries to match "derivatives" against
# subjectPattern_regex, fails, and silently finds zero candidate files at all
# - defeating any test that relies on the second run actually re-discovering
# the same file. Nesting under sub-XX/ses-XX avoids this: eyeQuality-v1's
# "derivatives" folder lands inside ses-XX (a sibling of the raw file, one
# level deeper than the root), never disturbing `dir`'s or sub-XX's own
# subdirectory listing between runs. (See write_p1_01_fixture()'s own
# eyeQualityBatch() usage above, and its comment on why each of *its* tests
# uses a fresh directory per run rather than reusing one across calls.)
write_p703_fixture <- function(dir, subject, session = "ses-01", filename = NULL) {
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

  if (is.null(filename)) {
    filename <- paste0(subject, "_", session, "_task-test_recording-eyetracking_physio.tsv")
  }

  session_dir <- file.path(dir, subject, session)
  dir.create(session_dir, recursive = TRUE, showWarnings = FALSE)
  filepath <- file.path(session_dir, filename)
  readr::write_tsv(d, filepath)
  filepath
}

test_that("eyeQualityBatch (force = FALSE default) on a fresh directory processes every file and reports 0 skipped", {
  skip_on_cran()

  dir <- tempfile("p703_fresh_")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  fp <- write_p703_fixture(dir, "sub-01")

  eyeQualityBatch(dir, batchName = "p703fresh", numberCores = 1)

  out <- create_new_filename(fp, "_desc-p703fresh_preproc_qcsummary", ".tsv")
  expect_true(file.exists(out))

  summary_file <- file.path(dir, "preprocessing_batch_summary_desc-p703fresh.txt")
  lines <- readr::read_lines(summary_file)
  skipped_header <- lines[grepl("^------ Skipped files", lines)]
  expect_equal(
    skipped_header,
    "------ Skipped files (already processed, resumability; n = 0):  "
  )

  skipped_result <- parsePreprocessingBatchSummary(summary_file, "skippedfiles")
  expect_equal(skipped_result, character(0))

  # P7-04: a no-failures run's "failedfiles" section should round-trip to a
  # zero-row file/error tibble (not character(0) -- that was the pre-P7-04
  # return shape) with the right column names/types.
  failed_result <- parsePreprocessingBatchSummary(summary_file, "failedfiles")
  expect_equal(failed_result, tibble::tibble(file = character(0), error = character(0)))
})

test_that("eyeQualityBatch resumed run against the same directory/batchName skips already-completed files, leaves their output untouched, and skips cluster creation", {
  skip_on_cran()

  dir <- tempfile("p703_resume_")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  fp <- write_p703_fixture(dir, "sub-01")

  eyeQualityBatch(dir, batchName = "p703resume", numberCores = 1)

  out <- create_new_filename(fp, "_desc-p703resume_preproc_qcsummary", ".tsv")
  expect_true(file.exists(out))
  mtime1 <- file.mtime(out)
  content1 <- readr::read_lines(out)

  # ensure a second write, if the fix regressed and the file got
  # reprocessed, would produce a detectably different mtime rather than
  # landing within filesystem mtime resolution of the first write
  Sys.sleep(1.1)

  eyeQualityBatch(dir, batchName = "p703resume", numberCores = 1)

  expect_equal(file.mtime(out), mtime1)
  expect_equal(readr::read_lines(out), content1)

  summary_file <- file.path(dir, "preprocessing_batch_summary_desc-p703resume.txt")
  lines <- readr::read_lines(summary_file)

  # the second run's "number of cores" line should reflect the "nothing to
  # dispatch" branch (cluster creation skipped entirely), not a real
  # makeCluster(1)
  cores_lines <- lines[grepl("^number of cores", lines)]
  expect_equal(cores_lines[length(cores_lines)], "number of cores = 0")

  skipped_result <- parsePreprocessingBatchSummary(summary_file, "skippedfiles")
  expect_equal(normalizePath(skipped_result), normalizePath(fp))
})

test_that("eyeQualityBatch partial resumability dispatches only files without an existing qcsummary output", {
  skip_on_cran()

  dir <- tempfile("p703_partial_")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  fp1 <- write_p703_fixture(dir, "sub-01")

  eyeQualityBatch(dir, batchName = "p703partial", numberCores = 1)

  out1 <- create_new_filename(fp1, "_desc-p703partial_preproc_qcsummary", ".tsv")
  expect_true(file.exists(out1))
  mtime1 <- file.mtime(out1)

  # a second, never-before-seen file added to the same BIDS directory
  fp2 <- write_p703_fixture(dir, "sub-02")
  Sys.sleep(1.1)

  eyeQualityBatch(dir, batchName = "p703partial", numberCores = 1)

  out2 <- create_new_filename(fp2, "_desc-p703partial_preproc_qcsummary", ".tsv")
  expect_true(file.exists(out2))

  # fp1's already-completed output must not have been reprocessed
  expect_equal(file.mtime(out1), mtime1)

  summary_file <- file.path(dir, "preprocessing_batch_summary_desc-p703partial.txt")
  lines <- readr::read_lines(summary_file)
  skipped_header_idx <- grep("^------ Skipped files", lines)
  # last (second run's) skipped-section header should report exactly fp1
  last_skipped_header <- lines[skipped_header_idx[length(skipped_header_idx)]]
  expect_equal(
    last_skipped_header,
    "------ Skipped files (already processed, resumability; n = 1):  "
  )

  skipped_result <- parsePreprocessingBatchSummary(summary_file, "skippedfiles")
  expect_equal(normalizePath(skipped_result), normalizePath(fp1))
})

test_that("eyeQualityBatch(force = TRUE) reprocesses every candidate file regardless of existing qcsummary output", {
  skip_on_cran()

  dir <- tempfile("p703_force_")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  fp <- write_p703_fixture(dir, "sub-01")

  eyeQualityBatch(dir, batchName = "p703force", numberCores = 1)

  out <- create_new_filename(fp, "_desc-p703force_preproc_qcsummary", ".tsv")
  mtime1 <- file.mtime(out)
  Sys.sleep(1.1)

  eyeQualityBatch(dir, batchName = "p703force", numberCores = 1, force = TRUE)

  expect_true(file.mtime(out) > mtime1)

  summary_file <- file.path(dir, "preprocessing_batch_summary_desc-p703force.txt")
  lines <- readr::read_lines(summary_file)

  skipped_headers <- lines[grepl("^------ Skipped files", lines)]
  expect_equal(
    skipped_headers[length(skipped_headers)],
    "------ Skipped files (already processed, resumability; n = 0):  "
  )

  cores_lines <- lines[grepl("^number of cores", lines)]
  expect_equal(cores_lines[length(cores_lines)], "number of cores = 1")
})

test_that("eyeQualityBatch resumability check honors outputDir rather than only the default derivatives location", {
  skip_on_cran()

  # inputDir never has its own subdirectory structure touched across the two
  # runs below (both runs write output to the external outDir, not
  # inputDir), so the flat, single-file write_p1_01_fixture() layout is safe
  # to reuse here across repeated calls against the same inputDir.
  inputDir <- tempfile("p703_outdir_in_")
  outDir <- tempfile("p703_outdir_out_")
  dir.create(inputDir)
  dir.create(outDir)
  on.exit(unlink(c(inputDir, outDir), recursive = TRUE), add = TRUE)

  fp <- write_p1_01_fixture(inputDir)

  eyeQualityBatch(inputDir, batchName = "p703outdir", numberCores = 1, outputDir = outDir)

  out <- create_new_filename(fp, "_desc-p703outdir_preproc_qcsummary", ".tsv", outputDir = outDir)
  expect_true(file.exists(out))
  # output actually landed in outDir, not the default derivatives location -
  # confirms the resumability check below isn't accidentally passing by
  # finding a file under the default <inputDir>/derivatives/ path instead
  expect_false(dir.exists(file.path(inputDir, "derivatives")))

  mtime1 <- file.mtime(out)
  Sys.sleep(1.1)

  eyeQualityBatch(inputDir, batchName = "p703outdir", numberCores = 1, outputDir = outDir)

  expect_equal(file.mtime(out), mtime1)

  summary_file <- file.path(inputDir, "preprocessing_batch_summary_desc-p703outdir.txt")
  skipped_result <- parsePreprocessingBatchSummary(summary_file, "skippedfiles")
  expect_equal(normalizePath(skipped_result), normalizePath(fp))
})

test_that("eyeQualityBatch completes cleanly with no cluster-creation crash when every candidate file is already processed", {
  skip_on_cran()

  dir <- tempfile("p703_allskip_")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  write_p703_fixture(dir, "sub-01")

  eyeQualityBatch(dir, batchName = "p703allskip", numberCores = 1)

  # makeCluster(0) errors -- if the length(files_to_process) == 0 guard
  # regressed, this second call would error out instead of completing
  expect_error(
    eyeQualityBatch(dir, batchName = "p703allskip", numberCores = 1),
    NA
  )

  summary_file <- file.path(dir, "preprocessing_batch_summary_desc-p703allskip.txt")
  expect_true(file.exists(summary_file))

  summary_result <- parsePreprocessingBatchSummary(summary_file, "summary")
  expect_equal(summary_result$nfiles, 1)
  expect_equal(summary_result$nPreprocessed, 1)
  expect_equal(summary_result$nFailed, 0)
})

# P7-04: eyeQualityBatch() gained structured per-file failure detail -- each
# failed file's conditionMessage(e), captured inside the parLapply() worker's
# tryCatch(), is now written into the batch summary's "Files that failed
# processing" section (a 2-line block per file: the filepath, then an
# indented "  error: <message>" line) and returned as the `error` column of
# parsePreprocessingBatchSummary(info_to_extract = "failedfiles")'s file/error
# tibble.
#
# Tobii Studio-format fixture with two out-of-order RecordingTimestamp
# values, structurally identical in spirit to
# tests/testthat/fixtures/tobii_studio_out_of_order_timestamps.tsv (see
# test-edgeCaseFixtures.R's "(b) out-of-order timestamps fixture" section,
# from P1-14) but built programmatically so it can be dropped into a
# sub-XX/ses-XX BIDS layout at an arbitrary path -- checkOrderedTimestamps()
# returns FALSE for it, and eyeQuality() aborts via stop() with a
# deterministic, single-line diagnostic message, which is what makes it a
# reliable "real eyeQuality() error" fixture for exercising P7-04's failure
# capture end-to-end (confirmed directly against eyeQuality() before use
# here).
write_p704_erroring_fixture <- function(dir, subject, session = "ses-01", filename = NULL) {
  n <- 10
  dt_ms <- 17
  ts <- seq(0, by = dt_ms, length.out = n)
  # swap rows 5 and 6 to break monotonicity, same shape as
  # tobii_studio_out_of_order_timestamps.tsv
  ts[c(5, 6)] <- ts[c(6, 5)]

  d <- data.frame(
    "StudioVersionRec" = rep("3.4.8", n),
    "StudioEvent" = rep(NA_character_, n),
    "StudioEventData" = rep(NA_character_, n),
    "RecordingDuration" = rep(NA_real_, n),
    "RecordingResolution" = rep("1920 x 1080", n),
    "EyeTrackerTimestamp" = ts,
    "RecordingTimestamp" = ts,
    "GazePointLeftX (ADCSpx)" = rep(1400, n),
    "GazePointLeftY (ADCSpx)" = rep(800, n),
    "GazePointRightX (ADCSpx)" = rep(1400, n),
    "GazePointRightY (ADCSpx)" = rep(800, n),
    "EyePosLeftZ (ADCSmm)" = rep(600, n),
    "EyePosRightZ (ADCSmm)" = rep(600, n),
    "PupilLeft" = rep(3.5, n),
    "PupilRight" = rep(3.5, n),
    "ValidityLeft" = rep(0, n),
    "ValidityRight" = rep(0, n),
    check.names = FALSE
  )

  if (is.null(filename)) {
    filename <- paste0(subject, "_", session, "_task-test_recording-eyetracking_physio.tsv")
  }

  session_dir <- file.path(dir, subject, session)
  dir.create(session_dir, recursive = TRUE, showWarnings = FALSE)
  filepath <- file.path(session_dir, filename)
  readr::write_tsv(d, filepath)
  filepath
}

# The exact diagnosticText eyeQuality() raises for this fixture (see
# R/eyeQuality.R's checkOrderedTimestamps() branch, and
# test-edgeCaseFixtures.R's identical assertion against the checked-in
# out-of-order-timestamps fixture).
p704_expected_error_message <- function(fp) {
  paste0(
    "Data is not chronologically ordered based on timestamp. Pre-processing for file ",
    fp,
    " has been aborted."
  )
}

test_that("eyeQualityBatch captures a real eyeQuality() error's message in the batch summary and parsePreprocessingBatchSummary('failedfiles') returns it against the right file", {
  skip_on_cran()

  dir <- tempfile("p704_singlefail_")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  fp <- write_p704_erroring_fixture(dir, "sub-01")

  eyeQualityBatch(dir, batchName = "p704singlefail", numberCores = 1)

  summary_file <- file.path(dir, "preprocessing_batch_summary_desc-p704singlefail.txt")
  expect_true(file.exists(summary_file))

  expected_error <- p704_expected_error_message(fp)

  lines <- readr::read_lines(summary_file)
  failed_header_idx <- grep("^------ Files that failed processing", lines)
  expect_length(failed_header_idx, 1)
  expect_match(lines[failed_header_idx], "n = 1\\)", fixed = FALSE)
  # the fixed 2-lines-per-entry block: filepath, then the indented error line
  expect_equal(lines[failed_header_idx + 1], fp)
  expect_equal(lines[failed_header_idx + 2], paste0("  error: ", expected_error))

  failed_result <- parsePreprocessingBatchSummary(summary_file, "failedfiles")
  expect_s3_class(failed_result, "data.frame")
  expect_named(failed_result, c("file", "error"))
  expect_equal(nrow(failed_result), 1)
  expect_equal(normalizePath(failed_result$file), normalizePath(fp))
  expect_equal(failed_result$error, expected_error)

  # no qcsummary output should exist for a file that errored
  out <- create_new_filename(fp, "_desc-p704singlefail_preproc_qcsummary", ".tsv")
  expect_false(file.exists(out))
})

test_that("eyeQualityBatch mixed batch (one success, one failure) under numberCores = 2 (real multi-worker PSOCK/FORK dispatch) reports exactly one failedfiles row without disturbing successfulfiles/skippedfiles", {
  skip_on_cran()

  # numberCores = 2 with two files forces both files to actually be
  # dispatched to two separate worker processes (rather than numberCores = 1
  # processing everything serially on a single worker) -- this is what
  # confirms the failing file's error message genuinely crosses a real
  # process boundary via parLapply()'s gathered return value, not just an
  # in-process capture that would work regardless of worker count.
  dir <- tempfile("p704_mixed_")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  fp_good <- write_p703_fixture(dir, "sub-01")
  fp_bad <- write_p704_erroring_fixture(dir, "sub-02")

  eyeQualityBatch(dir, batchName = "p704mixed", numberCores = 2)

  summary_file <- file.path(dir, "preprocessing_batch_summary_desc-p704mixed.txt")
  expect_true(file.exists(summary_file))

  cores_line <- readr::read_lines(summary_file)
  cores_line <- cores_line[grepl("^number of cores", cores_line)]
  expect_equal(cores_line, "number of cores = 2")

  out_good <- create_new_filename(fp_good, "_desc-p704mixed_preproc_qcsummary", ".tsv")
  expect_true(file.exists(out_good))
  out_bad <- create_new_filename(fp_bad, "_desc-p704mixed_preproc_qcsummary", ".tsv")
  expect_false(file.exists(out_bad))

  successful_result <- parsePreprocessingBatchSummary(summary_file, "successfulfiles")
  expect_equal(normalizePath(successful_result), normalizePath(out_good))

  skipped_result <- parsePreprocessingBatchSummary(summary_file, "skippedfiles")
  expect_equal(skipped_result, character(0))

  failed_result <- parsePreprocessingBatchSummary(summary_file, "failedfiles")
  expect_equal(nrow(failed_result), 1)
  expect_equal(normalizePath(failed_result$file), normalizePath(fp_bad))
  expect_equal(failed_result$error, p704_expected_error_message(fp_bad))
})

test_that("eyeQualityBatch resumed run (no force) retries a previously-failed file rather than skipping it, and the latest run no longer reports it as failed once it succeeds", {
  skip_on_cran()

  dir <- tempfile("p704_resume_")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  fp <- write_p704_erroring_fixture(dir, "sub-01")

  eyeQualityBatch(dir, batchName = "p704resume", numberCores = 1)

  summary_file <- file.path(dir, "preprocessing_batch_summary_desc-p704resume.txt")
  out <- create_new_filename(fp, "_desc-p704resume_preproc_qcsummary", ".tsv")
  expect_false(file.exists(out))

  failed_result_1 <- parsePreprocessingBatchSummary(summary_file, "failedfiles")
  expect_equal(nrow(failed_result_1), 1)
  expect_equal(normalizePath(failed_result_1$file), normalizePath(fp))

  # second run, same directory/batchName, no force = TRUE: since the first
  # run left no qcsummary output for fp (it errored), the resumability skip
  # check (get_qcsummary_output_path() existence) must NOT treat it as
  # already done -- it should be re-dispatched, not silently skipped and left
  # stale. The fixture is still broken at this point, so it fails again,
  # which is itself the proof it was actually retried rather than skipped.
  eyeQualityBatch(dir, batchName = "p704resume", numberCores = 1)

  lines <- readr::read_lines(summary_file)
  skipped_header_idx <- grep("^------ Skipped files", lines)
  expect_length(skipped_header_idx, 2)
  expect_equal(
    lines[skipped_header_idx[2]],
    "------ Skipped files (already processed, resumability; n = 0):  "
  )

  failed_result_2 <- parsePreprocessingBatchSummary(summary_file, "failedfiles")
  expect_equal(nrow(failed_result_2), 1)
  expect_equal(normalizePath(failed_result_2$file), normalizePath(fp))
  expect_false(file.exists(out))

  # now fix the fixture in place (same directory/subject/session/filename as
  # fp) so the third run actually succeeds
  write_p703_fixture(dir, "sub-01", filename = basename(fp))

  eyeQualityBatch(dir, batchName = "p704resume", numberCores = 1)

  expect_true(file.exists(out))

  # the LATEST run's failedfiles section (header_idx[length(header_idx)],
  # the P7-03 fix) should no longer show fp as failed, even though earlier
  # runs' sections in this same appended-to file still do
  failed_result_3 <- parsePreprocessingBatchSummary(summary_file, "failedfiles")
  expect_equal(nrow(failed_result_3), 0)

  successful_result_3 <- parsePreprocessingBatchSummary(summary_file, "successfulfiles")
  expect_true(normalizePath(out) %in% normalizePath(successful_result_3))
})

test_that("eyeQualityBatch substitutes the documented placeholder message for a failed file with no captured processing_results entry", {
  skip_on_cran()

  # This branch (failed_errors' vapply() fallback for a NULL/missing
  # processing_results entry) is unreachable through eyeQualityBatch()'s
  # normal tryCatch() flow -- a dispatched file either completes (NULL, no
  # error) or its tryCatch error handler runs and returns
  # conditionMessage(e), so processing_results always has a real, non-empty
  # string for a genuinely failed dispatched file. Reaching the fallback
  # requires an abnormal case the normal error path can't produce (e.g. a
  # worker process that ends without ever reaching the error handler), so it
  # is exercised here by mocking parallel::parLapply() itself to return a
  # same-length list of NULLs without ever calling the real eyeQuality() --
  # i.e. every dispatched file "completes" from parLapply()'s perspective
  # (NULL, not an error string) but produces no qcsummary output, landing it
  # in failedfiles with no real error message to report.
  dir <- tempfile("p704_placeholder_")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  fp <- write_p703_fixture(dir, "sub-01")

  testthat::local_mocked_bindings(
    parLapply = function(cl, X, fun, ...) {
      stats::setNames(vector("list", length(X)), NULL)
    },
    .package = "parallel"
  )

  eyeQualityBatch(dir, batchName = "p704placeholder", numberCores = 1)

  summary_file <- file.path(dir, "preprocessing_batch_summary_desc-p704placeholder.txt")
  expected_placeholder <- paste0(
    "(no error detail captured for this run - file was not dispatched this run, ",
    "or its worker process ended without raising a catchable R error)"
  )

  lines <- readr::read_lines(summary_file)
  failed_header_idx <- grep("^------ Files that failed processing", lines)
  expect_match(lines[failed_header_idx], "n = 1\\)", fixed = FALSE)
  expect_equal(lines[failed_header_idx + 2], paste0("  error: ", expected_placeholder))

  failed_result <- parsePreprocessingBatchSummary(summary_file, "failedfiles")
  expect_equal(nrow(failed_result), 1)
  expect_equal(normalizePath(failed_result$file), normalizePath(fp))
  expect_equal(failed_result$error, expected_placeholder)
})
