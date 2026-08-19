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

# ---------------------------------------------------------------------------
# P10-03: resolve_preproc_data_path()
# ---------------------------------------------------------------------------

test_that("resolve_preproc_data_path strips the trailing _qcsummary immediately before .tsv, leaving the sibling preproc data path", {
  # saveFiles() builds both filenames off the same "_desc-<batchName>_preproc"
  # stem, with qcsummary.tsv's name just appending "_qcsummary" onto that
  # stem before the extension (see R/saveFiles.R's preprocdesc/qcsummarydesc) --
  # so stripping "_qcsummary" right before ".tsv" must recover the exact
  # preproc sibling filename, for both the batchName-present and
  # batchName-NULL naming forms.
  expect_equal(
    resolve_preproc_data_path("/data/derivatives/eyeQuality-v1/sub-01_ses-1_desc-mybatch_preproc_qcsummary.tsv"),
    "/data/derivatives/eyeQuality-v1/sub-01_ses-1_desc-mybatch_preproc.tsv"
  )
  expect_equal(
    resolve_preproc_data_path("/data/derivatives/eyeQuality-v1/sub-01_ses-1_desc-preproc_qcsummary.tsv"),
    "/data/derivatives/eyeQuality-v1/sub-01_ses-1_desc-preproc.tsv"
  )
})

test_that("resolve_preproc_data_path leaves a path with no _qcsummary.tsv suffix unchanged rather than mangling it", {
  # sub() with an anchored pattern and no match returns the input unchanged --
  # this pins that behavior down explicitly, since a caller passing something
  # that isn't a real qcsummary.tsv path (e.g. already resolved, or a typo)
  # should get back exactly what it passed in, not a silently truncated path.
  expect_equal(
    resolve_preproc_data_path("/data/sub-01_desc-mybatch_preproc.tsv"),
    "/data/sub-01_desc-mybatch_preproc.tsv"
  )
})

# ---------------------------------------------------------------------------
# P10-03: load_plot_data()
# ---------------------------------------------------------------------------

test_that("load_plot_data returns ok = TRUE with 3 real ggplot objects for a real qcsummary/preproc pair from a batch run", {
  skip_on_cran()

  dir <- copy_bids_fixture_tree()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  eyeQualityBatch(dir, batchName = "p1003plots", numberCores = 1)

  found <- discover_qcsummary_files(dir, recursive = TRUE)
  expect_length(found, 4)

  result <- load_plot_data(found[1])

  expect_true(result$ok)
  expect_equal(result$preproc_path, resolve_preproc_data_path(found[1]))
  expect_true(file.exists(result$preproc_path))
  expect_s3_class(result$data, "data.frame")
  expect_length(result$plots, 3)
  expect_true(all(vapply(result$plots, function(p) inherits(p, "ggplot") || inherits(p, "gg"), logical(1))))
})

test_that("load_plot_data resolves two different qcsummary rows to two genuinely different preproc files and datasets", {
  skip_on_cran()

  dir <- copy_bids_fixture_tree()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  eyeQualityBatch(dir, batchName = "p1003distinct", numberCores = 1)

  found <- discover_qcsummary_files(dir, recursive = TRUE)
  expect_length(found, 4)

  result_a <- load_plot_data(found[1])
  result_b <- load_plot_data(found[2])

  expect_true(result_a$ok)
  expect_true(result_b$ok)
  expect_false(identical(result_a$preproc_path, result_b$preproc_path))
  expect_false(identical(result_a$data, result_b$data))
})

test_that("load_plot_data returns ok = FALSE with a clear, non-crashing error when the sibling preproc file is missing", {
  # Filename deliberately avoids the word "missing" -- load_plot_data()'s
  # generic tryCatch failure branch (a genuine read/plot error once a file IS
  # found) also produces an error string containing the file's basename, so a
  # qcsummary_path with "missing" baked into its own name would let this test
  # pass even if the dedicated file.exists() early-return branch below were
  # deleted and every failure fell through to the generic branch instead --
  # asserting on "should sit alongside" (wording unique to the early-return
  # branch's message) is what actually pins down that specific branch ran.
  qcsummary_path <- file.path(tempdir(), "sub-x_ses-1_desc-gonewrong_preproc_qcsummary.tsv")
  # deliberately don't create the sibling *_preproc.tsv file

  result <- load_plot_data(qcsummary_path)

  expect_false(result$ok)
  expect_equal(result$preproc_path, resolve_preproc_data_path(qcsummary_path))
  expect_true(is.character(result$error) && nzchar(result$error))
  expect_match(result$error, "should sit alongside", fixed = TRUE)
  expect_null(result$data)
  expect_null(result$plots)
})

test_that("load_plot_data returns ok = FALSE with a clear error (not a crash) when the preproc file exists but is missing columns generateEyeTrackingPlots() needs", {
  dir <- tempfile("p1003_badcols_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  qcsummary_path <- file.path(dir, "sub-y_ses-1_desc-badcols_preproc_qcsummary.tsv")
  preproc_path <- file.path(dir, "sub-y_ses-1_desc-badcols_preproc.tsv")
  file.create(qcsummary_path)
  # a syntactically valid .tsv, but missing every column
  # generateEyeTrackingPlots()/plotGazeAndBlinks() require (recordingTimestamp_ms,
  # IVT.classification, blink.classification, gazeLeftX, etc.)
  readr::write_tsv(data.frame(x = 1:3, y = 4:6), preproc_path)

  result <- load_plot_data(qcsummary_path)

  expect_false(result$ok)
  expect_true(is.character(result$error) && nzchar(result$error))
  expect_null(result$data)
  expect_null(result$plots)
})

# ---------------------------------------------------------------------------
# P10-03: row-selection wiring (app.R) -- DT_rows_selected index stability
# ---------------------------------------------------------------------------
#
# DT::renderDT()'s default server = TRUE (server-side processing) is what's
# actually used by this app's qc_table output (renderDT() at its default,
# unmodified -- see app.R). Confirmed directly from DT's own R source
# (DT:::dataTablesFilter(), the ajax handler invoked for every sort/search/
# page request DataTables' JS makes back to the server) that under
# server-side mode, row indices reported back to Shiny as
# "<id>_rows_selected" are indices into the *original* data.frame passed to
# datatable() (this app's result$table): dataTablesFilter() computes iAll
# (search-filtered indices into the original data), reorders it into iCurrent
# for the requested sort order, and DT's client-side JS
# (updateRowsSelected()/methods.select()) maps any click position on the
# currently-displayed page back through DT_rows_current before sending
# "rows_selected" to Shiny -- so a value the app receives in
# input$qc_table_rows_selected is guaranteed stable across sort/search/page
# state, never a raw display-order/page-local index. That guarantee is what
# lets app.R's selected_source_file() safely index directly into
# result$table$source_file[sel[1]] (the same pre-sort/filter data.frame) --
# these tests exercise that indexing directly, standing in for a value DT's
# JS would send after a user has sorted/filtered/paged the table.
test_that("row selection resolves distinct qc_table_rows_selected indices to their correct, distinct source_file/recording, matching a real 4-file, 128-row combined table", {
  skip_on_cran()

  app_dir <- system.file("shiny-apps", "analyze", package = "eyeQuality")
  expect_true(nzchar(app_dir))

  dir <- copy_bids_fixture_tree()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  # Normalized to forward slashes up front: shinyFiles::parseDirPath()
  # reconstructs directory paths via fs::path() (always forward-slash), so
  # comparing this test's own directly-built expected_table$source_file
  # against the app's resolved-through-shinyFiles values needs both sides on
  # the same separator convention -- tempfile()'s raw return value on Windows
  # keeps its root segment backslash-separated, which is purely a string
  # representation difference, not a real path mismatch.
  dir <- normalizePath(dir, winslash = "/", mustWork = TRUE)

  eyeQualityBatch(dir, batchName = "p1003rowsel", numberCores = 1)

  # Build the exact combined table the app itself would build, to know ahead
  # of time which row indices belong to which recording/source_file.
  expected_table <- load_qcsummary_table(dir, recursive = TRUE)$table
  expect_equal(nrow(expected_table), 128)

  # 32 qc_metric rows per file, in discover_qcsummary_files()'s sorted file
  # order -- pick one row index from each of the 4 files' row ranges,
  # deliberately not just row 1 of each, to stand in for indices a user could
  # only have reached after sorting/filtering/paging away from the table's
  # natural top-to-bottom order.
  probe_indices <- c(5, 40, 70, 115)
  expected_recordings <- expected_table$recording[probe_indices]
  expect_length(unique(expected_recordings), 4)

  # shinyDirChoose()'s "roots" include Home = fs::path_home() -- the same
  # root the app's server() constructs (see `volumes` in app.R) -- and
  # shinyFiles::parseDirPath() resolves an input$directory value of
  # list(root = <name>, path = <segments>) as
  # path(roots[[root]], paste0(path, collapse = "/")). tempfile() dirs sit
  # under the OS temp dir, which is itself under the user's home directory on
  # this platform, so `dir`'s path relative to fs::path_home() is a real,
  # resolvable shinyDirChoose()-style selection -- not a mocked-out
  # selected_dir(), but the actual shinyFiles resolution path app.R uses.
  home <- fs::path_home()
  expect_true(startsWith(normalizePath(dir, winslash = "/"), normalizePath(home, winslash = "/")))
  rel <- sub(
    paste0("^", normalizePath(home, winslash = "/"), "/?"),
    "",
    normalizePath(dir, winslash = "/")
  )
  path_segments <- as.list(strsplit(rel, "/")[[1]])

  shiny::testServer(app_dir, {
    session$setInputs(
      directory = list(root = "Home", path = path_segments),
      recursiveSearch = TRUE
    )
    session$setInputs(load = 1)

    result <- load_result()
    expect_equal(result$n_files, 4L)
    expect_equal(nrow(result$table), 128)

    # Compared via normalizePath() on both sides rather than raw string
    # equality: what this test actually needs to pin down is that
    # qc_table_rows_selected == idx resolves to the SAME underlying file
    # expected_table$source_file[idx] points at -- not that both code paths
    # produce byte-identical path strings. selected_source_file()/plot_result()
    # get there via shinyFiles::parseDirPath() (fs::path(), always
    # forward-slash), while expected_table was built by calling
    # load_qcsummary_table() directly on the raw tempfile()-returned `dir`
    # string; those two routes can differ in slash style/short-vs-long
    # Windows path segments for the exact same file without that being a real
    # bug in either.
    norm <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)

    for (idx in probe_indices) {
      session$setInputs(qc_table_rows_selected = idx)

      expect_equal(norm(selected_source_file()), norm(expected_table$source_file[idx]))

      pr <- plot_result()
      expect_true(pr$ok)
      expect_equal(norm(pr$preproc_path), norm(resolve_preproc_data_path(expected_table$source_file[idx])))
    }

    # deselecting (DT sends an empty integer vector, not NULL, when a
    # previously-selected row is clicked again to toggle it off) must clear
    # plot_result() back to NULL, not leave the last-selected row's plots
    # showing.
    session$setInputs(qc_table_rows_selected = integer(0))
    expect_null(selected_source_file())
    expect_null(plot_result())
  })
})

# ---------------------------------------------------------------------------
# P10-02: qc_threshold_config / default_qc_thresholds()
# ---------------------------------------------------------------------------

test_that("default_qc_thresholds() converts qc_threshold_config's 0-100 default_percent values to 0-1 fractions, one entry per unique threshold_id", {
  defaults <- default_qc_thresholds()

  expect_equal(names(defaults), c("valid_pct", "robust_pct", "interp_pct"))
  expect_equal(defaults$valid_pct, 0.80)
  expect_equal(defaults$robust_pct, 0.80)
  expect_equal(defaults$interp_pct, 0.20)
})

test_that("qc_threshold_config has exactly one row per (threshold_id, qc_metric) and interpolated_LeftEye/RightEye share one threshold_id", {
  # Pins down the "shared control" design: a regression that accidentally
  # split interp_pct into two separate threshold_ids would silently break the
  # single-numericInput-controls-both-eyes UI without any other test here
  # catching it.
  shared_rows <- qc_threshold_config[qc_threshold_config$threshold_id == "interp_pct", ]
  expect_equal(sort(shared_rows$qc_metric), c("interpolated_LeftEye", "interpolated_RightEye"))
  expect_true(all(shared_rows$direction == "max"))

  expect_equal(
    qc_threshold_config$direction[qc_threshold_config$qc_metric == "valid_raw_data"],
    "min"
  )
  expect_equal(
    qc_threshold_config$direction[qc_threshold_config$qc_metric == "robustness_proportion_valid_data_to_all_data"],
    "min"
  )
})

# ---------------------------------------------------------------------------
# P10-02: compute_qc_flags() -- boundary conditions, all 3 metrics, both
# directions
# ---------------------------------------------------------------------------
#
# Small hand-built tables throughout (qc_metric + percent only) rather than a
# real qcsummary table -- compute_qc_flags() only ever reads those two
# columns (see helpers.R), and real batch/eyeQuality() fixture output is
# uniformly 100% valid / 0% interpolated (too clean to naturally cross any
# default threshold), so a synthetic table is both sufficient and the only
# practical way to hit every boundary deliberately.

qc_flags_test_table <- function(valid_raw_data = 1, robustness = 1, interp_left = 0, interp_right = 0, blinks_both = 0.5) {
  data.frame(
    qc_metric = c(
      "valid_raw_data",
      "robustness_proportion_valid_data_to_all_data",
      "interpolated_LeftEye",
      "interpolated_RightEye",
      "blinks_BothEyes"
    ),
    percent = c(valid_raw_data, robustness, interp_left, interp_right, blinks_both),
    stringsAsFactors = FALSE
  )
}

test_that("compute_qc_flags flags valid_raw_data just below the min threshold, but not exactly at or just above it", {
  thresholds <- default_qc_thresholds() # valid_pct = 0.80

  below <- qc_flags_test_table(valid_raw_data = 0.799)
  at <- qc_flags_test_table(valid_raw_data = 0.80)
  above <- qc_flags_test_table(valid_raw_data = 0.801)

  expect_true(compute_qc_flags(below, thresholds)[1])
  expect_false(compute_qc_flags(at, thresholds)[1])
  expect_false(compute_qc_flags(above, thresholds)[1])
})

test_that("compute_qc_flags flags robustness_proportion_valid_data_to_all_data just below the min threshold, but not exactly at or just above it", {
  thresholds <- default_qc_thresholds() # robust_pct = 0.80

  below <- qc_flags_test_table(robustness = 0.799)
  at <- qc_flags_test_table(robustness = 0.80)
  above <- qc_flags_test_table(robustness = 0.801)

  expect_true(compute_qc_flags(below, thresholds)[2])
  expect_false(compute_qc_flags(at, thresholds)[2])
  expect_false(compute_qc_flags(above, thresholds)[2])
})

test_that("compute_qc_flags flags interpolated_LeftEye/RightEye just above the max threshold, but not exactly at or just below it", {
  thresholds <- default_qc_thresholds() # interp_pct = 0.20

  above <- qc_flags_test_table(interp_left = 0.201, interp_right = 0.201)
  at <- qc_flags_test_table(interp_left = 0.20, interp_right = 0.20)
  below <- qc_flags_test_table(interp_left = 0.199, interp_right = 0.199)

  expect_equal(compute_qc_flags(above, thresholds)[3:4], c(TRUE, TRUE))
  expect_equal(compute_qc_flags(at, thresholds)[3:4], c(FALSE, FALSE))
  expect_equal(compute_qc_flags(below, thresholds)[3:4], c(FALSE, FALSE))
})

test_that("compute_qc_flags's shared interp_pct threshold_id can flag one eye without the other", {
  thresholds <- default_qc_thresholds()
  tbl <- qc_flags_test_table(interp_left = 0.25, interp_right = 0.10)

  flags <- compute_qc_flags(tbl, thresholds)
  expect_true(flags[3]) # interpolated_LeftEye: 0.25 > 0.20
  expect_false(flags[4]) # interpolated_RightEye: 0.10 not > 0.20
})

test_that("compute_qc_flags never flags a qc_metric row that isn't in qc_threshold_config, regardless of value", {
  tbl <- qc_flags_test_table(blinks_both = 0.99)
  flags <- compute_qc_flags(tbl, default_qc_thresholds())
  expect_false(flags[5]) # blinks_BothEyes row
})

test_that("compute_qc_flags never flags a row whose percent value is NA, rather than propagating NA into the comparison", {
  tbl <- qc_flags_test_table(valid_raw_data = NA_real_)
  flags <- compute_qc_flags(tbl, default_qc_thresholds())
  expect_false(is.na(flags[1]))
  expect_false(flags[1])
})

test_that("compute_qc_flags skips a threshold_id entirely (no comparison made) when its threshold value is missing, NULL, or NA", {
  # value chosen to guarantee it WOULD flag under any real default threshold
  tbl <- qc_flags_test_table(valid_raw_data = 0.01)

  thresholds_missing_key <- default_qc_thresholds()
  thresholds_missing_key$valid_pct <- NULL
  expect_false(compute_qc_flags(tbl, thresholds_missing_key)[1])

  thresholds_na <- default_qc_thresholds()
  thresholds_na$valid_pct <- NA
  expect_false(compute_qc_flags(tbl, thresholds_na)[1])
})

# ---------------------------------------------------------------------------
# P10-02: compute_qc_flags() -- graceful degradation
# ---------------------------------------------------------------------------

test_that("compute_qc_flags returns a length-0 logical vector for a NULL table", {
  result <- compute_qc_flags(NULL, default_qc_thresholds())
  expect_type(result, "logical")
  expect_length(result, 0)
})

test_that("compute_qc_flags returns a length-0 logical vector for a 0-row table", {
  result <- compute_qc_flags(qc_flags_test_table()[0, ], default_qc_thresholds())
  expect_type(result, "logical")
  expect_length(result, 0)
})

test_that("compute_qc_flags returns an all-FALSE vector, not an error, when the table is missing the percent column", {
  tbl <- qc_flags_test_table()
  tbl$percent <- NULL
  result <- compute_qc_flags(tbl, default_qc_thresholds())
  expect_length(result, nrow(tbl))
  expect_true(all(!result))
})

test_that("compute_qc_flags returns an all-FALSE vector, not an error, when the table is missing the qc_metric column", {
  tbl <- qc_flags_test_table()
  tbl$qc_metric <- NULL
  result <- compute_qc_flags(tbl, default_qc_thresholds())
  expect_length(result, nrow(tbl))
  expect_true(all(!result))
})

# ---------------------------------------------------------------------------
# P10-02: live threshold changes drive app.R's reactive flagging end to end
# ---------------------------------------------------------------------------

test_that("changing a QC threshold numericInput live changes qc_thresholds() and qc_table_flagged()'s qc_flag output, including the interp_pct control shared by both eyes", {
  skip_on_cran()

  app_dir <- system.file("shiny-apps", "analyze", package = "eyeQuality")
  expect_true(nzchar(app_dir))

  # Hand-built qcsummary.tsv fixture (not a real batch run): real
  # eyeQuality()/eyeQualityBatch() fixture output is uniformly 100%
  # valid/0% interpolated and can't naturally cross any threshold in the
  # UI's 0-100 range, so this fixture is deliberately built with values that
  # DO cross the defaults, to prove the reactive wiring genuinely responds to
  # a live input change end to end (not just that compute_qc_flags() itself
  # works, which the tests above already cover in isolation).
  dir <- tempfile("p1002_testserver_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  dir <- normalizePath(dir, winslash = "/", mustWork = TRUE)

  readr::write_tsv(
    data.frame(
      qc_metric = c(
        "valid_raw_data",
        "robustness_proportion_valid_data_to_all_data",
        "interpolated_LeftEye",
        "interpolated_RightEye"
      ),
      percent = c(0.50, 0.90, 0.10, 0.10)
    ),
    file.path(dir, "sub-t_ses-1_recording-eyetracking_physio_desc-p1002ts_preproc_qcsummary.tsv")
  )

  home <- fs::path_home()
  rel <- sub(paste0("^", normalizePath(home, winslash = "/"), "/?"), "", dir)
  path_segments <- as.list(strsplit(rel, "/")[[1]])

  shiny::testServer(app_dir, {
    session$setInputs(
      directory = list(root = "Home", path = path_segments),
      recursiveSearch = FALSE
    )
    session$setInputs(load = 1)

    result <- load_result()
    expect_equal(result$n_files, 1L)

    # Before any threshold input has been touched, qc_thresholds() falls
    # back to default_qc_thresholds() (80/80/20%): valid_raw_data (0.50)
    # crosses (< 0.80), robustness (0.90) does not, interpolated (0.10, both
    # eyes) does not (not > 0.20).
    expect_equal(qc_thresholds()$valid_pct, 0.80)
    tbl0 <- qc_table_flagged()
    flags0 <- setNames(tbl0$qc_flag, tbl0$qc_metric)
    expect_true(flags0[["valid_raw_data"]])
    expect_false(flags0[["robustness_proportion_valid_data_to_all_data"]])
    expect_false(flags0[["interpolated_LeftEye"]])
    expect_false(flags0[["interpolated_RightEye"]])
    expect_equal(nrow(flagged_recordings()), 1)

    # Lower the valid_pct threshold below the fixture's 0.50 value: the
    # reactive itself must pick up the new value, and valid_raw_data must
    # stop crossing.
    session$setInputs(qc_threshold_valid_pct = 40)
    expect_equal(qc_thresholds()$valid_pct, 0.40)
    tbl1 <- qc_table_flagged()
    flags1 <- setNames(tbl1$qc_flag, tbl1$qc_metric)
    expect_false(flags1[["valid_raw_data"]])
    expect_equal(nrow(flagged_recordings()), 0)

    # Tighten the shared interp_pct threshold below the fixture's 0.10 value:
    # both interpolated_LeftEye AND interpolated_RightEye must start crossing
    # together, confirming the one-control-drives-both-eyes design is wired
    # through to the live reactive, not just qc_threshold_config's data shape.
    session$setInputs(qc_threshold_interp_pct = 5)
    expect_equal(qc_thresholds()$interp_pct, 0.05)
    tbl2 <- qc_table_flagged()
    flags2 <- setNames(tbl2$qc_flag, tbl2$qc_metric)
    expect_true(flags2[["interpolated_LeftEye"]])
    expect_true(flags2[["interpolated_RightEye"]])
    expect_false(flags2[["valid_raw_data"]])
    expect_equal(nrow(flagged_recordings()), 1)

    # Clearing the numericInput back to NA (user deletes the box's contents)
    # must revert that threshold_id to its documented default rather than
    # disabling flagging for it.
    session$setInputs(qc_threshold_valid_pct = NA)
    expect_equal(qc_thresholds()$valid_pct, 0.80)
    tbl3 <- qc_table_flagged()
    flags3 <- setNames(tbl3$qc_flag, tbl3$qc_metric)
    expect_true(flags3[["valid_raw_data"]])
  })
})
