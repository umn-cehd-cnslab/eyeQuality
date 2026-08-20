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

# ---------------------------------------------------------------------------
# P10-07: save/load QC thresholds via the shared batch_config.yaml
# ---------------------------------------------------------------------------
#
# helpers.R-level tests first (filter_recognized_qc_thresholds(),
# qc_thresholds_to_percent() -- Shiny-free), then live app.R save/load flows
# via shiny::testServer(), following the same shinyDirButton/shinySaveButton
# simulation technique and MockShinySession$sendInputMessage() no-op
# workaround established in test-runSetupApp.R's own P9-04 tests (see that
# file's header comment for the full explanation of the limitation).

# ---------------------------------------------------------------------------
# filter_recognized_qc_thresholds()
# ---------------------------------------------------------------------------

test_that("filter_recognized_qc_thresholds keeps every recognized, in-range entry and drops nothing when the input is already clean", {
  result <- filter_recognized_qc_thresholds(list(valid_pct = 72, robust_pct = 58, interp_pct = 22))

  expect_equal(result$kept, list(valid_pct = 72, robust_pct = 58, interp_pct = 22))
  expect_length(result$dropped, 0)
})

test_that("filter_recognized_qc_thresholds drops an unrecognized threshold_id but keeps the recognized entries alongside it", {
  result <- filter_recognized_qc_thresholds(list(valid_pct = 72, not_a_real_metric = 40))

  expect_equal(result$kept, list(valid_pct = 72))
  expect_equal(result$dropped, "not_a_real_metric")
})

test_that("filter_recognized_qc_thresholds drops a recognized id whose value is out of the 0-100 range", {
  result <- filter_recognized_qc_thresholds(list(robust_pct = 150, interp_pct = -5))

  expect_length(result$kept, 0)
  expect_setequal(result$dropped, c("robust_pct", "interp_pct"))
})

test_that("filter_recognized_qc_thresholds drops a recognized id whose value is non-numeric or NA", {
  result <- filter_recognized_qc_thresholds(list(valid_pct = "eighty", robust_pct = NA_real_))

  expect_length(result$kept, 0)
  expect_setequal(result$dropped, c("valid_pct", "robust_pct"))
})

test_that("filter_recognized_qc_thresholds returns empty kept/dropped for NULL or a zero-length input, never an error", {
  result_null <- filter_recognized_qc_thresholds(NULL)
  expect_equal(result_null, list(kept = list(), dropped = character(0)))

  result_empty <- filter_recognized_qc_thresholds(list())
  expect_equal(result_empty, list(kept = list(), dropped = character(0)))
})

test_that("filter_recognized_qc_thresholds does not raise an error the way validate_batch_config's strict path would on the same bad input", {
  # The whole point of this function existing separately from
  # validate_batch_config() (R/batchConfig.R): a hand-edited file with a typo
  # or out-of-range value must not block the Analyze app's "Load config" flow
  # the way it correctly blocks write_batch_config()/a strict re-save.
  bad_input <- list(valid_pct = 999, made_up_id = 10)

  expect_error(
    eyeQuality::validate_batch_config(list(
      schemaVersion = 1, batchName = "x", directoryBIDS = "/d",
      displayDimensionX_mm = 1, displayDimensionY_mm = 1,
      qcThresholds = bad_input
    )),
    "qcThresholds"
  )
  expect_no_error(filter_recognized_qc_thresholds(bad_input))
})

# ---------------------------------------------------------------------------
# qc_thresholds_to_percent()
# ---------------------------------------------------------------------------

test_that("qc_thresholds_to_percent converts a named list of 0-1 fractions to the 0-100 percentage scale batch_config.yaml stores", {
  result <- qc_thresholds_to_percent(list(valid_pct = 0.72, robust_pct = 0.5, interp_pct = 0.2))

  expect_equal(result, list(valid_pct = 72, robust_pct = 50, interp_pct = 20))
})

test_that("qc_thresholds_to_percent maps a NULL or NA fraction to NA_real_ rather than erroring", {
  result <- qc_thresholds_to_percent(list(valid_pct = NULL, robust_pct = NA))
  expect_equal(result$robust_pct, NA_real_)
})

# ---------------------------------------------------------------------------
# Live app.R save/load flows (shiny::testServer())
# ---------------------------------------------------------------------------

analyze_app_dir <- system.file("shiny-apps", "analyze", package = "eyeQuality")
if (!nzchar(analyze_app_dir)) {
  stop("test-runAnalyzeApp.R: could not locate inst/shiny-apps/analyze/ via system.file()")
}
setup_app_dir_p1007 <- system.file("shiny-apps", "setup", package = "eyeQuality")
if (!nzchar(setup_app_dir_p1007)) {
  stop("test-runAnalyzeApp.R: could not locate inst/shiny-apps/setup/ via system.file()")
}

# rel_home_segments: converts an absolute path under fs::path_home() into the
# root/path-segment list shinyFiles::shinyDirChoose()/shinyFileSave()
# selections use, so a real directory/save-target can be simulated via
# session$setInputs() without a live browser picker. Duplicated locally
# (rather than shared with test-runSetupApp.R) per this repo's test-file
# self-containedness convention -- see e.g. copy_bids_fixture_tree() above.
rel_home_segments_p1007 <- function(path) {
  home <- normalizePath(fs::path_home(), winslash = "/")
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  testthat::skip_if_not(
    startsWith(path, home),
    "test tempdir is not under fs::path_home(); shinyFiles root-relative simulation would not resolve"
  )
  rel <- sub(paste0("^", home, "/?"), "", path)
  as.list(strsplit(rel, "/")[[1]])
}

test_that("Analyze app fresh save: study info + live thresholds round-trip through write_batch_config()/read_batch_config()", {
  skip_on_cran()

  save_dir <- tempfile("p1007_fresh_save_dest_")
  dir.create(save_dir, recursive = TRUE)
  on.exit(unlink(save_dir, recursive = TRUE), add = TRUE)
  save_segments <- rel_home_segments_p1007(save_dir)
  save_path <- file.path(normalizePath(save_dir, winslash = "/"), "fresh.yaml")

  shiny::testServer(analyze_app_dir, {
    session$setInputs(
      cfg_batchName = "analyze_fresh_save",
      cfg_directoryBIDS = "/data/analyze_fresh",
      cfg_displayDimensionX_mm = 601,
      cfg_displayDimensionY_mm = 401,
      qc_threshold_valid_pct = 70,
      qc_threshold_robust_pct = 55,
      qc_threshold_interp_pct = 25
    )

    session$setInputs(save_config = list(
      root = "Home", path = save_segments, name = "fresh.yaml", type = "yaml"
    ))

    status <- config_io_status()
    expect_true(status$ok)
  })

  expect_true(file.exists(save_path))
  loaded <- read_batch_config(save_path)
  expect_equal(loaded$batchName, "analyze_fresh_save")
  expect_equal(loaded$directoryBIDS, "/data/analyze_fresh")
  expect_equal(loaded$displayDimensionX_mm, 601)
  expect_equal(loaded$displayDimensionY_mm, 401)
  expect_equal(loaded$qcThresholds$valid_pct, 70)
  expect_equal(loaded$qcThresholds$robust_pct, 55)
  expect_equal(loaded$qcThresholds$interp_pct, 25)
  # no config was ever loaded this session, so run-parameter fields the
  # Analyze app's own form doesn't expose fall back to default_batch_config()
  expect_equal(loaded$layout, "bids")
  expect_null(loaded$adapterType)
})

test_that("Analyze app fresh save with no study info filled in fails cleanly via validate_batch_config(), no partial file written", {
  skip_on_cran()

  save_dir <- tempfile("p1007_fresh_fail_dest_")
  dir.create(save_dir, recursive = TRUE)
  on.exit(unlink(save_dir, recursive = TRUE), add = TRUE)
  save_segments <- rel_home_segments_p1007(save_dir)
  save_path <- file.path(normalizePath(save_dir, winslash = "/"), "fresh_fail.yaml")

  shiny::testServer(analyze_app_dir, {
    # deliberately leave cfg_batchName/cfg_directoryBIDS/cfg_displayDimension*_mm
    # at their blank/NA ui defaults -- only touch the thresholds
    session$setInputs(qc_threshold_valid_pct = 65)

    session$setInputs(save_config = list(
      root = "Home", path = save_segments, name = "fresh_fail.yaml", type = "yaml"
    ))

    status <- config_io_status()
    expect_false(status$ok)
    expect_match(status$message, "batchName", fixed = TRUE)
    expect_match(status$message, "directoryBIDS", fixed = TRUE)
    expect_match(status$message, "displayDimensionX_mm", fixed = TRUE)
    expect_match(status$message, "displayDimensionY_mm", fixed = TRUE)
  })

  expect_false(file.exists(save_path))
})

test_that("Analyze app load-Setup-only-config-then-resave: run params survive untouched, only the tweaked threshold changes", {
  skip_on_cran()

  data_dir <- tempfile("p1007_loadresave_data_")
  dir.create(data_dir, recursive = TRUE)
  on.exit(unlink(data_dir, recursive = TRUE), add = TRUE)

  # A config with no qcThresholds section at all -- exactly what the Setup
  # app alone would have produced (P9-04), predating this task.
  loaded_cfg_path <- tempfile("p1007_loadresave_cfg_", fileext = ".yaml")
  on.exit(unlink(loaded_cfg_path), add = TRUE)
  write_batch_config(
    list(
      batchName = "setup_only_run",
      directoryBIDS = normalizePath(data_dir, winslash = "/"),
      layout = "glob",
      pathPattern = "sub-*/**/*.tsv",
      excludePattern_regex = "deriv",
      modalityPattern_regex = "gaze",
      adapterType = "TobiiStudio",
      numberCores = 6,
      eyeSelection_method = "Left",
      validityThreshold = 0.55,
      outputDir = "/custom/out",
      displayDimensionX_mm = 555,
      displayDimensionY_mm = 333
    ),
    loaded_cfg_path
  )

  save_dir <- tempfile("p1007_loadresave_dest_")
  dir.create(save_dir, recursive = TRUE)
  on.exit(unlink(save_dir, recursive = TRUE), add = TRUE)
  save_segments <- rel_home_segments_p1007(save_dir)
  save_path <- file.path(normalizePath(save_dir, winslash = "/"), "resaved.yaml")

  shiny::testServer(analyze_app_dir, {
    session$setInputs(load_config_file = data.frame(
      name = "setup_only_run.yaml",
      datapath = loaded_cfg_path,
      stringsAsFactors = FALSE
    ))

    load_status <- config_io_status()
    expect_true(load_status$ok)

    extra <- loaded_config_extra()
    expect_equal(extra$layout, "glob")
    expect_equal(extra$pathPattern, "sub-*/**/*.tsv")
    expect_equal(extra$adapterType, "TobiiStudio")
    expect_equal(extra$numberCores, 6)
    expect_equal(extra$eyeSelection_method, "Left")
    expect_equal(extra$validityThreshold, 0.55)
    expect_equal(extra$outputDir, "/custom/out")
    # no qcThresholds section in the loaded file -> filtered to an empty kept set
    expect_equal(extra$qcThresholds, list())

    # See this file's header comment: update*Input() calls the load handler
    # just made are no-ops in this harness, so input$cfg_batchName/
    # cfg_directoryBIDS/cfg_displayDimension*_mm never actually changed here
    # the way a real browser would have reflected them. Set them explicitly to
    # what the config just loaded, standing in for that reflection, so the
    # resave below can succeed (validate_batch_config() requires all four) --
    # this does not touch layout/pathPattern/adapterType/etc., which is the
    # actual behavior under test.
    session$setInputs(
      cfg_batchName = "setup_only_run",
      cfg_directoryBIDS = normalizePath(data_dir, winslash = "/"),
      cfg_displayDimensionX_mm = 555,
      cfg_displayDimensionY_mm = 333
    )

    # tweak exactly one threshold
    session$setInputs(qc_threshold_interp_pct = 12)

    session$setInputs(save_config = list(
      root = "Home", path = save_segments, name = "resaved.yaml", type = "yaml"
    ))
    save_status <- config_io_status()
    expect_true(save_status$ok)
  })

  expect_true(file.exists(save_path))
  resaved <- read_batch_config(save_path)
  expect_equal(resaved$batchName, "setup_only_run")
  expect_equal(resaved$layout, "glob")
  expect_equal(resaved$pathPattern, "sub-*/**/*.tsv")
  expect_equal(resaved$excludePattern_regex, "deriv")
  expect_equal(resaved$modalityPattern_regex, "gaze")
  expect_equal(resaved$adapterType, "TobiiStudio")
  expect_equal(resaved$numberCores, 6)
  expect_equal(resaved$eyeSelection_method, "Left")
  expect_equal(resaved$validityThreshold, 0.55)
  expect_equal(resaved$outputDir, "/custom/out")
  expect_equal(resaved$displayDimensionX_mm, 555)
  expect_equal(resaved$displayDimensionY_mm, 333)
  # tweaked threshold shows the new value...
  expect_equal(resaved$qcThresholds$interp_pct, 12)
  # ...while untouched thresholds fall back to their documented defaults
  # (80/80), not to something left over from the loaded config (which had none)
  expect_equal(resaved$qcThresholds$valid_pct, 80)
  expect_equal(resaved$qcThresholds$robust_pct, 80)
})

test_that("Analyze app load-with-unrecognized-key: dropped gracefully with a status message, recognized key still carried forward for resave", {
  skip_on_cran()

  # Hand-edited config (not written via write_batch_config(), which would
  # refuse this outright) with one recognized-and-sane entry, one
  # unrecognized threshold_id, and one recognized id with an out-of-range
  # value -- both of the latter two must be dropped, not just the first.
  hand_edited_path <- tempfile("p1007_handedited_", fileext = ".yaml")
  on.exit(unlink(hand_edited_path), add = TRUE)
  yaml::write_yaml(list(
    schemaVersion = 1,
    batchName = "hand_edited_run",
    directoryBIDS = "/data/hand_edited",
    displayDimensionX_mm = 200,
    displayDimensionY_mm = 150,
    qcThresholds = list(valid_pct = 66, made_up_metric = 999, robust_pct = 150)
  ), hand_edited_path)

  save_dir <- tempfile("p1007_handedited_dest_")
  dir.create(save_dir, recursive = TRUE)
  on.exit(unlink(save_dir, recursive = TRUE), add = TRUE)
  save_segments <- rel_home_segments_p1007(save_dir)
  save_path <- file.path(normalizePath(save_dir, winslash = "/"), "handedited_resaved.yaml")

  shiny::testServer(analyze_app_dir, {
    session$setInputs(load_config_file = data.frame(
      name = "hand_edited.yaml",
      datapath = hand_edited_path,
      stringsAsFactors = FALSE
    ))

    load_status <- config_io_status()
    expect_true(load_status$ok) # the load itself still succeeds overall
    expect_match(load_status$message, "made_up_metric", fixed = TRUE)
    expect_match(load_status$message, "robust_pct", fixed = TRUE)

    extra <- loaded_config_extra()
    expect_equal(extra$qcThresholds, list(valid_pct = 66))

    # See the file-level workaround comment above: reflect what a real
    # browser's update*Input() calls would have set for the one entry that
    # WAS kept, since MockShinySession's sendInputMessage() is a no-op here.
    session$setInputs(
      cfg_batchName = "hand_edited_run",
      cfg_directoryBIDS = "/data/hand_edited",
      cfg_displayDimensionX_mm = 200,
      cfg_displayDimensionY_mm = 150,
      qc_threshold_valid_pct = 66
    )

    session$setInputs(save_config = list(
      root = "Home", path = save_segments, name = "handedited_resaved.yaml", type = "yaml"
    ))
    save_status <- config_io_status()
    expect_true(save_status$ok)
  })

  resaved <- read_batch_config(save_path)
  # the recognized/sane entry survived the drop-and-resave round trip...
  expect_equal(resaved$qcThresholds$valid_pct, 66)
  # ...while the dropped entries are nowhere in the resaved file: robust_pct
  # falls back to its documented default (80), and made_up_metric never
  # existed as a recognized key to begin with
  expect_equal(resaved$qcThresholds$robust_pct, 80)
  expect_false("made_up_metric" %in% names(resaved$qcThresholds))
})

test_that("Cross-app round trip: Setup app saves a config, Analyze app loads/tweaks/resaves it in place, Setup app reloads it with its own fields unaffected", {
  skip_on_cran()

  data_dir <- tempfile("p1007_crossapp_data_")
  dir.create(data_dir, recursive = TRUE)
  on.exit(unlink(data_dir, recursive = TRUE), add = TRUE)
  data_segments <- rel_home_segments_p1007(data_dir)

  shared_dir <- tempfile("p1007_crossapp_shared_")
  dir.create(shared_dir, recursive = TRUE)
  on.exit(unlink(shared_dir, recursive = TRUE), add = TRUE)
  shared_segments <- rel_home_segments_p1007(shared_dir)
  shared_path <- file.path(normalizePath(shared_dir, winslash = "/"), "shared.yaml")

  # --- Step 1: Setup app saves the initial config -----------------------
  shiny::testServer(setup_app_dir_p1007, {
    session$setInputs(directory = list(root = "Home", path = data_segments))
    session$setInputs(
      layout = "glob",
      pathPattern = "**/*.tsv",
      excludePattern_regex = "deriv",
      modalityPattern_regex = "gaze",
      displayDimensionX_mm = 611,
      displayDimensionY_mm = 411,
      eyeSelection_method = "Strict",
      validityThreshold = 0.6,
      outputDir = "",
      batchName = "crossapp_study"
    )
    session$setInputs(save_config = list(
      root = "Home", path = shared_segments, name = "shared.yaml", type = "yaml"
    ))
    expect_true(config_io_status()$ok)
  })

  expect_true(file.exists(shared_path))
  after_setup_save <- read_batch_config(shared_path)
  expect_null(after_setup_save$qcThresholds)

  # --- Step 2: Analyze app loads it, tweaks thresholds, resaves in place --
  shiny::testServer(analyze_app_dir, {
    session$setInputs(load_config_file = data.frame(
      name = "shared.yaml",
      datapath = shared_path,
      stringsAsFactors = FALSE
    ))
    expect_true(config_io_status()$ok)

    # reflect the loaded study-info fields (see no-op workaround comment above)
    session$setInputs(
      cfg_batchName = "crossapp_study",
      cfg_directoryBIDS = after_setup_save$directoryBIDS,
      cfg_displayDimensionX_mm = 611,
      cfg_displayDimensionY_mm = 411,
      qc_threshold_valid_pct = 90,
      qc_threshold_robust_pct = 85,
      qc_threshold_interp_pct = 10
    )

    session$setInputs(save_config = list(
      root = "Home", path = shared_segments, name = "shared.yaml", type = "yaml"
    ))
    expect_true(config_io_status()$ok)
  })

  after_analyze_save <- read_batch_config(shared_path)
  expect_equal(after_analyze_save$qcThresholds$valid_pct, 90)
  expect_equal(after_analyze_save$qcThresholds$robust_pct, 85)
  expect_equal(after_analyze_save$qcThresholds$interp_pct, 10)
  # Setup app's own fields must be exactly what step 1 wrote, unaffected by
  # the Analyze app's resave.
  expect_equal(after_analyze_save$layout, "glob")
  expect_equal(after_analyze_save$pathPattern, "**/*.tsv")
  expect_equal(after_analyze_save$excludePattern_regex, "deriv")
  expect_equal(after_analyze_save$modalityPattern_regex, "gaze")
  expect_equal(after_analyze_save$eyeSelection_method, "Strict")
  expect_equal(after_analyze_save$validityThreshold, 0.6)
  expect_equal(after_analyze_save$batchName, "crossapp_study")

  # --- Step 3: Setup app re-loads the same file; its own fields survive ---
  shiny::testServer(setup_app_dir_p1007, {
    session$setInputs(load_config_file = data.frame(
      name = "shared.yaml",
      datapath = shared_path,
      stringsAsFactors = FALSE
    ))

    reload_status <- config_io_status()
    expect_true(reload_status$ok)

    extra <- loaded_config_extra()
    expect_equal(extra$layout, "glob")
    expect_equal(extra$pathPattern, "**/*.tsv")
    expect_equal(extra$excludePattern_regex, "deriv")
    expect_equal(extra$modalityPattern_regex, "gaze")
    expect_equal(extra$eyeSelection_method, "Strict")
    expect_equal(extra$validityThreshold, 0.6)
    expect_equal(extra$batchName, "crossapp_study")
    expect_equal(extra$displayDimensionX_mm, 611)
    expect_equal(extra$displayDimensionY_mm, 411)
    # the Analyze app's qcThresholds edits are still present in the config
    # this app doesn't itself expose a form for, unaffected by this reload
    expect_equal(extra$qcThresholds$valid_pct, 90)
  })
})

# ---------------------------------------------------------------------------
# P10-04: build_qc_comparison_plot() / build_qc_comparison_table()
# ---------------------------------------------------------------------------
#
# Small hand-built long-format tables throughout (recording/batch_name/
# source_file/qc_metric/percent[/qc_flag]) rather than a real qcsummary
# table -- both functions only ever read those columns (see helpers.R), and
# a synthetic table lets deliberately chosen values cross/not cross
# thresholds and exercise both thresholdable and non-thresholdable metrics,
# which real batch/eyeQuality() fixture output (uniformly 100% valid/0%
# interpolated) can't do.

# comparison_test_table: builds a synthetic table_flagged-shaped data.frame
# for n recordings, all reporting the same qc_metric. qc_flags defaults to
# all-FALSE (the correct compute_qc_flags() output for a non-thresholdable
# metric, or a thresholdable metric whose values don't cross), matching this
# file's qc_flags_test_table() convention above.
comparison_test_table <- function(metric, percent, qc_flags = NULL) {
  n <- length(percent)
  if (is.null(qc_flags)) {
    qc_flags <- rep(FALSE, n)
  }
  data.frame(
    recording = paste0("rec-", seq_len(n)),
    batch_name = "test_batch",
    source_file = paste0("/data/rec-", seq_len(n), "_qcsummary.tsv"),
    qc_metric = metric,
    percent = percent,
    qc_flag = qc_flags,
    stringsAsFactors = FALSE
  )
}

# find_hline_layer: returns the first geom_hline() ggplot2 layer in `plot`,
# or NULL if none is present -- geom_hline(yintercept = ...)'s value is
# stored on the layer's own `data` slot (a 1-row data.frame with a
# yintercept column), not as a mapped aesthetic, confirmed directly against
# ggplot2's own layer construction behavior rather than assumed.
find_hline_layer <- function(plot) {
  for (l in plot$layers) {
    if (inherits(l$geom, "GeomHline")) {
      return(l)
    }
  }
  NULL
}

test_that("build_qc_comparison_plot returns a ggplot object with one bar row per matching recording", {
  tbl <- comparison_test_table("valid_raw_data", c(0.9, 0.5, 0.7))
  plot <- build_qc_comparison_plot(tbl, "valid_raw_data", default_qc_thresholds())

  expect_s3_class(plot, "ggplot")
  expect_equal(nrow(plot$data), 3)
})

test_that("build_qc_comparison_plot colors bars Flagged/OK matching table_flagged's own qc_flag column for a thresholdable metric", {
  tbl <- comparison_test_table("valid_raw_data", c(0.9, 0.5, 0.7), qc_flags = c(FALSE, TRUE, FALSE))
  plot <- build_qc_comparison_plot(tbl, "valid_raw_data", default_qc_thresholds())

  expect_equal(
    as.character(plot$data$comparison_status),
    c("OK", "Flagged", "OK")
  )
})

test_that("build_qc_comparison_plot marks a metric with no configured threshold as 'No threshold configured' regardless of qc_flag", {
  # blinks_BothEyes is not in qc_threshold_config (see helpers.R's own
  # comment on which 3 metrics are thresholdable) -- compute_qc_flags()
  # never sets qc_flag TRUE for it, but this pins down that even if qc_flag
  # were somehow TRUE here, the plot still reports the distinct third status
  # rather than miscategorizing it as "OK".
  tbl <- comparison_test_table("blinks_BothEyes", c(0.1, 0.9), qc_flags = c(FALSE, TRUE))
  plot <- build_qc_comparison_plot(tbl, "blinks_BothEyes", default_qc_thresholds())

  expect_equal(
    as.character(plot$data$comparison_status),
    c("No threshold configured", "No threshold configured")
  )
})

test_that("build_qc_comparison_plot draws a dashed threshold reference line at the configured threshold's live value", {
  tbl <- comparison_test_table("valid_raw_data", c(0.9, 0.5))
  thresholds <- default_qc_thresholds()
  thresholds$valid_pct <- 0.65

  plot <- build_qc_comparison_plot(tbl, "valid_raw_data", thresholds)
  hline <- find_hline_layer(plot)

  expect_false(is.null(hline))
  expect_equal(hline$data$yintercept, 0.65)
})

test_that("build_qc_comparison_plot omits the threshold reference line for a metric with no configured threshold", {
  tbl <- comparison_test_table("blinks_BothEyes", c(0.1, 0.9))
  plot <- build_qc_comparison_plot(tbl, "blinks_BothEyes", default_qc_thresholds())

  expect_true(is.null(find_hline_layer(plot)))
})

test_that("build_qc_comparison_plot omits the threshold reference line when the configured threshold's value is NA", {
  tbl <- comparison_test_table("valid_raw_data", c(0.9, 0.5))
  thresholds <- default_qc_thresholds()
  thresholds$valid_pct <- NA

  plot <- build_qc_comparison_plot(tbl, "valid_raw_data", thresholds)

  expect_true(is.null(find_hline_layer(plot)))
})

test_that("build_qc_comparison_plot returns NULL when the metric has no matching rows in the table", {
  tbl <- comparison_test_table("valid_raw_data", c(0.9, 0.5))
  plot <- build_qc_comparison_plot(tbl, "some_metric_not_in_table", default_qc_thresholds())

  expect_null(plot)
})

test_that("build_qc_comparison_plot returns NULL for a NULL table, NULL metric, or empty-string metric, rather than erroring", {
  tbl <- comparison_test_table("valid_raw_data", c(0.9, 0.5))
  thresholds <- default_qc_thresholds()

  expect_null(build_qc_comparison_plot(NULL, "valid_raw_data", thresholds))
  expect_null(build_qc_comparison_plot(tbl, NULL, thresholds))
  expect_null(build_qc_comparison_plot(tbl, "", thresholds))
})

test_that("build_qc_comparison_table pivots a long qcsummary table wide, one row per file and one column per selected metric, with correct values", {
  long_tbl <- rbind(
    comparison_test_table("valid_raw_data", c(0.9, 0.5)),
    comparison_test_table("robustness_proportion_valid_data_to_all_data", c(0.95, 0.55))
  )
  # rows above were built independently, so give them matching recording ids
  # across the two metrics the way a real combined table would
  long_tbl$recording <- rep(c("rec-1", "rec-2"), 2)
  long_tbl$source_file <- rep(c("/data/rec-1_qcsummary.tsv", "/data/rec-2_qcsummary.tsv"), 2)

  result <- build_qc_comparison_table(
    long_tbl,
    c("valid_raw_data", "robustness_proportion_valid_data_to_all_data")
  )

  expect_equal(nrow(result), 2)
  expect_setequal(
    colnames(result),
    c("recording", "batch_name", "source_file", "valid_raw_data", "robustness_proportion_valid_data_to_all_data")
  )

  by_recording <- setNames(seq_len(nrow(result)), result$recording)
  expect_equal(result$valid_raw_data[by_recording[["rec-1"]]], 0.9)
  expect_equal(result$valid_raw_data[by_recording[["rec-2"]]], 0.5)
  expect_equal(result$robustness_proportion_valid_data_to_all_data[by_recording[["rec-1"]]], 0.95)
  expect_equal(result$robustness_proportion_valid_data_to_all_data[by_recording[["rec-2"]]], 0.55)
})

test_that("build_qc_comparison_table returns NULL for an empty metric selection", {
  tbl <- comparison_test_table("valid_raw_data", c(0.9, 0.5))
  expect_null(build_qc_comparison_table(tbl, character(0)))
})

test_that("build_qc_comparison_table returns NULL for a NULL or 0-row input table", {
  tbl <- comparison_test_table("valid_raw_data", c(0.9, 0.5))
  expect_null(build_qc_comparison_table(NULL, "valid_raw_data"))
  expect_null(build_qc_comparison_table(tbl[0, ], "valid_raw_data"))
})

test_that("build_qc_comparison_table returns NULL when none of the selected metrics have any matching rows", {
  tbl <- comparison_test_table("valid_raw_data", c(0.9, 0.5))
  expect_null(build_qc_comparison_table(tbl, "some_metric_not_in_table"))
})

test_that("build_qc_comparison_table keeps only the first value for a duplicate recording/qc_metric combination rather than nesting a list-column", {
  dup_tbl <- comparison_test_table("valid_raw_data", c(0.9, 0.9))
  # id_cols for pivot_wider() is recording/batch_name/source_file together --
  # all three must match for this to be a genuine duplicate id/metric
  # combination, not just a same-named recording from two distinct files.
  dup_tbl$recording <- c("rec-1", "rec-1")
  dup_tbl$source_file <- c("/data/rec-1_qcsummary.tsv", "/data/rec-1_qcsummary.tsv")
  dup_tbl$percent <- c(0.9, 0.3) # two different values reported for the same recording/metric

  result <- build_qc_comparison_table(dup_tbl, "valid_raw_data")

  expect_equal(nrow(result), 1)
  expect_equal(result$valid_raw_data, 0.9)
  expect_type(result$valid_raw_data, "double")
})

# ---------------------------------------------------------------------------
# P10-05: export "flagged for review" file list -- regression tests for
# build_flagged_export_table() (helpers.R).
# ---------------------------------------------------------------------------
#
# flagged_export_test_table: builds a synthetic table_flagged-shaped
# data.frame directly from parallel vectors, same required-column shape as
# comparison_test_table() above but general enough to mix several qc_metric
# rows (and mixed qc_flag values) per recording -- exactly what
# build_flagged_export_table()'s per-recording rollup needs to be exercised
# against.
flagged_export_test_table <- function(recording, batch_name, source_file, qc_metric, percent, qc_flag) {
  data.frame(
    recording = recording,
    batch_name = batch_name,
    source_file = source_file,
    qc_metric = qc_metric,
    percent = percent,
    qc_flag = qc_flag,
    stringsAsFactors = FALSE
  )
}

test_that("build_flagged_export_table rolls up two flagged metrics sharing one threshold_id into a correct n_flagged_metrics/flagged_metrics/flagged_values row", {
  # interpolated_LeftEye/RightEye share the interp_pct threshold_id (see
  # qc_threshold_config in helpers.R) -- both flagged here for the same
  # recording is the scenario n_flagged_metrics == 2 needs to get right.
  tbl <- flagged_export_test_table(
    recording = "rec-1",
    batch_name = "test_batch",
    source_file = "/data/rec-1_qcsummary.tsv",
    qc_metric = c("interpolated_LeftEye", "interpolated_RightEye"),
    percent = c(0.25, 0.30),
    qc_flag = c(TRUE, TRUE)
  )

  result <- build_flagged_export_table(tbl)

  expect_equal(nrow(result), 1)
  expect_equal(result$recording, "rec-1")
  expect_equal(result$n_flagged_metrics, 2)
  expect_equal(result$flagged_metrics, "interpolated_LeftEye, interpolated_RightEye")
  expect_equal(result$flagged_values, "25, 30")
})

test_that("build_flagged_export_table produces a single value with no stray comma when a recording is flagged on exactly one metric", {
  tbl <- flagged_export_test_table(
    recording = "rec-2",
    batch_name = "test_batch",
    source_file = "/data/rec-2_qcsummary.tsv",
    qc_metric = "valid_raw_data",
    percent = 0.65,
    qc_flag = TRUE
  )

  result <- build_flagged_export_table(tbl)

  expect_equal(nrow(result), 1)
  expect_equal(result$n_flagged_metrics, 1)
  expect_equal(result$flagged_metrics, "valid_raw_data")
  expect_false(grepl(",", result$flagged_metrics, fixed = TRUE))
  expect_equal(result$flagged_values, "65")
  expect_false(grepl(",", result$flagged_values, fixed = TRUE))
})

test_that("build_flagged_export_table omits a recording with zero flagged rows entirely", {
  tbl <- rbind(
    flagged_export_test_table(
      recording = "rec-1", batch_name = "test_batch", source_file = "/data/rec-1_qcsummary.tsv",
      qc_metric = "valid_raw_data", percent = 0.5, qc_flag = TRUE
    ),
    flagged_export_test_table(
      recording = "rec-2", batch_name = "test_batch", source_file = "/data/rec-2_qcsummary.tsv",
      qc_metric = c("valid_raw_data", "robustness_proportion_valid_data_to_all_data"),
      percent = c(0.9, 0.95),
      qc_flag = c(FALSE, FALSE)
    )
  )

  result <- build_flagged_export_table(tbl)

  expect_equal(nrow(result), 1)
  expect_equal(result$recording, "rec-1")
  expect_false("rec-2" %in% result$recording)
})

test_that("build_flagged_export_table returns NULL for NULL, 0-row, malformed (missing required column), or all-unflagged input rather than erroring", {
  base_tbl <- flagged_export_test_table(
    recording = "rec-1", batch_name = "test_batch", source_file = "/data/rec-1_qcsummary.tsv",
    qc_metric = "valid_raw_data", percent = 0.5, qc_flag = TRUE
  )

  expect_null(build_flagged_export_table(NULL))
  expect_null(build_flagged_export_table(base_tbl[0, ]))

  missing_col <- base_tbl
  missing_col$qc_flag <- NULL
  expect_null(build_flagged_export_table(missing_col))

  all_unflagged <- base_tbl
  all_unflagged$qc_flag <- FALSE
  expect_null(build_flagged_export_table(all_unflagged))
})

test_that("build_flagged_export_table excludes unflagged rows from the count/metrics/values even when mixed with flagged rows for the same recording", {
  tbl <- flagged_export_test_table(
    recording = "rec-1",
    batch_name = "test_batch",
    source_file = "/data/rec-1_qcsummary.tsv",
    qc_metric = c("valid_raw_data", "robustness_proportion_valid_data_to_all_data", "interpolated_LeftEye"),
    percent = c(0.5, 0.6, 0.05),
    qc_flag = c(TRUE, TRUE, FALSE)
  )

  result <- build_flagged_export_table(tbl)

  expect_equal(nrow(result), 1)
  expect_equal(result$n_flagged_metrics, 2)
  expect_equal(result$flagged_metrics, "valid_raw_data, robustness_proportion_valid_data_to_all_data")
  expect_false(grepl("interpolated_LeftEye", result$flagged_metrics, fixed = TRUE))
  expect_equal(result$flagged_values, "50, 60")
})
