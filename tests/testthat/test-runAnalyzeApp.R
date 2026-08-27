# P10-01: regression tests for the Analyze / QC Explorer tabs' Shiny-free
# helpers (inst/shiny-apps/app/analyze_helpers.R) -- discover_qcsummary_files(),
# derive_recording_label(), derive_batch_name(), build_zero_match_diagnostic(),
# read_one_qcsummary(), and load_qcsummary_table(). Sourced via system.file()
# rather than a relative path, same convention as test-runSetupApp.R, both
# because that's the only way to reach inst/ files portably from tests and
# because it doubles as a check that the app is packaged where
# eyeQualityApp() (R/eyeQualityApp.R) expects to find it.
#
# Exercised against real eyeQualityBatch() output (both the default nested
# derivatives/eyeQuality-v1/ layout and a centralized outputDir) for the
# discovery/combination paths, plus hand-built synthetic qcsummary.tsv files
# for the column-mismatch and corrupted-file edge cases that would be slow
# or unreliable to reproduce through a real batch run.

helpers_path <- system.file("shiny-apps", "app", "analyze_helpers.R", package = "eyeQuality")
if (!nzchar(helpers_path)) {
  stop("test-runAnalyzeApp.R: could not locate inst/shiny-apps/app/analyze_helpers.R via system.file()")
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

  app_dir <- system.file("shiny-apps", "app", package = "eyeQuality")
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
  # shinyFiles::parseDirPath() resolves an input$analyze_directory value of
  # list(root = <name>, path = <segments>) as
  # path(roots[[root]], paste0(path, collapse = "/")). tempfile() dirs sit
  # under the OS temp dir, which is under the user's home directory on some
  # platforms but not others (e.g. /tmp is not under $HOME on many Linux/Mac
  # setups), so this is a real, resolvable shinyDirChoose()-style selection
  # -- not a mocked-out analyze_selected_dir() -- only where that happens to hold;
  # skipped below otherwise.
  home <- normalizePath(fs::path_home(), winslash = "/")
  dir_norm <- normalizePath(dir, winslash = "/")
  skip_if_not(
    startsWith(dir_norm, home),
    "test tempdir is not under fs::path_home(); shinyFiles root-relative simulation would not resolve"
  )
  rel <- sub(paste0("^", home, "/?"), "", dir_norm)
  path_segments <- as.list(strsplit(rel, "/")[[1]])

  shiny::testServer(app_dir, {
    session$setInputs(
      analyze_directory = list(root = "Home", path = path_segments),
      analyze_recursiveSearch = TRUE
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

test_that("output$qc_table's actual rendered DT widget reflects load_result() after 'Load qcsummary files' is clicked, not just an empty pre-load render", {
  # Regression guard for a real bug found in manual browser testing: an
  # earlier version of output$qc_table wrapped its entire renderDT() body in
  # isolate(qc_table_flagged()), which strips every reactive dependency --
  # including the dependency on load_result() that's supposed to make this
  # re-render after a real "Load" click. With no dependencies at all, the
  # render only ever executes once, at app startup before any directory is
  # chosen (load_result() hasn't fired, so it produces nothing), and never
  # runs again no matter what gets loaded afterward -- the table (and
  # therefore row-click plots, which need a row to click) just stays
  # permanently blank in a live session. Every other test in this file calls
  # the underlying reactives (qc_table_flagged(), selected_source_file(),
  # etc.) directly rather than session$getOutput("qc_table") itself, which is
  # exactly how this slipped through: those reactives all work fine in
  # isolation, only the actual render wiring was broken.
  skip_on_cran()

  app_dir <- system.file("shiny-apps", "app", package = "eyeQuality")
  expect_true(nzchar(app_dir))

  dir <- tempfile("p10qctablebug_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  dir <- normalizePath(dir, winslash = "/", mustWork = TRUE)

  # A marker string distinctive enough that it can only appear in the
  # rendered DT widget's embedded data if this render actually re-executed
  # against real loaded data, not an empty/pre-load render.
  marker <- "qctablebugmarker12345"
  readr::write_tsv(
    data.frame(qc_metric = "valid_raw_data", percent = 0.5),
    file.path(dir, sprintf(
      "sub-%s_ses-1_recording-eyetracking_physio_desc-run1_preproc_qcsummary.tsv", marker
    ))
  )

  home <- normalizePath(fs::path_home(), winslash = "/")
  skip_if_not(
    startsWith(dir, home),
    "test tempdir is not under fs::path_home(); shinyFiles root-relative simulation would not resolve"
  )
  rel <- sub(paste0("^", home, "/?"), "", dir)
  path_segments <- as.list(strsplit(rel, "/")[[1]])

  shiny::testServer(app_dir, {
    # Force the render's very first execution to happen in the "no data yet"
    # state, the same way a real browser session renders this (visible)
    # output immediately on page load, before any directory is chosen --
    # req(load_result()$table) throws (the eventReactive hasn't fired yet),
    # which a real Shiny render cycle swallows silently (blank output, no
    # visible crash); session$getOutput() surfaces that as a real R
    # condition instead, so it's caught here to match, not because it's
    # itself the bug being guarded against.
    pre_load_output <- tryCatch(
      session$getOutput("qc_table"),
      shiny.silent.error = function(e) NULL,
      validation = function(e) NULL
    )
    if (!is.null(pre_load_output)) {
      expect_false(grepl(marker, pre_load_output, fixed = TRUE))
    }

    session$setInputs(
      analyze_directory = list(root = "Home", path = path_segments),
      analyze_recursiveSearch = TRUE
    )
    session$setInputs(load = 1)

    # The actual regression guard: under the isolate()-everything bug, this
    # second getOutput() call would still return the same empty pre-load
    # render (no reactive dependency ever fired to re-execute it) and this
    # would fail.
    post_load_output <- session$getOutput("qc_table")
    expect_true(grepl(marker, post_load_output, fixed = TRUE))
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

  app_dir <- system.file("shiny-apps", "app", package = "eyeQuality")
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

  home <- normalizePath(fs::path_home(), winslash = "/")
  skip_if_not(
    startsWith(dir, home),
    "test tempdir is not under fs::path_home(); shinyFiles root-relative simulation would not resolve"
  )
  rel <- sub(paste0("^", home, "/?"), "", dir)
  path_segments <- as.list(strsplit(rel, "/")[[1]])

  shiny::testServer(app_dir, {
    session$setInputs(
      analyze_directory = list(root = "Home", path = path_segments),
      analyze_recursiveSearch = FALSE
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
# P10-12: the Analyze tabs' own save/load-config flow (formerly here, keyed
# off cfg_batchName/cfg_directoryBIDS/cfg_displayDimension*_mm "study info"
# fields unique to the standalone Analyze app) no longer exists as its own
# thing -- P10-12 merged it into the Setup tab's single reconciled
# save/load control, which now also carries the Analyze tabs' live
# qc_threshold_* values. See test-runSetupApp.R's own "P10-12: reconciled
# save/load" section for that coverage (build_current_config()'s threshold
# overlay, one-shot save/load round-tripping both halves, and the
# incomplete-config-now-fails-entirely behavior change). analyze_app_dir and
# rel_home_segments_p1007 are kept here (target directory renamed only)
# since later sections of this file still use both for Analyze-tab-only
# shiny::testServer() coverage that has nothing to do with save/load.
# ---------------------------------------------------------------------------

analyze_app_dir <- system.file("shiny-apps", "app", package = "eyeQuality")
if (!nzchar(analyze_app_dir)) {
  stop("test-runAnalyzeApp.R: could not locate inst/shiny-apps/app/ via system.file()")
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

test_that("build_qc_comparison_plot disambiguates two rows sharing the same recording but different batch_name, rather than letting geom_col's default stacking sum them into one bar", {
  # Regression guard for a real bug found in field testing: recording alone
  # isn't a unique file identity -- derive_recording_label() strips the
  # batchName-specific part of the filename, so two runs of the same
  # subject/session under different batch_name values previously shared one
  # x-axis category. geom_col()'s default position = "stack" then summed
  # their percent values into a single bar (e.g. 0.8 + 0.7 rendered as an
  # impossible "150%").
  tbl <- data.frame(
    recording = c("sub-01_ses-1_recording-eyetracking_physio", "sub-01_ses-1_recording-eyetracking_physio"),
    batch_name = c("run1", "run2"),
    source_file = c("/data/run1_qcsummary.tsv", "/data/run2_qcsummary.tsv"),
    qc_metric = "robustness_proportion_valid_data_to_all_data",
    percent = c(0.8, 0.7),
    qc_flag = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  plot <- build_qc_comparison_plot(tbl, "robustness_proportion_valid_data_to_all_data", default_qc_thresholds())

  expect_s3_class(plot, "ggplot")
  expect_equal(nrow(plot$data), 2)
  # The actual fix: each row must land on its own distinct x-axis category,
  # even though `recording` alone is identical for both -- that's what
  # prevents geom_col() from stacking them together.
  expect_length(unique(plot$data$bar_label), 2)
  expect_true(all(grepl("run1|run2", plot$data$bar_label, fixed = FALSE)))
})

test_that("build_qc_comparison_plot leaves bar_label as the plain recording value when it's already unambiguous (no batch_name collision)", {
  tbl <- comparison_test_table("valid_raw_data", c(0.9, 0.5, 0.7))
  plot <- build_qc_comparison_plot(tbl, "valid_raw_data", default_qc_thresholds())
  expect_equal(sort(plot$data$bar_label), sort(tbl$recording))
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

# ---------------------------------------------------------------------------
# P10-05 regression: output$download_flagged_csv's content() must not throw
# when no directory has ever been loaded.
# ---------------------------------------------------------------------------
#
# The bug this guards against: qc_table_flagged() (app.R) does req(result$table)
# internally, which throws a shiny.silent.error/validation condition -- not
# returns NULL -- before any `load` click. That's normally invisible because
# render contexts swallow the condition, but downloadHandler's content()
# runs outside that flush cycle (only when the download URL is actually hit),
# so an unguarded call from content() into qc_table_flagged() surfaces as a
# broken download rather than the header-only-CSV fallback the surrounding
# code otherwise provides for "loaded, nothing flagged".
#
# session$getOutput() is the shiny::testServer() technique that reproduces
# this: for a downloadHandler, it evaluates the registered render function
# (which calls registerDownload() and, per MockShinySession's download
# support, immediately executes content() against a real temp file and
# returns that file's path) -- i.e. it exercises content() itself, not just
# the button/reactive wiring around it, which is exactly where the bug lived
# and exactly what the unit tests for build_flagged_export_table() above
# (which only ever see the helper in isolation, never app.R's reactive
# chain) couldn't catch.
test_that("output$download_flagged_csv's content() does not throw and produces a clean header-only CSV when triggered before any directory has been loaded", {
  skip_on_cran()

  app_dir <- system.file("shiny-apps", "app", package = "eyeQuality")
  expect_true(nzchar(app_dir))

  shiny::testServer(app_dir, {
    # No session$setInputs(load = ...) here -- qc_table_flagged()'s
    # req(result$table) has never been satisfied, matching the verifier's
    # exact repro (calling qc_table_flagged() before `load` has ever fired).
    downloaded_path <- session$getOutput("download_flagged_csv")
    expect_true(nzchar(downloaded_path))
    expect_true(file.exists(downloaded_path))

    result <- utils::read.csv(downloaded_path, stringsAsFactors = FALSE)
    expect_equal(nrow(result), 0)
    expect_setequal(
      names(result),
      c("recording", "batch_name", "source_file", "n_flagged_metrics", "flagged_metrics", "flagged_values")
    )
  })
})

# ---------------------------------------------------------------------------
# P9-07: Analyze tabs' analyze_directory_override seeded from analyze_initialDirectory
# ---------------------------------------------------------------------------
#
# eyeQualityApp()'s initialDirectory argument (validation, and that a valid
# value is actually plumbed through to shiny::shinyOptions() before
# shiny::runApp() would be reached) is covered by test-eyeQualityApp.R, not
# here -- P10-12 merged the once-separate runSetupApp()/runAnalyzeApp()
# wrappers into that single function. What's exercised in this section is the
# Analyze tabs' own `analyze_directory_override` reactive (app.R), seeded from
# getShinyOption("analyze_initialDirectory", NULL), via shiny::testServer().
#
# In real production use, eyeQualityApp() calls shiny::shinyOptions() BEFORE
# shiny::runApp(app_dir), and the resulting global option is inherited by
# every real browser session's own session$options at session-creation time
# (confirmed directly against shiny's own source: ShinySession$initialize()
# does `self$options <- getCurrentAppState()$options`, and getShinyOption()'s
# session branch reads only from session$options). shiny::testServer(),
# however, bypasses shiny::runApp()/shinyApp() entirely and constructs a bare
# MockShinySession with no app-level state behind it, so a
# shiny::shinyOptions() call made from top-level test code before
# testServer() is never seen by getShinyOption() inside server() -- confirmed
# directly while writing these tests (session$options came back empty
# regardless). The correct way to simulate a seeded option in this harness is
# to construct the MockShinySession by hand, set its `options` field directly
# (a plain public R6 field), and pass it in via testServer()'s own `session`
# argument -- exercising the exact same getShinyOption()/session$options code
# path server() itself uses, just seeded a different way than production
# code seeds it.

# seeded_session: a MockShinySession pre-populated with
# analyze_initialDirectory, for handing to shiny::testServer(..., session = ...).
seeded_session <- function(initial_directory) {
  sess <- shiny::MockShinySession$new()
  sess$options <- list(analyze_initialDirectory = initial_directory)
  sess
}

test_that("Analyze app pre-populates the directory field from a seeded analyze_initialDirectory shinyOption, without auto-loading, and the seeded value is directly usable once 'load' is clicked", {
  skip_on_cran()

  seed_dir <- tempfile("p907_seed_")
  dir.create(seed_dir, recursive = TRUE)
  on.exit(unlink(seed_dir, recursive = TRUE), add = TRUE)
  readr::write_tsv(
    data.frame(qc_metric = c("a", "b"), n = c(1, 2)),
    file.path(seed_dir, "sub-a_ses-1_desc-p907seed_preproc_qcsummary.tsv")
  )
  seed_dir <- normalizePath(seed_dir, winslash = "/", mustWork = TRUE)

  shiny::testServer(analyze_app_dir, {
    # pre-populated purely from the seeded option, before any picker interaction
    expect_equal(analyze_selected_dir(), seed_dir)
    expect_equal(session$getOutput("analyze_selected_directory"), seed_dir)

    # no auto-load: load_result() is an eventReactive bound to input$load and
    # has never fired, so evaluating it now raises a silent shiny condition
    # rather than returning a real (even if empty) result -- if the app ever
    # started auto-triggering a load off the seeded directory, this would
    # instead return a populated result here.
    expect_error(load_result())

    # clicking "Load qcsummary files" now uses the seeded directory directly,
    # confirming the pre-populated value is a real, usable selection and not
    # just a display-only placeholder.
    session$setInputs(load = 1)
    result <- load_result()
    expect_equal(result$n_files, 1L)
  }, session = seeded_session(seed_dir))
})

test_that("Analyze app: a real directory-picker selection overrides the seeded analyze_initialDirectory fallback, never the reverse", {
  skip_on_cran()

  seed_dir <- tempfile("p907_seed_override_")
  dir.create(seed_dir, recursive = TRUE)
  on.exit(unlink(seed_dir, recursive = TRUE), add = TRUE)
  seed_dir <- normalizePath(seed_dir, winslash = "/", mustWork = TRUE)

  real_dir <- tempfile("p907_real_picked_")
  dir.create(real_dir, recursive = TRUE)
  on.exit(unlink(real_dir, recursive = TRUE), add = TRUE)
  real_dir <- normalizePath(real_dir, winslash = "/", mustWork = TRUE)
  real_segments <- rel_home_segments_p1007(real_dir)

  shiny::testServer(analyze_app_dir, {
    expect_equal(analyze_selected_dir(), seed_dir)

    session$setInputs(analyze_directory = list(root = "Home", path = real_segments))
    expect_equal(normalizePath(analyze_selected_dir(), winslash = "/"), real_dir)
    expect_false(identical(normalizePath(analyze_selected_dir(), winslash = "/"), seed_dir))
  }, session = seeded_session(seed_dir))
})

test_that("Analyze app with no analyze_initialDirectory shinyOption set: analyze_selected_dir() is empty, matching pre-P9-07 behavior", {
  skip_on_cran()

  # A plain, unseeded MockShinySession (testServer()'s own default) -- no
  # analyze_initialDirectory anywhere in its options, the same as launching
  # eyeQualityApp() with no initialDirectory argument at all.
  shiny::testServer(analyze_app_dir, {
    expect_length(analyze_selected_dir(), 0)
    expect_equal(session$getOutput("analyze_selected_directory"), "No directory selected yet.")
  })
})

# ---------------------------------------------------------------------------
# Live auto-refresh polling: current_load_result()/manual_load_trigger()/
# has_loaded_once()/auto_refresh_interval_ms(), and the observe() block that
# ties them together (app.R). Exercised via shiny::testServer()'s
# session$elapse(millis) -- MockShinySession$elapse() (confirmed directly
# against shiny's own R6 source) advances the mock session's simulated timer
# and runs any reactives/observers invalidateLater() scheduled along the way,
# synchronously and deterministically, rather than needing a real wall-clock
# wait for a periodic reactive under test.
#
# One quirk this file's tests below all account for: the observe() block
# itself does an immediate poll (if input$autoRefresh and has_loaded_once()
# are both already TRUE) the very first time it's invalidated -- e.g. right
# when input$autoRefresh flips to TRUE -- not only after the first
# invalidateLater() interval elapses; invalidateLater() only governs when the
# *next* tick after that happens. Confirmed directly with a minimal
# reproduction before relying on it here.
# ---------------------------------------------------------------------------

# renderui_html: shiny::testServer()'s session$getOutput() returns a
# renderUI() output as a list(html = <character>, deps = <list>) in this
# harness, not a bare character string (confirmed directly -- every state of
# output$auto_refresh_status returns this same shape, unlike renderText()'s
# output$analyze_selected_directory elsewhere in this file, which really is plain
# character). Unwrapped here once so the tests below can still assert against
# a plain string.
renderui_html <- function(x) {
  if (is.list(x) && !is.null(x$html)) x$html else x
}

test_that("checking Auto-refresh before any directory has ever been loaded does nothing: no polling occurs, and auto_refresh_status shows the not-yet-eligible state", {
  skip_on_cran()

  shiny::testServer(analyze_app_dir, {
    expect_match(renderui_html(session$getOutput("auto_refresh_status")), "Auto-refresh is off", fixed = TRUE)

    session$setInputs(autoRefresh = TRUE)
    expect_match(
      renderui_html(session$getOutput("auto_refresh_status")),
      "will begin once you load a directory",
      fixed = TRUE
    )
    expect_false(has_loaded_once())
    expect_null(current_load_result())

    # Advancing simulated time well past the (5s default) interval must
    # produce no update at all -- the observe() block's early return (gated
    # on has_loaded_once()) means invalidateLater() is never even reached, so
    # there's nothing scheduled to elapse into in the first place.
    session$elapse(20000)
    expect_null(current_load_result())
    expect_equal(manual_load_trigger(), 0L)
  })
})

test_that("checking Auto-refresh after a real manual load re-polls the loaded directory on a timer tick, updating current_load_result() with fresh data without bumping manual_load_trigger()", {
  skip_on_cran()

  dir <- tempfile("p10_autorefresh_tick_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  dir <- normalizePath(dir, winslash = "/", mustWork = TRUE)

  readr::write_tsv(
    data.frame(qc_metric = "valid_raw_data", percent = 0.9),
    file.path(dir, "sub-a_ses-1_desc-tickrun_preproc_qcsummary.tsv")
  )

  path_segments <- rel_home_segments_p1007(dir)

  shiny::testServer(analyze_app_dir, {
    session$setInputs(
      analyze_directory = list(root = "Home", path = path_segments),
      analyze_recursiveSearch = FALSE
    )
    session$setInputs(load = 1)

    expect_true(has_loaded_once())
    expect_equal(manual_load_trigger(), 1L)
    expect_equal(current_load_result()$n_files, 1L)

    session$setInputs(autoRefresh = TRUE)

    # Add a new qcsummary.tsv to the SAME directory between the load and the
    # next scheduled tick -- exactly what a user pointing this app at a
    # Setup app run's in-progress output directory is meant to see picked up
    # without another manual "Load qcsummary files" click.
    readr::write_tsv(
      data.frame(qc_metric = "valid_raw_data", percent = 0.5),
      file.path(dir, "sub-b_ses-1_desc-tickrun_preproc_qcsummary.tsv")
    )

    session$elapse(6000) # past the default 5s interval

    expect_equal(current_load_result()$n_files, 2L)
    # The tick's own current_load_result() update must NOT also bump
    # manual_load_trigger() -- that's output$qc_table's OWN rebuild trigger,
    # and bumping it on every tick would rebuild the whole DT widget (and
    # reset a user's sort/filter/page state) every few seconds, exactly the
    # UX regression auto-refresh exists to avoid.
    expect_equal(manual_load_trigger(), 1L)
  })
})

test_that("unchecking Auto-refresh mid-session stops further polling", {
  skip_on_cran()

  dir <- tempfile("p10_autorefresh_stop_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  dir <- normalizePath(dir, winslash = "/", mustWork = TRUE)

  readr::write_tsv(
    data.frame(qc_metric = "valid_raw_data", percent = 0.9),
    file.path(dir, "sub-a_ses-1_desc-stoprun_preproc_qcsummary.tsv")
  )

  path_segments <- rel_home_segments_p1007(dir)

  shiny::testServer(analyze_app_dir, {
    session$setInputs(
      analyze_directory = list(root = "Home", path = path_segments),
      analyze_recursiveSearch = FALSE
    )
    session$setInputs(load = 1)
    expect_equal(current_load_result()$n_files, 1L)

    session$setInputs(autoRefresh = TRUE)

    readr::write_tsv(
      data.frame(qc_metric = "valid_raw_data", percent = 0.5),
      file.path(dir, "sub-b_ses-1_desc-stoprun_preproc_qcsummary.tsv")
    )
    session$elapse(6000)
    expect_equal(current_load_result()$n_files, 2L) # picked up while still checked

    session$setInputs(autoRefresh = FALSE)

    readr::write_tsv(
      data.frame(qc_metric = "valid_raw_data", percent = 0.1),
      file.path(dir, "sub-c_ses-1_desc-stoprun_preproc_qcsummary.tsv")
    )
    session$elapse(6000)

    # Polling has stopped: this third file must NOT be picked up.
    expect_equal(current_load_result()$n_files, 2L)
  })
})

test_that("auto_refresh_interval_ms() reflects autoRefreshIntervalSec and falls back to its documented 5s default for NULL/NA/below-1 values", {
  skip_on_cran()

  shiny::testServer(analyze_app_dir, {
    # No input set yet (MockShinySession starts every input NULL, unlike a
    # real browser session which would already report the numericInput's UI
    # default of 5) -- this is itself the NULL-fallback case.
    expect_equal(auto_refresh_interval_ms(), 5000)

    session$setInputs(autoRefreshIntervalSec = 10)
    expect_equal(auto_refresh_interval_ms(), 10000)

    session$setInputs(autoRefreshIntervalSec = NA)
    expect_equal(auto_refresh_interval_ms(), 5000)

    session$setInputs(autoRefreshIntervalSec = 0)
    expect_equal(auto_refresh_interval_ms(), 5000)

    session$setInputs(autoRefreshIntervalSec = -5)
    expect_equal(auto_refresh_interval_ms(), 5000)
  })
})

test_that("selected_source_file() survives an auto-refresh tick where a new file sorts ahead of the previously selected row, shifting every subsequent row's index", {
  skip_on_cran()

  dir <- tempfile("p10_selection_shift_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  dir <- normalizePath(dir, winslash = "/", mustWork = TRUE)

  readr::write_tsv(
    data.frame(qc_metric = "valid_raw_data", percent = 0.9),
    file.path(dir, "sub-b_ses-1_desc-shiftrun_preproc_qcsummary.tsv")
  )
  readr::write_tsv(
    data.frame(qc_metric = "valid_raw_data", percent = 0.5),
    file.path(dir, "sub-c_ses-1_desc-shiftrun_preproc_qcsummary.tsv")
  )

  path_segments <- rel_home_segments_p1007(dir)

  shiny::testServer(analyze_app_dir, {
    session$setInputs(
      analyze_directory = list(root = "Home", path = path_segments),
      analyze_recursiveSearch = FALSE
    )
    session$setInputs(load = 1)

    result <- current_load_result()
    expect_equal(nrow(result$table), 2)
    # row 2 (alphabetically) is sub-c
    expect_match(result$table$source_file[2], "sub-c_ses-1")

    session$setInputs(qc_table_rows_selected = 2)
    expect_match(selected_source_file(), "sub-c_ses-1")

    session$setInputs(autoRefresh = TRUE)

    # A new file that sorts BEFORE sub-b -- once auto-refresh reloads the
    # directory, every row after it (including the originally selected
    # sub-c) shifts down by one numeric index.
    readr::write_tsv(
      data.frame(qc_metric = "valid_raw_data", percent = 0.1),
      file.path(dir, "sub-a_ses-1_desc-shiftrun_preproc_qcsummary.tsv")
    )

    session$elapse(6000)

    new_result <- current_load_result()
    expect_equal(nrow(new_result$table), 3)
    # what NOW sits at row index 2 is sub-b, not the originally selected sub-c
    expect_match(new_result$table$source_file[2], "sub-b_ses-1")

    # The stash-at-click-time design must still resolve to the ORIGINALLY
    # selected file (sub-c), not whatever now happens to sit at index 2
    # (sub-b) -- this is the specific "wrong file, no crash, no warning"
    # failure mode this design exists to prevent.
    expect_match(selected_source_file(), "sub-c_ses-1")
    expect_false(grepl("sub-b_ses-1", selected_source_file(), fixed = TRUE))
  })
})

test_that("selected_source_file() clears (rather than pointing at a nonexistent file) when an auto-refresh tick's reload no longer contains the previously selected file", {
  skip_on_cran()

  dir <- tempfile("p10_selection_delete_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  dir <- normalizePath(dir, winslash = "/", mustWork = TRUE)

  path_a <- file.path(dir, "sub-a_ses-1_desc-delrun_preproc_qcsummary.tsv")
  readr::write_tsv(data.frame(qc_metric = "valid_raw_data", percent = 0.9), path_a)
  readr::write_tsv(
    data.frame(qc_metric = "valid_raw_data", percent = 0.5),
    file.path(dir, "sub-b_ses-1_desc-delrun_preproc_qcsummary.tsv")
  )

  path_segments <- rel_home_segments_p1007(dir)

  shiny::testServer(analyze_app_dir, {
    session$setInputs(
      analyze_directory = list(root = "Home", path = path_segments),
      analyze_recursiveSearch = FALSE
    )
    session$setInputs(load = 1)

    session$setInputs(qc_table_rows_selected = 1)
    expect_match(selected_source_file(), "sub-a_ses-1")

    session$setInputs(autoRefresh = TRUE)

    unlink(path_a)

    session$elapse(6000)

    new_result <- current_load_result()
    expect_equal(nrow(new_result$table), 1)
    expect_null(selected_source_file())
  })
})

test_that("output$auto_refresh_status shows the actively-polling state with a last-updated timestamp once a directory is loaded and Auto-refresh is checked", {
  skip_on_cran()

  dir <- tempfile("p10_autorefresh_status_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  dir <- normalizePath(dir, winslash = "/", mustWork = TRUE)

  readr::write_tsv(
    data.frame(qc_metric = "valid_raw_data", percent = 0.9),
    file.path(dir, "sub-a_ses-1_desc-statusrun_preproc_qcsummary.tsv")
  )

  path_segments <- rel_home_segments_p1007(dir)

  shiny::testServer(analyze_app_dir, {
    session$setInputs(
      analyze_directory = list(root = "Home", path = path_segments),
      analyze_recursiveSearch = FALSE
    )
    session$setInputs(load = 1)
    session$setInputs(autoRefresh = TRUE)

    status <- renderui_html(session$getOutput("auto_refresh_status"))
    expect_match(status, "Auto-refreshing every", fixed = TRUE)
    expect_false(grepl("will begin once", status, fixed = TRUE))
    expect_false(grepl("Auto-refresh is off", status, fixed = TRUE))
  })
})

# ---------------------------------------------------------------------------
# "Compare files" tab: batch_name (run) filter -- Shiny-free helpers first
# (batch_name_none_sentinel(), compare_batch_name_choices(),
# filter_by_batch_name()), then the live app.R wiring (default selection,
# narrowing output$compare_plot/output$compare_table, and the interaction
# with auto-refresh) via shiny::testServer().
# ---------------------------------------------------------------------------

# batch_name_filter_test_table: a minimal table carrying only the columns
# compare_batch_name_choices()/filter_by_batch_name() actually read
# (batch_name, plus the identifying/metric columns the two Compare-tab
# builder functions need so the same table can double as their input where
# useful).
batch_name_filter_test_table <- function(batch_names) {
  n <- length(batch_names)
  data.frame(
    recording = paste0("rec-", seq_len(n)),
    batch_name = batch_names,
    source_file = paste0("/data/rec-", seq_len(n), "_qcsummary.tsv"),
    qc_metric = "valid_raw_data",
    percent = seq(0.9, by = -0.05, length.out = n),
    stringsAsFactors = FALSE
  )
}

test_that("batch_name_none_sentinel returns the literal '(none)' string", {
  expect_equal(batch_name_none_sentinel(), "(none)")
})

test_that("compare_batch_name_choices returns sorted distinct batch_name values with a single trailing '(none)' entry for NA rows", {
  tbl <- batch_name_filter_test_table(c("runB", "runA", NA, "runA"))
  expect_equal(compare_batch_name_choices(tbl), c("runA", "runB", "(none)"))
})

test_that("compare_batch_name_choices omits the '(none)' entry entirely when there are no NA batch_name rows", {
  tbl <- batch_name_filter_test_table(c("runB", "runA"))
  expect_equal(compare_batch_name_choices(tbl), c("runA", "runB"))
})

test_that("compare_batch_name_choices returns character(0) for a NULL table or one missing the batch_name column", {
  expect_equal(compare_batch_name_choices(NULL), character(0))

  tbl <- batch_name_filter_test_table("runA")
  tbl$batch_name <- NULL
  expect_equal(compare_batch_name_choices(tbl), character(0))
})

test_that("filter_by_batch_name keeps only the matching rows for a real multi-batch_name table", {
  tbl <- batch_name_filter_test_table(c("runA", "runB", "runA"))
  result <- filter_by_batch_name(tbl, "runA")

  expect_equal(nrow(result), 2)
  expect_true(all(result$batch_name == "runA"))
})

test_that("filter_by_batch_name honors '(none)' in selected as a stand-in for keeping the NA batch_name rows", {
  tbl <- batch_name_filter_test_table(c("runA", NA, "runB", NA))
  result <- filter_by_batch_name(tbl, batch_name_none_sentinel())

  expect_equal(nrow(result), 2)
  expect_true(all(is.na(result$batch_name)))
})

test_that("filter_by_batch_name returns a 0-row subset -- not the unfiltered table -- for a NULL or zero-length selected", {
  # Deliberate, easy-to-get-backwards design choice (see filter_by_batch_name()'s
  # own comment in helpers.R): a user who has cleared every selection has
  # asked to see nothing, not everything.
  tbl <- batch_name_filter_test_table(c("runA", "runB"))

  result_null <- filter_by_batch_name(tbl, NULL)
  expect_equal(nrow(result_null), 0)
  expect_equal(colnames(result_null), colnames(tbl))

  result_empty <- filter_by_batch_name(tbl, character(0))
  expect_equal(nrow(result_empty), 0)
})

test_that("filter_by_batch_name returns the input unchanged for a NULL table or one missing the batch_name column", {
  expect_null(filter_by_batch_name(NULL, "runA"))

  tbl <- batch_name_filter_test_table("runA")
  tbl$batch_name <- NULL
  expect_identical(filter_by_batch_name(tbl, "runA"), tbl)
})

# capturing_update_session: a MockShinySession whose sendInputMessage() --
# a documented no-op in a bare MockShinySession (see test-runSetupApp.R's own
# file-level comment on update*Input() calls never reflecting into input$...
# in this harness) -- instead records every update*Input() call's
# inputId/message into `calls`, keyed by inputId. This lets a test assert on
# what app.R's own observeEvent(manual_load_trigger(), ...) block actually
# told the (simulated) browser to set for "compare_batch_name_filter" --
# specifically that `selected` defaults to every currently loaded
# batch_name -- without needing input$compare_batch_name_filter itself to
# reflect that value (which, per the no-op above, it never does here).
capturing_update_session <- function() {
  sess <- shiny::MockShinySession$new()
  calls <- new.env(parent = emptyenv())
  sess$sendInputMessage <- function(inputId, message) {
    existing <- calls[[inputId]]
    if (is.null(existing)) {
      existing <- list()
    }
    existing[[length(existing) + 1]] <- message
    calls[[inputId]] <- existing
    invisible()
  }
  list(session = sess, calls = calls)
}

test_that("Compare files batch_name filter defaults to every loaded batch_name selected (including the '(none)' case) on a real manual load", {
  skip_on_cran()

  dir <- tempfile("p1004_batchfilter_default_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  dir <- normalizePath(dir, winslash = "/", mustWork = TRUE)

  readr::write_tsv(
    data.frame(qc_metric = "valid_raw_data", percent = 0.9),
    file.path(dir, "sub-a_ses-1_desc-batchA_preproc_qcsummary.tsv")
  )
  readr::write_tsv(
    data.frame(qc_metric = "valid_raw_data", percent = 0.5),
    file.path(dir, "sub-b_ses-1_desc-batchB_preproc_qcsummary.tsv")
  )
  # eyeQuality()'s single-file (batchName == NULL) naming form -- the "(none)" case.
  readr::write_tsv(
    data.frame(qc_metric = "valid_raw_data", percent = 0.6),
    file.path(dir, "sub-c_ses-1_desc-preproc_qcsummary.tsv")
  )

  path_segments <- rel_home_segments_p1007(dir)
  cs <- capturing_update_session()

  shiny::testServer(analyze_app_dir, {
    session$setInputs(
      analyze_directory = list(root = "Home", path = path_segments),
      analyze_recursiveSearch = FALSE
    )
    session$setInputs(load = 1)

    calls <- get("compare_batch_name_filter", envir = cs$calls)
    expect_length(calls, 1)
    expect_setequal(calls[[1]]$value, c("batchA", "batchB", "(none)"))
  }, session = cs$session)
})

test_that("narrowing the Compare files batch_name filter narrows output$compare_plot/output$compare_table, and a later auto-refresh tick does not reset that narrowed selection back to all", {
  skip_on_cran()

  dir <- tempfile("p1004_batchfilter_interaction_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  dir <- normalizePath(dir, winslash = "/", mustWork = TRUE)

  # One batchA file, and enough batchB files that the unfiltered (15-row) vs.
  # narrowed-to-batchA-only (1-row) row counts push output$compare_plot's
  # dynamic height() function (n * 28 + 120, floored at 500px -- see app.R)
  # to two genuinely different values, rather than both landing on the same
  # 500px floor.
  readr::write_tsv(
    data.frame(qc_metric = "valid_raw_data", percent = 0.9),
    file.path(dir, "sub-a_ses-1_desc-batchA_preproc_qcsummary.tsv")
  )
  for (i in seq_len(14)) {
    readr::write_tsv(
      data.frame(qc_metric = "valid_raw_data", percent = 0.5),
      file.path(dir, sprintf("sub-b%02d_ses-1_desc-batchB_preproc_qcsummary.tsv", i))
    )
  }

  path_segments <- rel_home_segments_p1007(dir)
  cs <- capturing_update_session()

  shiny::testServer(analyze_app_dir, {
    session$setInputs(
      analyze_directory = list(root = "Home", path = path_segments),
      analyze_recursiveSearch = FALSE
    )
    session$setInputs(load = 1)
    expect_equal(current_load_result()$n_files, 15L)

    initial_calls <- get("compare_batch_name_filter", envir = cs$calls)
    expect_length(initial_calls, 1)
    expect_setequal(initial_calls[[1]]$value, c("batchA", "batchB"))

    # Reflect the default "select all" state a real browser would now show
    # (see capturing_update_session()'s own comment on why input$<id> doesn't
    # do this automatically here), plus the metric selectors' own defaults,
    # so output$compare_plot/output$compare_table have something to render.
    session$setInputs(
      compare_batch_name_filter = c("batchA", "batchB"),
      compare_metric_plot = "valid_raw_data",
      compare_metrics_table = "valid_raw_data"
    )

    expect_equal(session$getOutput("compare_plot")$height, 15 * 28 + 120)

    # output$compare_table's own renderDT() uses DT::renderDT()'s default
    # server = TRUE (server-side processing), unlike output$qc_table (which
    # explicitly sets server = FALSE for exactly this reason -- see that
    # render's own comment in app.R). Under server = TRUE, session$getOutput()
    # returns only the initial widget shell/columns, never the actual row
    # data (confirmed directly: DT registers its row-serving function via
    # session$registerDataObj(), which -- like sendInputMessage() -- is a
    # documented no-op on MockShinySession, so there's no real AJAX endpoint
    # here to fetch filtered rows from). That rules out grepping recording
    # labels out of the rendered widget the way the qc_table marker
    # regression test above does. Instead, this test calls the exact same
    # production helpers (filter_by_batch_name(), build_qc_comparison_table())
    # against the exact same live reactive values (current_load_result(),
    # input$compare_batch_name_filter/compare_metrics_table) that
    # output$compare_table's render body itself reads and calls, which
    # exercises the identical narrowing logic -- plus a getOutput() call to
    # confirm the render pipeline still completes without error at each
    # selection state.
    compare_table_now <- function() {
      filtered <- filter_by_batch_name(current_load_result()$table, input$compare_batch_name_filter)
      build_qc_comparison_table(filtered, input$compare_metrics_table)
    }

    all_selected <- compare_table_now()
    expect_equal(nrow(all_selected), 15)
    expect_true("sub-a_ses-1" %in% all_selected$recording)
    expect_true("sub-b01_ses-1" %in% all_selected$recording)
    expect_true(nzchar(session$getOutput("compare_table")))

    # --- deselect batchB: narrow to batchA only ---
    session$setInputs(compare_batch_name_filter = "batchA")

    expect_equal(session$getOutput("compare_plot")$height, 500) # 1*28+120=148, floored to 500
    narrowed <- compare_table_now()
    expect_equal(nrow(narrowed), 1)
    expect_equal(narrowed$recording, "sub-a_ses-1")
    expect_true(nzchar(session$getOutput("compare_table")))

    # --- a later auto-refresh tick must NOT reset this narrowed selection ---
    session$setInputs(autoRefresh = TRUE)

    # A brand-new batch_name arriving mid-session -- app.R's own documented
    # behavior is that this is never auto-selected into an already-narrowed
    # filter (a user who wants to see it selects it manually).
    readr::write_tsv(
      data.frame(qc_metric = "valid_raw_data", percent = 0.3),
      file.path(dir, "sub-c_ses-1_desc-batchC_preproc_qcsummary.tsv")
    )

    session$elapse(6000)

    # The tick genuinely re-polled (proves this isn't a no-op tick)...
    expect_equal(current_load_result()$n_files, 16L)

    # ...but "compare_batch_name_filter"'s own updateSelectizeInput was never
    # called a second time -- its repopulating observer is deliberately keyed
    # on manual_load_trigger() only (never current_load_result(), which is
    # what ticks actually change), matching the metric-selector observer's
    # own reasoning in app.R.
    expect_length(get("compare_batch_name_filter", envir = cs$calls), 1)

    # ...and input$compare_batch_name_filter is still just "batchA" -- the
    # tick never touched it -- so the same narrowing helpers still resolve to
    # only the batchA row, not "every batch_name including the new one".
    expect_equal(input$compare_batch_name_filter, "batchA")
    post_tick <- compare_table_now()
    expect_equal(nrow(post_tick), 1)
    expect_equal(post_tick$recording, "sub-a_ses-1")
    expect_equal(session$getOutput("compare_plot")$height, 500)
    expect_true(nzchar(session$getOutput("compare_table")))
  }, session = cs$session)
})

# ---------------------------------------------------------------------------
# "Gaze Explorer" tab: resolve_events_data_path(), load_gaze_trajectory_data(),
# derive_event_marker_windows(), points_in_polygon(), compute_aoi_percent(),
# thin_for_display(), and the live app.R reactive wiring around all of the
# above (helpers.R additions committed alongside b746f87).
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# resolve_events_data_path()
# ---------------------------------------------------------------------------

test_that("resolve_events_data_path swaps the trailing preproc segment for events immediately before .tsv, for both the batchName-present and batchName-NULL naming forms", {
  # Mirrors resolve_preproc_data_path()'s own two-form coverage above --
  # saveFiles() builds preproc.tsv and events.tsv off two different words
  # ("preproc" vs "events") sharing only the "_desc-<batchName>_" prefix, not
  # one derived from the other by suffixing (see this function's own header
  # comment in helpers.R), so this needs its own dedicated test rather than
  # inheriting correctness from resolve_preproc_data_path()'s.
  expect_equal(
    resolve_events_data_path("/data/derivatives/eyeQuality-v1/sub-01_ses-1_desc-mybatch_preproc_qcsummary.tsv"),
    "/data/derivatives/eyeQuality-v1/sub-01_ses-1_desc-mybatch_events.tsv"
  )
  expect_equal(
    resolve_events_data_path("/data/derivatives/eyeQuality-v1/sub-01_ses-1_desc-preproc_qcsummary.tsv"),
    "/data/derivatives/eyeQuality-v1/sub-01_ses-1_desc-events.tsv"
  )
})

# ---------------------------------------------------------------------------
# load_gaze_trajectory_data()
# ---------------------------------------------------------------------------

test_that("load_gaze_trajectory_data returns ok = TRUE with real trajectory data and a NULL events result for a real batch run's genuine 0-row events.tsv", {
  skip_on_cran()

  dir <- copy_bids_fixture_tree()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  eyeQualityBatch(dir, batchName = "p10gaze1", numberCores = 1)

  found <- discover_qcsummary_files(dir, recursive = TRUE)
  expect_length(found, 4)

  result <- load_gaze_trajectory_data(found[1])

  expect_true(result$ok)
  expect_equal(result$preproc_path, resolve_preproc_data_path(found[1]))
  expect_equal(result$events_path, resolve_events_data_path(found[1]))
  expect_true(file.exists(result$events_path)) # the events.tsv genuinely exists...
  expect_s3_class(result$data, "data.frame")
  expect_true(all(c("recordingTimestamp_ms", "gazeX.preprocessed_px", "gazeY.preprocessed_px") %in% names(result$data)))
  expect_null(result$events) # ...but with 0 rows, degrades to NULL rather than an empty data.frame
})

test_that("load_gaze_trajectory_data returns ok = FALSE with a clear, non-crashing error when the sibling preproc file is missing", {
  qcsummary_path <- file.path(tempdir(), "sub-x_ses-1_desc-gazegone_preproc_qcsummary.tsv")
  # deliberately don't create the sibling *_preproc.tsv file

  result <- load_gaze_trajectory_data(qcsummary_path)

  expect_false(result$ok)
  expect_equal(result$preproc_path, resolve_preproc_data_path(qcsummary_path))
  expect_equal(result$events_path, resolve_events_data_path(qcsummary_path))
  expect_true(is.character(result$error) && nzchar(result$error))
  expect_match(result$error, "should sit alongside", fixed = TRUE)
  expect_null(result$data)
})

test_that("load_gaze_trajectory_data returns ok = FALSE with a clear error (not a crash) when the preproc file exists but is missing this tab's required trajectory columns", {
  dir <- tempfile("p10gaze_badcols_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  qcsummary_path <- file.path(dir, "sub-y_ses-1_desc-gazebadcols_preproc_qcsummary.tsv")
  preproc_path <- file.path(dir, "sub-y_ses-1_desc-gazebadcols_preproc.tsv")
  file.create(qcsummary_path)
  # has recordingTimestamp_ms but is missing gazeX.preprocessed_px/gazeY.preprocessed_px
  readr::write_tsv(data.frame(recordingTimestamp_ms = 1:3), preproc_path)

  result <- load_gaze_trajectory_data(qcsummary_path)

  expect_false(result$ok)
  expect_true(is.character(result$error) && nzchar(result$error))
  expect_match(result$error, "gazeX.preprocessed_px", fixed = TRUE)
  expect_match(result$error, "gazeY.preprocessed_px", fixed = TRUE)
  expect_null(result$data)
})

# ---------------------------------------------------------------------------
# derive_event_marker_windows()
# ---------------------------------------------------------------------------

test_that("derive_event_marker_windows collapses multiple occurrences of distinct event labels to one first-to-last window each", {
  events <- data.frame(
    event = c("A", "A", "B", "C", "A"),
    recordingTimestamp_ms = c(10, 20, 5, 100, 30)
  )

  result <- derive_event_marker_windows(events)

  expect_equal(result$event, c("A", "B", "C"))
  expect_equal(result$n_occurrences, c(3, 1, 1))
  expect_equal(result$start_ms, c(10, 5, 100))
  expect_equal(result$end_ms, c(30, 5, 100))
})

test_that("derive_event_marker_windows produces a zero-width window for an event label that occurs only once", {
  events <- data.frame(event = "OnlyOnce", recordingTimestamp_ms = 42)

  result <- derive_event_marker_windows(events)

  expect_equal(result$n_occurrences, 1)
  expect_equal(result$start_ms, 42)
  expect_equal(result$end_ms, 42)
})

test_that("derive_event_marker_windows returns NULL (not an empty data.frame) for NULL events, wrong-shaped events, or events with 0 usable rows", {
  expect_null(derive_event_marker_windows(NULL))
  expect_null(derive_event_marker_windows(data.frame(a = 1))) # missing required columns
  expect_null(derive_event_marker_windows(data.frame(event = character(0), recordingTimestamp_ms = numeric(0))))
  expect_null(derive_event_marker_windows(data.frame(event = NA_character_, recordingTimestamp_ms = NA_real_)))
})

# ---------------------------------------------------------------------------
# points_in_polygon()
# ---------------------------------------------------------------------------

test_that("points_in_polygon correctly classifies clearly-inside and clearly-outside points against a simple square", {
  square_x <- c(0, 10, 10, 0)
  square_y <- c(0, 0, 10, 10)

  expect_true(points_in_polygon(5, 5, square_x, square_y))
  expect_false(points_in_polygon(20, 20, square_x, square_y))
})

test_that("points_in_polygon's boundary behavior on a vertex is the standard half-open ray-casting rule (bottom/left edges in, top/right edges out) -- documented as empirically verified, not assumed", {
  # Verified directly (not just asserted): the bottom-left vertex/edges of
  # this square classify as inside, the top-right vertex/edges as outside.
  # This is standard ray-casting boundary behavior (a point exactly on an
  # edge can go either way depending on the specific edge, this pins down
  # which way THIS implementation goes), not a design requirement this tab
  # depends on -- AOI edges are user-drawn approximate regions, not exact
  # trial boundaries, so this test exists to catch an accidental algorithm
  # change, not to assert a specific edge behavior is "correct".
  square_x <- c(0, 10, 10, 0)
  square_y <- c(0, 0, 10, 10)

  expect_true(points_in_polygon(0, 0, square_x, square_y)) # bottom-left vertex
  expect_false(points_in_polygon(10, 10, square_x, square_y)) # top-right vertex
  expect_true(points_in_polygon(5, 0, square_x, square_y)) # bottom edge midpoint
  expect_true(points_in_polygon(0, 5, square_x, square_y)) # left edge midpoint
  expect_false(points_in_polygon(10, 5, square_x, square_y)) # right edge midpoint
})

test_that("points_in_polygon returns NA (not FALSE) for a point with NA x or NA y", {
  square_x <- c(0, 10, 10, 0)
  square_y <- c(0, 0, 10, 10)

  expect_true(is.na(points_in_polygon(NA, 5, square_x, square_y)))
  expect_true(is.na(points_in_polygon(5, NA, square_x, square_y)))

  # vectorized: NA at one index must not corrupt neighboring indices' results
  result <- points_in_polygon(c(5, NA, 20), c(5, 5, 20), square_x, square_y)
  expect_equal(result, c(TRUE, NA, FALSE))
})

test_that("points_in_polygon correctly classifies points against a non-convex L-shaped polygon, not just rectangles", {
  # An L-shape: a 10x10 square with a 5x5 notch cut out of its top-right
  # corner. Confirms the ray-casting implementation isn't accidentally
  # rectangle-only despite the AOI-definition UI only ever offering 4 corners.
  lx <- c(0, 10, 10, 5, 5, 0)
  ly <- c(0, 0, 5, 5, 10, 10)

  expect_true(points_in_polygon(2, 2, lx, ly)) # in the main body
  expect_false(points_in_polygon(7, 7, lx, ly)) # in the notched-out area
  expect_true(points_in_polygon(7, 2, lx, ly)) # in the lower-right leg
})

test_that("points_in_polygon correctly classifies points against a triangle", {
  tx <- c(0, 10, 5)
  ty <- c(0, 0, 10)

  expect_true(points_in_polygon(5, 3, tx, ty)) # inside, near centroid
  expect_false(points_in_polygon(9, 9, tx, ty)) # outside
})

test_that("points_in_polygon errors on fewer than 3 polygon vertices", {
  expect_error(
    points_in_polygon(1, 1, c(0, 1), c(0, 1)),
    "at least 3 vertices"
  )
})

test_that("points_in_polygon errors on mismatched x/y vector lengths", {
  square_x <- c(0, 10, 10, 0)
  square_y <- c(0, 0, 10, 10)

  expect_error(
    points_in_polygon(c(1, 2), 1, square_x, square_y),
    "x and y must be equal-length"
  )
})

test_that("points_in_polygon errors on mismatched poly_x/poly_y vector lengths", {
  expect_error(
    points_in_polygon(1, 1, c(0, 10, 10), c(0, 0)),
    "at least 3 vertices"
  )
})

# ---------------------------------------------------------------------------
# compute_aoi_percent()
# ---------------------------------------------------------------------------

test_that("compute_aoi_percent computes n_total/n_inside/pct correctly for a small hand-computed data.frame and square AOI", {
  # AOI: unit square [0, 10] x [0, 10].
  # Rows, by hand: (1,1) inside, (5,5) inside, (20,20) outside, row 4/5 have
  # an NA in one coordinate (excluded from n_total entirely), row 6 has NA in
  # both (also excluded).
  # -> n_total = 3 usable rows, n_inside = 2 ((1,1) and (5,5)), pct = 2/3.
  df <- data.frame(
    gazeX.preprocessed_px = c(1, 5, 20, NA, 3, NA),
    gazeY.preprocessed_px = c(1, 5, 20, 3, NA, NA)
  )
  aoi <- list(x = c(0, 10, 10, 0), y = c(0, 0, 10, 10))

  result <- compute_aoi_percent(df, aoi)

  expect_equal(result$n_total, 3)
  expect_equal(result$n_inside, 2)
  expect_equal(result$pct, 2 / 3)
})

test_that("compute_aoi_percent returns NULL for a NULL aoi", {
  df <- data.frame(gazeX.preprocessed_px = 5, gazeY.preprocessed_px = 5)
  expect_null(compute_aoi_percent(df, NULL))
})

test_that("compute_aoi_percent returns NULL when x_col or y_col is missing from df", {
  aoi <- list(x = c(0, 10, 10, 0), y = c(0, 0, 10, 10))
  expect_null(compute_aoi_percent(data.frame(gazeX.preprocessed_px = 5), aoi))
  expect_null(compute_aoi_percent(data.frame(a = 1), aoi))
  expect_null(compute_aoi_percent(NULL, aoi))
})

test_that("compute_aoi_percent returns NULL when every row has NA coordinates (0 usable rows)", {
  aoi <- list(x = c(0, 10, 10, 0), y = c(0, 0, 10, 10))
  df <- data.frame(
    gazeX.preprocessed_px = c(NA, NA),
    gazeY.preprocessed_px = c(NA, NA)
  )
  expect_null(compute_aoi_percent(df, aoi))
})

test_that("compute_aoi_percent excludes rows with only one of x/y missing, not just fully-NA rows", {
  aoi <- list(x = c(0, 10, 10, 0), y = c(0, 0, 10, 10))
  df <- data.frame(
    gazeX.preprocessed_px = c(5, NA, 5),
    gazeY.preprocessed_px = c(5, 5, NA)
  )
  result <- compute_aoi_percent(df, aoi)
  expect_equal(result$n_total, 1) # only row 1 has both coordinates present
  expect_equal(result$n_inside, 1)
})

# ---------------------------------------------------------------------------
# thin_for_display()
# ---------------------------------------------------------------------------

test_that("thin_for_display returns df unchanged when nrow(df) is at or under the cap", {
  df_under <- data.frame(x = 1:100)
  expect_identical(thin_for_display(df_under, cap = 5000L), df_under)

  df_at_cap <- data.frame(x = 1:5000)
  expect_identical(thin_for_display(df_at_cap, cap = 5000L), df_at_cap)
})

test_that("thin_for_display evenly subsamples down to exactly the cap when nrow(df) exceeds it", {
  df <- data.frame(x = 1:12000)
  thinned <- thin_for_display(df, cap = 5000L)
  expect_equal(nrow(thinned), 5000L)
  # first and last rows of the original range are preserved by an
  # evenly-spaced seq(1, n, length.out = cap) subsample.
  expect_equal(thinned$x[1], 1)
  expect_equal(thinned$x[nrow(thinned)], 12000)
})

test_that("compute_aoi_percent() against the full untrimmed data can give a different (and always the CORRECT, non-silently-substituted) answer than against thin_for_display()'s output, when the AOI-relevant point falls at a row position the thinned subsample drops", {
  # 6000 rows, cap = 5000: thin_for_display()'s evenly-spaced
  # unique(round(seq(1, 6000, length.out = 5000))) subsample is confirmed
  # (checked directly) to drop row 4 -- placing the ONLY AOI-inside point
  # there means the full data's AOI percentage and the thinned copy's AOI
  # percentage are guaranteed to disagree, proving thin_for_display()'s
  # output is never silently an acceptable substitute for the untrimmed data
  # a correctness-sensitive computation like compute_aoi_percent() needs.
  n <- 6000L
  x <- rep(100, n)
  y <- rep(100, n)
  x[4] <- 5
  y[4] <- 5
  df <- data.frame(gazeX.preprocessed_px = x, gazeY.preprocessed_px = y)
  aoi <- list(x = c(0, 10, 10, 0), y = c(0, 0, 10, 10))

  full_result <- compute_aoi_percent(df, aoi)
  thinned_result <- compute_aoi_percent(thin_for_display(df, cap = 5000L), aoi)

  expect_equal(full_result$n_total, 6000)
  expect_equal(full_result$n_inside, 1)

  expect_equal(thinned_result$n_total, 5000)
  expect_equal(thinned_result$n_inside, 0)

  expect_false(identical(full_result$pct, thinned_result$pct))
})

# ---------------------------------------------------------------------------
# "Gaze Explorer" tab: live app.R reactive wiring (shiny::testServer())
# ---------------------------------------------------------------------------

# build_gaze_explorer_fixture: a hand-built (NOT eyeQualityBatch()-run) pair
# of qcsummary/preproc/[events] file trios, one per "file", giving full
# control over gaze coordinates/timestamps/event labels for the
# hand-computable AOI-percentage and time-range assertions below -- unlike a
# real batch run's own fixture output, whose gaze coordinates are near-
# constant across the whole recording and can't be hand-designed to land a
# known fraction of points inside/outside a given AOI (see the real-batch-run
# load_gaze_trajectory_data() tests above, which use eyeQualityBatch() output
# instead specifically because those only need real file *layout*, not
# specific gaze values).
#
# File A: has real event markers (one label, "Trial1", occurring twice) and
#   10 gaze samples over recordingTimestamp_ms 0-90, with coordinates chosen
#   so a [0,10] x [0,10] square AOI classifies a hand-computable 5 of 8
#   usable rows as inside (see the AOI-readout test below for the arithmetic).
# File B: no events.tsv sibling at all (a real, expected "no markers"
#   degradation case -- see load_gaze_trajectory_data()'s own header comment)
#   and a wholly different, non-overlapping recordingTimestamp_ms range from
#   file A -- lets the row-switch reset tests below confirm BOTH the slider
#   bounds and any AOI defined against file A are actually reset, not just
#   assumed to be.
#
# discover_qcsummary_files()'s sort() means file A ("sub-a...") always
# resolves to combined-table row 1 and file B ("sub-b...") to row 2.
build_gaze_explorer_fixture <- function() {
  dir <- tempfile("p10_gaze_explorer_")
  dir.create(dir)

  readr::write_tsv(
    data.frame(qc_metric = "valid_raw_data", percent = 0.9),
    file.path(dir, "sub-a_ses-1_desc-fileA_preproc_qcsummary.tsv")
  )
  readr::write_tsv(
    data.frame(
      recordingTimestamp_ms = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90),
      gazeX.preprocessed_px = c(5, 5, 20, 5, NA, 5, 0, 10, -5, 3),
      gazeY.preprocessed_px = c(5, 5, 20, 5, 5, NA, 0, 10, -5, 3)
    ),
    file.path(dir, "sub-a_ses-1_desc-fileA_preproc.tsv")
  )
  readr::write_tsv(
    data.frame(
      event = c("Trial1", "Trial1"),
      recordingTimestamp_ms = c(10, 50)
    ),
    file.path(dir, "sub-a_ses-1_desc-fileA_events.tsv")
  )

  readr::write_tsv(
    data.frame(qc_metric = "valid_raw_data", percent = 0.5),
    file.path(dir, "sub-b_ses-1_desc-fileB_preproc_qcsummary.tsv")
  )
  readr::write_tsv(
    data.frame(
      recordingTimestamp_ms = c(1000, 1010, 1020),
      gazeX.preprocessed_px = c(50, 50, 50),
      gazeY.preprocessed_px = c(50, 50, 50)
    ),
    file.path(dir, "sub-b_ses-1_desc-fileB_preproc.tsv")
  )
  # deliberately no sub-b...events.tsv at all

  normalizePath(dir, winslash = "/", mustWork = TRUE)
}

test_that("selecting a QC-table row for a file with real event markers populates the event-marker selector; a file without any shows the degraded 'no markers' message instead", {
  skip_on_cran()
  skip_if_not_installed("plotly")

  dir <- build_gaze_explorer_fixture()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  path_segments <- rel_home_segments_p1007(dir)

  shiny::testServer(analyze_app_dir, {
    session$setInputs(analyze_directory = list(root = "Home", path = path_segments), analyze_recursiveSearch = FALSE)
    session$setInputs(load = 1)

    session$setInputs(qc_table_rows_selected = 1) # file A, has events
    marker_ui_a <- renderui_html(session$getOutput("gaze_event_marker_ui"))
    expect_match(marker_ui_a, "Trial1", fixed = TRUE)
    expect_match(marker_ui_a, "Jump to event marker", fixed = TRUE)
    windows_a <- gaze_event_windows()
    expect_equal(windows_a$event, "Trial1")
    expect_equal(windows_a$n_occurrences, 2)

    session$setInputs(qc_table_rows_selected = 2) # file B, no events.tsv at all
    marker_ui_b <- renderui_html(session$getOutput("gaze_event_marker_ui"))
    expect_match(marker_ui_b, "No event markers found for this recording", fixed = TRUE)
    expect_null(gaze_event_windows())
  })
})

test_that("switching the selected row from file A to file B resets the time-range slider to file B's own bounds and clears any AOI defined for file A", {
  skip_on_cran()
  skip_if_not_installed("plotly")

  dir <- build_gaze_explorer_fixture()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  path_segments <- rel_home_segments_p1007(dir)

  # updateSliderInput()'s sendInputMessage() call is a documented no-op on a
  # bare MockShinySession (input$gaze_time_range itself never reflects it --
  # see capturing_update_session()'s own comment above), so this asserts on
  # the actual min/max/value the server told the (simulated) browser to set
  # instead, the same technique this file's existing Compare-files
  # batch_name-filter tests already use for the same reason.
  cs <- capturing_update_session()

  shiny::testServer(analyze_app_dir, {
    session$setInputs(analyze_directory = list(root = "Home", path = path_segments), analyze_recursiveSearch = FALSE)
    session$setInputs(load = 1)

    session$setInputs(qc_table_rows_selected = 1) # file A: recordingTimestamp_ms 0-90
    calls_a <- get("gaze_time_range", envir = cs$calls)
    last_call_a <- calls_a[[length(calls_a)]]
    expect_equal(as.numeric(last_call_a$min), 0)
    expect_equal(as.numeric(last_call_a$max), 90)
    expect_equal(as.numeric(last_call_a$value), c(0, 90))

    # Reflect that reset the way a real browser would (see the comment
    # above) so filtered_trajectory()/gaze_aoi_result() below have a real
    # rng to work from, then define an AOI while file A is selected.
    session$setInputs(gaze_time_range = c(0, 90))
    session$setInputs(gaze_define_aoi = 1)
    session$setInputs(
      gaze_aoi1x = 0, gaze_aoi1y = 0,
      gaze_aoi2x = 10, gaze_aoi2y = 0,
      gaze_aoi3x = 10, gaze_aoi3y = 10,
      gaze_aoi4x = 0, gaze_aoi4y = 10
    )
    session$setInputs(gaze_aoi_submit = 1)
    expect_false(is.null(aoi_polygon()))

    # switch to file B: recordingTimestamp_ms 1000-1020, a wholly different
    # (non-overlapping) range from file A's own 0-90
    session$setInputs(qc_table_rows_selected = 2)
    calls_b <- get("gaze_time_range", envir = cs$calls)
    last_call_b <- calls_b[[length(calls_b)]]
    expect_equal(as.numeric(last_call_b$min), 1000)
    expect_equal(as.numeric(last_call_b$max), 1020)
    expect_equal(as.numeric(last_call_b$value), c(1000, 1020))
    expect_null(aoi_polygon())
  }, session = cs$session)
})

test_that("defining an AOI via the modal's 4 corner inputs produces an n_inside/n_total/pct readout matching a hand-computed expectation", {
  skip_on_cran()
  skip_if_not_installed("plotly")

  dir <- build_gaze_explorer_fixture()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  path_segments <- rel_home_segments_p1007(dir)

  shiny::testServer(analyze_app_dir, {
    session$setInputs(analyze_directory = list(root = "Home", path = path_segments), analyze_recursiveSearch = FALSE)
    session$setInputs(load = 1)
    session$setInputs(qc_table_rows_selected = 1) # file A

    # updateSliderInput()'s reset (observeEvent(selected_source_file(), ...)
    # in app.R) is a no-op on input$gaze_time_range itself under a bare
    # MockShinySession (see capturing_update_session()'s own comment
    # elsewhere in this file) -- reflect the real browser echo of file A's
    # own [0, 90] bounds by hand so filtered_trajectory() has a real rng.
    session$setInputs(gaze_time_range = c(0, 90))

    session$setInputs(gaze_define_aoi = 1)
    session$setInputs(
      gaze_aoi1x = 0, gaze_aoi1y = 0,
      gaze_aoi2x = 10, gaze_aoi2y = 0,
      gaze_aoi3x = 10, gaze_aoi3y = 10,
      gaze_aoi4x = 0, gaze_aoi4y = 10
    )
    session$setInputs(gaze_aoi_submit = 1)

    result <- gaze_aoi_result()
    # Hand-computed against file A's 10 gaze rows and the [0,10] x [0,10]
    # square: rows 5/6 have one NA coordinate each (excluded), leaving 8
    # usable rows -- (5,5) x3 inside, (20,20) outside, (0,0) vertex inside,
    # (10,10) vertex outside, (-5,-5) outside, (3,3) inside -> 5 of 8 inside.
    expect_equal(result$n_total, 8)
    expect_equal(result$n_inside, 5)
    expect_equal(result$pct, 5 / 8)

    status_html <- renderui_html(session$getOutput("gaze_aoi_status"))
    expect_match(status_html, "5 of 8", fixed = TRUE)
  })
})

test_that("narrowing the time range to a window with 0 gaze samples shows the documented empty-range message instead of erroring", {
  skip_on_cran()
  skip_if_not_installed("plotly")

  dir <- build_gaze_explorer_fixture()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  path_segments <- rel_home_segments_p1007(dir)

  shiny::testServer(analyze_app_dir, {
    session$setInputs(analyze_directory = list(root = "Home", path = path_segments), analyze_recursiveSearch = FALSE)
    session$setInputs(load = 1)
    session$setInputs(qc_table_rows_selected = 1) # file A: samples at 0,10,20,...,90

    # narrow to a sub-range of file A's own [0, 90] slider bounds that
    # contains no actual sample timestamps at all.
    session$setInputs(gaze_time_range = c(2, 8))

    plot_output <- tryCatch(
      session$getOutput("gaze_trajectory_plot"),
      shiny.silent.error = function(e) conditionMessage(e),
      validation = function(e) conditionMessage(e)
    )
    expect_match(plot_output, "No gaze samples in the current time range.", fixed = TRUE)
  })
})

test_that("the app-level wiring passes the untrimmed filtered_trajectory() to compute_aoi_percent(), never the display-thinned copy", {
  # A live-app-level version of the thin_for_display() unit test above:
  # reads app.R's actual reactive graph (gaze_aoi_result() -> filtered_trajectory(),
  # NOT the thinned copy build_gaze_trajectory_plot() computes internally for
  # rendering) rather than assuming helpers.R's own docstrings are honored by
  # the caller.
  skip_on_cran()
  skip_if_not_installed("plotly")

  dir <- tempfile("p10_gaze_explorer_thinguarantee_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  n <- 6000L
  x <- rep(100, n)
  y <- rep(100, n)
  # thin_for_display()'s evenly-spaced cap=5000 subsample of a 6000-row df is
  # confirmed (see the Shiny-free unit test above) to drop row 4 -- placing
  # the ONLY AOI-inside point there means the app-level AOI readout can only
  # show 1 inside point if it's computed against the FULL, untrimmed data.
  x[4] <- 5
  y[4] <- 5
  readr::write_tsv(
    data.frame(qc_metric = "valid_raw_data", percent = 0.9),
    file.path(dir, "sub-c_ses-1_desc-fileC_preproc_qcsummary.tsv")
  )
  readr::write_tsv(
    data.frame(
      recordingTimestamp_ms = seq_len(n),
      gazeX.preprocessed_px = x,
      gazeY.preprocessed_px = y
    ),
    file.path(dir, "sub-c_ses-1_desc-fileC_preproc.tsv")
  )
  dir <- normalizePath(dir, winslash = "/", mustWork = TRUE)
  path_segments <- rel_home_segments_p1007(dir)

  shiny::testServer(analyze_app_dir, {
    session$setInputs(analyze_directory = list(root = "Home", path = path_segments), analyze_recursiveSearch = FALSE)
    session$setInputs(load = 1)
    session$setInputs(qc_table_rows_selected = 1)

    # updateSliderInput()'s reset is a no-op on input$gaze_time_range itself
    # under a bare MockShinySession (see capturing_update_session()'s own
    # comment elsewhere in this file) -- reflect the real browser echo of
    # this file's own full [1, 6000] bounds by hand.
    session$setInputs(gaze_time_range = c(1, n))

    session$setInputs(gaze_define_aoi = 1)
    session$setInputs(
      gaze_aoi1x = 0, gaze_aoi1y = 0,
      gaze_aoi2x = 10, gaze_aoi2y = 0,
      gaze_aoi3x = 10, gaze_aoi3y = 10,
      gaze_aoi4x = 0, gaze_aoi4y = 10
    )
    session$setInputs(gaze_aoi_submit = 1)

    result <- gaze_aoi_result()
    expect_equal(result$n_total, 6000)
    expect_equal(result$n_inside, 1) # would be 0 if wired against the thinned copy instead
  })
})

# ---------------------------------------------------------------------------
# Shared file selector / notes / Compare-files scatterplot redesign
# (helpers.R + app.R) -- Shiny-free unit tests first, live shiny::testServer()
# coverage further down.
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# build_file_selector_choices()
# ---------------------------------------------------------------------------

test_that("build_file_selector_choices returns character(0) for NULL, 0-row, or a table missing a required identity column", {
  expect_equal(build_file_selector_choices(NULL), character(0))

  tbl <- data.frame(recording = character(0), batch_name = character(0), source_file = character(0))
  expect_equal(build_file_selector_choices(tbl), character(0))

  missing_col <- data.frame(recording = "rec-1", source_file = "/d/f1.tsv", stringsAsFactors = FALSE)
  expect_equal(build_file_selector_choices(missing_col), character(0))
})

test_that("build_file_selector_choices labels an unambiguous recording with its plain recording id and sorts by label", {
  tbl <- data.frame(
    recording = c("rec-b", "rec-a"),
    batch_name = c("runA", "runA"),
    source_file = c("/d/b.tsv", "/d/a.tsv"),
    stringsAsFactors = FALSE
  )
  choices <- build_file_selector_choices(tbl)

  expect_equal(names(choices), c("rec-a", "rec-b"))
  expect_equal(unname(choices), c("/d/a.tsv", "/d/b.tsv"))
})

test_that("build_file_selector_choices disambiguates a recording spanning more than one batch_name with a '[batch_name]' suffix, including the NA batch_name case", {
  tbl <- data.frame(
    recording = c("rec-1", "rec-1", "rec-2"),
    batch_name = c(NA_character_, "runB", "runA"),
    source_file = c("/d/f1.tsv", "/d/f2.tsv", "/d/f3.tsv"),
    stringsAsFactors = FALSE
  )
  choices <- build_file_selector_choices(tbl)

  expect_equal(names(choices), c("rec-1 [NA]", "rec-1 [runB]", "rec-2"))
  expect_equal(unname(choices), c("/d/f1.tsv", "/d/f2.tsv", "/d/f3.tsv"))
})

# ---------------------------------------------------------------------------
# major_qc_metrics() / build_major_metrics_summary()
# ---------------------------------------------------------------------------

test_that("major_qc_metrics returns exactly the 4 qc_metric values major_qc_metric_display_labels covers", {
  expect_setequal(
    major_qc_metrics(),
    c(
      "valid_raw_data", "robustness_proportion_valid_data_to_all_data",
      "interpolated_LeftEye", "interpolated_RightEye"
    )
  )
  expect_length(major_qc_metrics(), 4)
})

test_that("build_major_metrics_summary returns only the major-metric rows for the requested file, correctly labeled and flagged, excluding a non-major metric present for the same file", {
  tbl <- data.frame(
    source_file = c("f1", "f1", "f1", "f1", "f1", "f2"),
    qc_metric = c(
      "valid_raw_data", "robustness_proportion_valid_data_to_all_data",
      "interpolated_LeftEye", "interpolated_RightEye", "blinks_BothEyes",
      "valid_raw_data"
    ),
    percent = c(0.9, 0.8, 0.05, 0.06, 0.2, 0.99),
    qc_flag = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  result <- build_major_metrics_summary(tbl, "f1")

  expect_equal(nrow(result), 4)
  by_label <- setNames(result$percent, result$label)
  expect_equal(by_label[["Valid raw data"]], 0.9)
  expect_equal(by_label[["Robust data"]], 0.8)
  expect_equal(by_label[["Interpolated (L)"]], 0.05)
  expect_equal(by_label[["Interpolated (R)"]], 0.06)

  flag_by_label <- setNames(result$qc_flag, result$label)
  expect_true(flag_by_label[["Interpolated (R)"]])
  expect_false(flag_by_label[["Interpolated (L)"]])
})

test_that("build_major_metrics_summary returns NULL for a NULL table, NULL/empty-string source_file, or a file with no matching major-metric rows", {
  tbl <- data.frame(
    source_file = "f1", qc_metric = "valid_raw_data", percent = 0.9, qc_flag = FALSE,
    stringsAsFactors = FALSE
  )

  expect_null(build_major_metrics_summary(NULL, "f1"))
  expect_null(build_major_metrics_summary(tbl, NULL))
  expect_null(build_major_metrics_summary(tbl, ""))
  expect_null(build_major_metrics_summary(tbl, "not-a-real-file"))
})

# ---------------------------------------------------------------------------
# build_qc_comparison_scatter()
# ---------------------------------------------------------------------------
#
# find_vline_layer: the geom_vline() counterpart to this file's own
# find_hline_layer() (P10-04, above) -- geom_vline(xintercept = ...)'s value
# is likewise stored on the layer's own `data` slot, not as a mapped
# aesthetic.
find_vline_layer <- function(plot) {
  for (l in plot$layers) {
    if (inherits(l$geom, "GeomVline")) {
      return(l)
    }
  }
  NULL
}

# scatter_test_table: 4 files, each reporting both valid_raw_data and
# robustness_proportion_valid_data_to_all_data -- rec-1 flagged on neither,
# rec-2 flagged only on the x metric, rec-3 flagged only on the y metric,
# rec-4 flagged on both, covering every comparison_status combination
# build_qc_comparison_scatter()'s "flagged on EITHER metric" rule needs to
# get right.
scatter_test_table <- function() {
  data.frame(
    recording = rep(c("rec-1", "rec-2", "rec-3", "rec-4"), each = 2),
    batch_name = "test_batch",
    source_file = rep(c("/d/rec-1.tsv", "/d/rec-2.tsv", "/d/rec-3.tsv", "/d/rec-4.tsv"), each = 2),
    qc_metric = rep(
      c("valid_raw_data", "robustness_proportion_valid_data_to_all_data"),
      times = 4
    ),
    percent = c(0.9, 0.95, 0.5, 0.95, 0.9, 0.55, 0.5, 0.55),
    qc_flag = c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
}

test_that("build_qc_comparison_scatter returns one point per file with the correct x/y values", {
  tbl <- scatter_test_table()
  plot <- build_qc_comparison_scatter(
    tbl, "valid_raw_data", "robustness_proportion_valid_data_to_all_data", default_qc_thresholds()
  )

  expect_s3_class(plot, "ggplot")
  expect_equal(nrow(plot$data), 4)

  by_recording <- setNames(seq_len(4), plot$data$recording)
  expect_equal(plot$data$valid_raw_data[by_recording[["rec-2"]]], 0.5)
  expect_equal(plot$data$robustness_proportion_valid_data_to_all_data[by_recording[["rec-2"]]], 0.95)
})

test_that("build_qc_comparison_scatter colors a point Flagged if EITHER of the two plotted metrics is flagged for that file, OK only when neither is", {
  tbl <- scatter_test_table()
  plot <- build_qc_comparison_scatter(
    tbl, "valid_raw_data", "robustness_proportion_valid_data_to_all_data", default_qc_thresholds()
  )

  by_recording <- setNames(as.character(plot$data$comparison_status), plot$data$recording)
  expect_equal(by_recording[["rec-1"]], "OK")
  expect_equal(by_recording[["rec-2"]], "Flagged")
  expect_equal(by_recording[["rec-3"]], "Flagged")
  expect_equal(by_recording[["rec-4"]], "Flagged")
})

test_that("build_qc_comparison_scatter draws a dashed threshold reference line only on the axis with a configured, non-NA threshold", {
  tbl <- scatter_test_table()
  thresholds <- default_qc_thresholds()
  thresholds$valid_pct <- 0.7
  thresholds$robust_pct <- NA

  plot <- build_qc_comparison_scatter(
    tbl, "valid_raw_data", "robustness_proportion_valid_data_to_all_data", thresholds
  )

  vline <- find_vline_layer(plot)
  expect_false(is.null(vline))
  expect_equal(vline$data$xintercept, 0.7)
  expect_true(is.null(find_hline_layer(plot)))
})

test_that("build_qc_comparison_scatter draws both dashed threshold reference lines when both axes have a configured threshold", {
  tbl <- scatter_test_table()
  thresholds <- default_qc_thresholds()
  thresholds$valid_pct <- 0.7
  thresholds$robust_pct <- 0.6

  plot <- build_qc_comparison_scatter(
    tbl, "valid_raw_data", "robustness_proportion_valid_data_to_all_data", thresholds
  )

  expect_equal(find_vline_layer(plot)$data$xintercept, 0.7)
  expect_equal(find_hline_layer(plot)$data$yintercept, 0.6)
})

test_that("build_qc_comparison_scatter returns NULL for identical metric_x/metric_y, a NULL table, or a metric absent from the table", {
  tbl <- scatter_test_table()

  expect_null(build_qc_comparison_scatter(tbl, "valid_raw_data", "valid_raw_data", default_qc_thresholds()))
  expect_null(build_qc_comparison_scatter(
    NULL, "valid_raw_data", "robustness_proportion_valid_data_to_all_data", default_qc_thresholds()
  ))
  expect_null(build_qc_comparison_scatter(
    tbl, "valid_raw_data", "some_metric_not_in_table", default_qc_thresholds()
  ))
})

# ---------------------------------------------------------------------------
# Per-file review notes: empty_notes_table() / load_notes_table() /
# save_notes_table() / resolve_notes_path()
# ---------------------------------------------------------------------------

test_that("load_notes_table returns empty_notes_table() when no notes file exists yet in the directory", {
  dir <- tempfile("p10notes_missing_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  expect_equal(load_notes_table(dir), empty_notes_table())
})

test_that("save_notes_table then load_notes_table round-trips note rows exactly, including a genuinely NA batch_name", {
  dir <- tempfile("p10notes_roundtrip_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  notes <- data.frame(
    recording = c("rec-1", "rec-2"),
    batch_name = c("runA", NA_character_),
    note = c("first note", "second note"),
    updated_at = c("2026-01-01T00:00:00+0000", "2026-01-02T00:00:00+0000"),
    stringsAsFactors = FALSE
  )
  save_notes_table(notes, dir)
  expect_true(file.exists(resolve_notes_path(dir)))

  reloaded <- load_notes_table(dir)
  expect_equal(reloaded$recording, notes$recording)
  expect_true(is.na(reloaded$batch_name[2]))
  expect_equal(reloaded$batch_name[1], "runA")
  expect_equal(reloaded$note, notes$note)
})

test_that("load_notes_table degrades to empty_notes_table(), not an error, for a wrong-shape file missing the expected columns", {
  dir <- tempfile("p10notes_wrongshape_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  readr::write_tsv(data.frame(foo = 1:2, bar = c("a", "b")), resolve_notes_path(dir))

  expect_equal(load_notes_table(dir), empty_notes_table())
})

test_that("load_notes_table degrades to empty_notes_table(), not an error, for a genuinely unparseable file", {
  dir <- tempfile("p10notes_corrupt_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  writeBin(as.raw(sample(0:255, 200, replace = TRUE)), resolve_notes_path(dir))

  expect_error(load_notes_table(dir), NA)
  expect_equal(load_notes_table(dir), empty_notes_table())
})

# ---------------------------------------------------------------------------
# upsert_note()
# ---------------------------------------------------------------------------

test_that("upsert_note inserts a new row for a recording that has no note yet", {
  result <- upsert_note(empty_notes_table(), "rec-1", "runA", "hello")

  expect_equal(nrow(result), 1)
  expect_equal(result$recording, "rec-1")
  expect_equal(result$batch_name, "runA")
  expect_equal(result$note, "hello")
})

test_that("upsert_note updates an existing row in place rather than duplicating it", {
  notes <- upsert_note(empty_notes_table(), "rec-1", "runA", "first")
  updated <- upsert_note(notes, "rec-1", "runA", "second, revised")

  expect_equal(nrow(updated), 1)
  expect_equal(updated$note, "second, revised")
})

test_that("upsert_note with a blank or whitespace-only note removes the existing row instead of saving empty text", {
  notes <- upsert_note(empty_notes_table(), "rec-1", "runA", "will be cleared")
  cleared_blank <- upsert_note(notes, "rec-1", "runA", "")
  expect_equal(nrow(cleared_blank), 0)

  notes2 <- upsert_note(empty_notes_table(), "rec-1", "runA", "will be cleared again")
  cleared_ws <- upsert_note(notes2, "rec-1", "runA", "   \n\t  ")
  expect_equal(nrow(cleared_ws), 0)
})

test_that("upsert_note matches an existing NA-batch_name row via is.na(), not plain ==, when replacing that recording's note", {
  # Regression guard for the specific bug class upsert_note()'s is.na()
  # branch (helpers.R) exists to prevent: notes_df$batch_name == NA
  # evaluates to NA (never TRUE) for every row, so `same_recording` would be
  # NA rather than TRUE for the row that should be replaced, and subsetting
  # a data.frame with a logical index containing NA produces a row of NAs
  # instead of dropping it. Confirmed directly (not just asserted) that
  # replacing the is.na() branch with `==` reproduces exactly this: the
  # existing row survives as an all-NA row alongside the newly rbind()ed
  # replacement, i.e. nrow() becomes 2 with a corrupted first row, rather
  # than the correct nrow() == 1 clean replacement asserted below.
  existing <- data.frame(
    recording = "rec-1", batch_name = NA_character_, note = "old note",
    updated_at = "2026-01-01T00:00:00+0000", stringsAsFactors = FALSE
  )
  updated <- upsert_note(existing, "rec-1", NA_character_, "new note")

  expect_equal(nrow(updated), 1)
  expect_true(is.na(updated$batch_name[1]))
  expect_equal(updated$note[1], "new note")
  expect_false(is.na(updated$recording[1]))
})

test_that("upsert_note treats two different recordings that both have NA batch_name as distinct rows, keyed by recording, not merged", {
  notes <- upsert_note(empty_notes_table(), "rec-1", NA_character_, "note for rec-1")
  notes <- upsert_note(notes, "rec-2", NA_character_, "note for rec-2")
  expect_equal(nrow(notes), 2)

  # Updating rec-1's note must not touch rec-2's row.
  notes <- upsert_note(notes, "rec-1", NA_character_, "updated note for rec-1")
  expect_equal(nrow(notes), 2)

  by_recording <- setNames(notes$note, notes$recording)
  expect_equal(by_recording[["rec-1"]], "updated note for rec-1")
  expect_equal(by_recording[["rec-2"]], "note for rec-2")
})

# ---------------------------------------------------------------------------
# note_for_recording()
# ---------------------------------------------------------------------------

test_that("note_for_recording returns the saved note text for a matching recording/batch_name pair", {
  notes <- data.frame(
    recording = c("rec-1", "rec-2"), batch_name = c("runA", "runB"),
    note = c("note one", "note two"), updated_at = c("t1", "t2"), stringsAsFactors = FALSE
  )
  expect_equal(note_for_recording(notes, "rec-2", "runB"), "note two")
})

test_that("note_for_recording returns '' for a recording with no saved note", {
  notes <- data.frame(
    recording = "rec-1", batch_name = "runA", note = "only note", updated_at = "t1",
    stringsAsFactors = FALSE
  )
  expect_equal(note_for_recording(notes, "rec-2", "runA"), "")
})

test_that("note_for_recording matches a NA batch_name row correctly rather than just the plain recording", {
  notes <- data.frame(
    recording = c("rec-1", "rec-1"), batch_name = c(NA_character_, "runB"),
    note = c("na-batch note", "runB note"), updated_at = c("t1", "t2"), stringsAsFactors = FALSE
  )
  expect_equal(note_for_recording(notes, "rec-1", NA_character_), "na-batch note")
  expect_equal(note_for_recording(notes, "rec-1", "runB"), "runB note")
})

test_that("note_for_recording returns '' for a NULL notes_df, NULL recording, or a 0-row table, rather than erroring", {
  notes <- data.frame(
    recording = "rec-1", batch_name = "runA", note = "x", updated_at = "t", stringsAsFactors = FALSE
  )

  expect_equal(note_for_recording(NULL, "rec-1", "runA"), "")
  expect_equal(note_for_recording(empty_notes_table(), "rec-1", "runA"), "")
  expect_equal(note_for_recording(notes, NULL, "runA"), "")
})

# ---------------------------------------------------------------------------
# build_flagged_export_table(..., notes = NULL) -- new optional parameter
# ---------------------------------------------------------------------------

test_that("build_flagged_export_table with notes = NULL (the default) still returns the original 6-column shape, backward compatible with every existing 1-argument call", {
  tbl <- flagged_export_test_table(
    recording = "rec-1", batch_name = "test_batch", source_file = "/data/rec-1_qcsummary.tsv",
    qc_metric = "valid_raw_data", percent = 0.5, qc_flag = TRUE
  )
  result <- build_flagged_export_table(tbl)

  expect_equal(
    colnames(result),
    c("recording", "batch_name", "source_file", "n_flagged_metrics", "flagged_metrics", "flagged_values")
  )
  expect_false("note" %in% colnames(result))
})

test_that("build_flagged_export_table with a notes table adds a correctly matched note column, blank for a flagged file with no saved note", {
  tbl <- rbind(
    flagged_export_test_table(
      recording = "rec-1", batch_name = "runA", source_file = "/data/rec-1_qcsummary.tsv",
      qc_metric = "valid_raw_data", percent = 0.5, qc_flag = TRUE
    ),
    flagged_export_test_table(
      recording = "rec-2", batch_name = "runA", source_file = "/data/rec-2_qcsummary.tsv",
      qc_metric = "valid_raw_data", percent = 0.4, qc_flag = TRUE
    )
  )
  notes <- data.frame(
    recording = "rec-1", batch_name = "runA", note = "excessive blinking", updated_at = "t1",
    stringsAsFactors = FALSE
  )

  result <- build_flagged_export_table(tbl, notes = notes)

  expect_true("note" %in% colnames(result))
  by_recording <- setNames(result$note, result$recording)
  expect_equal(by_recording[["rec-1"]], "excessive blinking")
  expect_equal(by_recording[["rec-2"]], "")
})

test_that("build_flagged_export_table's notes join matches a genuinely NA batch_name row correctly", {
  tbl <- flagged_export_test_table(
    recording = "rec-1", batch_name = NA_character_, source_file = "/data/rec-1_qcsummary.tsv",
    qc_metric = "valid_raw_data", percent = 0.5, qc_flag = TRUE
  )
  notes <- data.frame(
    recording = "rec-1", batch_name = NA_character_, note = "no-batchName run note",
    updated_at = "t1", stringsAsFactors = FALSE
  )

  result <- build_flagged_export_table(tbl, notes = notes)
  expect_equal(result$note, "no-batchName run note")
})

# ---------------------------------------------------------------------------
# Live shiny::testServer() coverage: shared file selector, notes, Compare
# files 1-vs-2-metric branch, Plots/Gaze Explorer badges.
# ---------------------------------------------------------------------------

test_that("selecting a file via one file selector reflects into selected_source_file() and pushes the same selection to the other two selector widgets, and a QC table row click does the same", {
  skip_on_cran()

  dir <- tempfile("p10fileselector_sync_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  dir <- normalizePath(dir, winslash = "/", mustWork = TRUE)

  readr::write_tsv(
    data.frame(qc_metric = "valid_raw_data", percent = 0.9),
    file.path(dir, "sub-a_ses-1_desc-selA_preproc_qcsummary.tsv")
  )
  readr::write_tsv(
    data.frame(qc_metric = "valid_raw_data", percent = 0.5),
    file.path(dir, "sub-b_ses-1_desc-selB_preproc_qcsummary.tsv")
  )

  path_segments <- rel_home_segments_p1007(dir)
  cs <- capturing_update_session()

  shiny::testServer(analyze_app_dir, {
    session$setInputs(analyze_directory = list(root = "Home", path = path_segments), analyze_recursiveSearch = FALSE)
    session$setInputs(load = 1)

    tbl <- current_load_result()$table
    file_a <- unique(tbl$source_file[grepl("selA", tbl$source_file)])
    file_b <- unique(tbl$source_file[grepl("selB", tbl$source_file)])
    expect_length(file_a, 1)
    expect_length(file_b, 1)

    session$setInputs(qcflags_file_selector = file_a)
    expect_equal(selected_source_file(), file_a)

    plots_calls <- get("plots_file_selector", envir = cs$calls)
    expect_equal(plots_calls[[length(plots_calls)]]$value, file_a)
    gaze_calls <- get("gaze_file_selector", envir = cs$calls)
    expect_equal(gaze_calls[[length(gaze_calls)]]$value, file_a)

    # a DIFFERENT selector's own change becomes the new shared selection...
    session$setInputs(gaze_file_selector = file_b)
    expect_equal(selected_source_file(), file_b)
    qcflags_calls <- get("qcflags_file_selector", envir = cs$calls)
    expect_equal(qcflags_calls[[length(qcflags_calls)]]$value, file_b)

    # ...and a QC table row click (P10-03) is reflected the same way.
    idx_a <- which(tbl$source_file == file_a)[1]
    session$setInputs(qc_table_rows_selected = idx_a)
    expect_equal(selected_source_file(), file_a)
    plots_calls2 <- get("plots_file_selector", envir = cs$calls)
    expect_equal(plots_calls2[[length(plots_calls2)]]$value, file_a)
  }, session = cs$session)
})

test_that("output$qc_major_table renders without error on a real load, and the same build_qc_comparison_table(tbl, major_qc_metrics()) pipeline it uses internally produces the correct major-metric values, excluding a non-major metric", {
  # output$qc_major_table's DT::datatable() call doesn't pass server = FALSE
  # (unlike output$qc_table's own deliberate opt-out, see that render's own
  # comment on why it embeds data for testability) -- it defaults to the
  # same server = TRUE DT::renderDT() default output$compare_table already
  # accepts uninspected elsewhere in this file, so the rendered widget's
  # underlying row data isn't embedded in session$getOutput()'s JSON the way
  # output$qc_table's is. Confirmed directly (not assumed) via a scratch
  # comparison against output$qc_table's own embedded-data behavior before
  # writing this test this way, rather than assuming either shape. This test
  # therefore checks the render doesn't error against a real load (the part
  # session$getOutput() genuinely exercises), and separately calls the exact
  # same helper pipeline (build_qc_comparison_table(tbl, major_qc_metrics()))
  # output$qc_major_table's own render body uses, against the live
  # current_load_result()$table, to confirm the values that pipeline
  # produces are correct.
  skip_on_cran()

  dir <- tempfile("p10qcmajor_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  dir <- normalizePath(dir, winslash = "/", mustWork = TRUE)

  readr::write_tsv(
    data.frame(
      qc_metric = c(
        "valid_raw_data", "robustness_proportion_valid_data_to_all_data",
        "interpolated_LeftEye", "interpolated_RightEye", "blinks_BothEyes"
      ),
      percent = c(0.876, 0.654, 0.321, 0.048, 0.999)
    ),
    file.path(dir, "sub-m_ses-1_desc-majortbl_preproc_qcsummary.tsv")
  )
  path_segments <- rel_home_segments_p1007(dir)

  shiny::testServer(analyze_app_dir, {
    session$setInputs(analyze_directory = list(root = "Home", path = path_segments), analyze_recursiveSearch = FALSE)
    session$setInputs(load = 1)

    expect_error(session$getOutput("qc_major_table"), NA)

    major_tbl <- build_qc_comparison_table(current_load_result()$table, major_qc_metrics())
    expect_equal(nrow(major_tbl), 1)
    expect_false("blinks_BothEyes" %in% names(major_tbl))
    expect_equal(major_tbl$valid_raw_data[1], 0.876)
    expect_equal(major_tbl$robustness_proportion_valid_data_to_all_data[1], 0.654)
    expect_equal(major_tbl$interpolated_LeftEye[1], 0.321)
    expect_equal(major_tbl$interpolated_RightEye[1], 0.048)
  })
})

test_that("typing and saving a note for the selected file persists it to eyeQuality_analyze_notes.tsv in the loaded directory", {
  skip_on_cran()

  dir <- tempfile("p10notes_save_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  dir <- normalizePath(dir, winslash = "/", mustWork = TRUE)

  readr::write_tsv(
    data.frame(qc_metric = "valid_raw_data", percent = 0.9),
    file.path(dir, "sub-n_ses-1_desc-notesave_preproc_qcsummary.tsv")
  )
  path_segments <- rel_home_segments_p1007(dir)

  shiny::testServer(analyze_app_dir, {
    session$setInputs(analyze_directory = list(root = "Home", path = path_segments), analyze_recursiveSearch = FALSE)
    session$setInputs(load = 1)

    tbl <- current_load_result()$table
    file_path <- unique(tbl$source_file)[1]
    session$setInputs(qcflags_file_selector = file_path)

    session$setInputs(qcflags_note_text = "excessive blinking, re-run recommended")
    session$setInputs(qcflags_save_note = 1)

    notes_path <- file.path(dir, "eyeQuality_analyze_notes.tsv")
    expect_true(file.exists(notes_path))

    on_disk <- readr::read_tsv(notes_path, show_col_types = FALSE)
    expect_equal(nrow(on_disk), 1)
    expect_equal(on_disk$note[1], "excessive blinking, re-run recommended")
    expect_equal(on_disk$recording[1], unique(tbl$recording)[1])

    status_html <- renderui_html(session$getOutput("qcflags_note_status"))
    expect_match(status_html, "Note saved.", fixed = TRUE)
  })
})

test_that("switching the selected file updates the notes textarea to THAT file's own note, not a carried-over value from the previous file", {
  skip_on_cran()

  dir <- tempfile("p10notes_switch_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  dir <- normalizePath(dir, winslash = "/", mustWork = TRUE)

  readr::write_tsv(
    data.frame(qc_metric = "valid_raw_data", percent = 0.9),
    file.path(dir, "sub-x_ses-1_desc-switchA_preproc_qcsummary.tsv")
  )
  readr::write_tsv(
    data.frame(qc_metric = "valid_raw_data", percent = 0.5),
    file.path(dir, "sub-y_ses-1_desc-switchB_preproc_qcsummary.tsv")
  )
  path_segments <- rel_home_segments_p1007(dir)
  cs <- capturing_update_session()

  shiny::testServer(analyze_app_dir, {
    session$setInputs(analyze_directory = list(root = "Home", path = path_segments), analyze_recursiveSearch = FALSE)
    session$setInputs(load = 1)

    tbl <- current_load_result()$table
    file_a <- unique(tbl$source_file[grepl("switchA", tbl$source_file)])
    file_b <- unique(tbl$source_file[grepl("switchB", tbl$source_file)])

    session$setInputs(qcflags_file_selector = file_a)
    session$setInputs(qcflags_note_text = "note only for file A")
    session$setInputs(qcflags_save_note = 1)

    # switch to file B, which has no saved note
    session$setInputs(qcflags_file_selector = file_b)
    calls <- get("qcflags_note_text", envir = cs$calls)
    expect_equal(calls[[length(calls)]]$value, "")

    # switch back to file A: its own note must reappear
    session$setInputs(qcflags_file_selector = file_a)
    calls2 <- get("qcflags_note_text", envir = cs$calls)
    expect_equal(calls2[[length(calls2)]]$value, "note only for file A")
  }, session = cs$session)
})

test_that("reloading the same directory in a fresh app session repopulates a previously saved note, including into the notes textarea once that file is selected", {
  skip_on_cran()

  dir <- tempfile("p10notes_reload_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  dir <- normalizePath(dir, winslash = "/", mustWork = TRUE)

  readr::write_tsv(
    data.frame(qc_metric = "valid_raw_data", percent = 0.9),
    file.path(dir, "sub-r_ses-1_desc-reload_preproc_qcsummary.tsv")
  )
  path_segments <- rel_home_segments_p1007(dir)

  # First app instance: save a note, then let it end -- standing in for an
  # app restart / a different reviewer opening this directory later.
  shiny::testServer(analyze_app_dir, {
    session$setInputs(analyze_directory = list(root = "Home", path = path_segments), analyze_recursiveSearch = FALSE)
    session$setInputs(load = 1)
    tbl <- current_load_result()$table
    session$setInputs(qcflags_file_selector = unique(tbl$source_file)[1])
    session$setInputs(qcflags_note_text = "saved before restart")
    session$setInputs(qcflags_save_note = 1)
  })

  cs <- capturing_update_session()
  shiny::testServer(analyze_app_dir, {
    session$setInputs(analyze_directory = list(root = "Home", path = path_segments), analyze_recursiveSearch = FALSE)
    session$setInputs(load = 1)

    reloaded_notes <- notes_store()
    expect_equal(nrow(reloaded_notes), 1)
    expect_equal(reloaded_notes$note[1], "saved before restart")

    tbl <- current_load_result()$table
    session$setInputs(qcflags_file_selector = unique(tbl$source_file)[1])

    calls <- get("qcflags_note_text", envir = cs$calls)
    expect_equal(calls[[length(calls)]]$value, "saved before restart")
  }, session = cs$session)
})

test_that("clearing the note text and saving removes that file's row from notes.tsv rather than leaving a blank note behind", {
  skip_on_cran()

  dir <- tempfile("p10notes_clear_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  dir <- normalizePath(dir, winslash = "/", mustWork = TRUE)

  readr::write_tsv(
    data.frame(qc_metric = "valid_raw_data", percent = 0.9),
    file.path(dir, "sub-c_ses-1_desc-clearnote_preproc_qcsummary.tsv")
  )
  path_segments <- rel_home_segments_p1007(dir)

  shiny::testServer(analyze_app_dir, {
    session$setInputs(analyze_directory = list(root = "Home", path = path_segments), analyze_recursiveSearch = FALSE)
    session$setInputs(load = 1)
    tbl <- current_load_result()$table
    session$setInputs(qcflags_file_selector = unique(tbl$source_file)[1])

    session$setInputs(qcflags_note_text = "will be cleared")
    session$setInputs(qcflags_save_note = 1)
    notes_path <- file.path(dir, "eyeQuality_analyze_notes.tsv")
    expect_equal(nrow(readr::read_tsv(notes_path, show_col_types = FALSE)), 1)

    session$setInputs(qcflags_note_text = "")
    session$setInputs(qcflags_save_note = 2)

    on_disk <- readr::read_tsv(notes_path, show_col_types = FALSE)
    expect_equal(nrow(on_disk), 0)
    expect_equal(nrow(notes_store()), 0)
  })
})

test_that("the flagged-for-review CSV export's note column reflects the currently saved note for a flagged file", {
  skip_on_cran()

  dir <- tempfile("p10notes_export_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  dir <- normalizePath(dir, winslash = "/", mustWork = TRUE)

  # valid_raw_data at 0.5 crosses the default 80% min threshold -> flagged.
  readr::write_tsv(
    data.frame(qc_metric = "valid_raw_data", percent = 0.5),
    file.path(dir, "sub-e_ses-1_desc-notesexport_preproc_qcsummary.tsv")
  )
  path_segments <- rel_home_segments_p1007(dir)

  shiny::testServer(analyze_app_dir, {
    session$setInputs(analyze_directory = list(root = "Home", path = path_segments), analyze_recursiveSearch = FALSE)
    session$setInputs(load = 1)
    tbl <- current_load_result()$table
    session$setInputs(qcflags_file_selector = unique(tbl$source_file)[1])
    session$setInputs(qcflags_note_text = "flagged for low validity")
    session$setInputs(qcflags_save_note = 1)

    downloaded_path <- session$getOutput("download_flagged_csv")
    result <- utils::read.csv(downloaded_path, stringsAsFactors = FALSE)

    expect_equal(nrow(result), 1)
    expect_true("note" %in% names(result))
    expect_equal(result$note[1], "flagged for low validity")
  })
})

test_that("selecting exactly 1 Compare-files metric still renders the original per-file bar-chart height formula, unchanged by the new 2-metric scatterplot option", {
  skip_on_cran()

  dir <- tempfile("p10compare_bar_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  dir <- normalizePath(dir, winslash = "/", mustWork = TRUE)

  n_files <- 20
  for (i in seq_len(n_files)) {
    readr::write_tsv(
      data.frame(
        qc_metric = c("valid_raw_data", "robustness_proportion_valid_data_to_all_data"),
        percent = c(0.9, 0.8)
      ),
      file.path(dir, sprintf("sub-%02d_ses-1_desc-cmpbar_preproc_qcsummary.tsv", i))
    )
  }
  path_segments <- rel_home_segments_p1007(dir)

  shiny::testServer(analyze_app_dir, {
    session$setInputs(analyze_directory = list(root = "Home", path = path_segments), analyze_recursiveSearch = FALSE)
    session$setInputs(load = 1)
    session$setInputs(
      compare_batch_name_filter = "cmpbar",
      compare_metric_plot = "valid_raw_data",
      compare_metrics_table = "valid_raw_data"
    )

    expect_equal(session$getOutput("compare_plot")$height, n_files * 28 + 120)
  })
})

test_that("selecting 2 Compare-files metrics switches to the fixed-height scatterplot instead of scaling with file count", {
  skip_on_cran()

  dir <- tempfile("p10compare_scatter_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  dir <- normalizePath(dir, winslash = "/", mustWork = TRUE)

  n_files <- 20
  for (i in seq_len(n_files)) {
    readr::write_tsv(
      data.frame(
        qc_metric = c("valid_raw_data", "robustness_proportion_valid_data_to_all_data"),
        percent = c(0.9, 0.8)
      ),
      file.path(dir, sprintf("sub-%02d_ses-1_desc-cmpscatter_preproc_qcsummary.tsv", i))
    )
  }
  path_segments <- rel_home_segments_p1007(dir)

  shiny::testServer(analyze_app_dir, {
    session$setInputs(analyze_directory = list(root = "Home", path = path_segments), analyze_recursiveSearch = FALSE)
    session$setInputs(load = 1)
    session$setInputs(
      compare_batch_name_filter = "cmpscatter",
      compare_metric_plot = c("valid_raw_data", "robustness_proportion_valid_data_to_all_data"),
      compare_metrics_table = "valid_raw_data"
    )

    # Would equal n_files * 28 + 120 = 680 if this still (incorrectly) took
    # the bar-chart branch instead of switching to the scatterplot's fixed
    # height once a second metric is selected.
    expect_equal(session$getOutput("compare_plot")$height, 550)
  })
})

test_that("output$compare_plot renders nothing (not a crash) if input$compare_metric_plot somehow holds more than 2 values, bypassing the UI's client-side maxItems = 2 cap", {
  skip_on_cran()

  dir <- tempfile("p10compare_guard_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  dir <- normalizePath(dir, winslash = "/", mustWork = TRUE)

  readr::write_tsv(
    data.frame(
      qc_metric = c("valid_raw_data", "robustness_proportion_valid_data_to_all_data", "interpolated_LeftEye"),
      percent = c(0.9, 0.8, 0.05)
    ),
    file.path(dir, "sub-g_ses-1_desc-guard_preproc_qcsummary.tsv")
  )
  path_segments <- rel_home_segments_p1007(dir)

  shiny::testServer(analyze_app_dir, {
    session$setInputs(analyze_directory = list(root = "Home", path = path_segments), analyze_recursiveSearch = FALSE)
    session$setInputs(load = 1)
    session$setInputs(
      compare_batch_name_filter = "guard",
      compare_metric_plot = c("valid_raw_data", "robustness_proportion_valid_data_to_all_data", "interpolated_LeftEye")
    )

    plot_output <- tryCatch(
      session$getOutput("compare_plot"),
      shiny.silent.error = function(e) "guarded",
      validation = function(e) "guarded"
    )
    expect_equal(plot_output, "guarded")
  })
})

test_that("the Plots and Gaze Explorer tabs' major-metrics badges show the correct file's values and switch when the selected file changes", {
  skip_on_cran()

  dir <- tempfile("p10badges_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  dir <- normalizePath(dir, winslash = "/", mustWork = TRUE)

  readr::write_tsv(
    data.frame(
      qc_metric = c(
        "valid_raw_data", "robustness_proportion_valid_data_to_all_data",
        "interpolated_LeftEye", "interpolated_RightEye"
      ),
      percent = c(0.95, 0.9, 0.02, 0.02)
    ),
    file.path(dir, "sub-p_ses-1_desc-badgeA_preproc_qcsummary.tsv")
  )
  readr::write_tsv(
    data.frame(
      qc_metric = c(
        "valid_raw_data", "robustness_proportion_valid_data_to_all_data",
        "interpolated_LeftEye", "interpolated_RightEye"
      ),
      percent = c(0.5, 0.4, 0.5, 0.5)
    ),
    file.path(dir, "sub-q_ses-1_desc-badgeB_preproc_qcsummary.tsv")
  )
  path_segments <- rel_home_segments_p1007(dir)

  shiny::testServer(analyze_app_dir, {
    session$setInputs(analyze_directory = list(root = "Home", path = path_segments), analyze_recursiveSearch = FALSE)
    session$setInputs(load = 1)
    tbl <- current_load_result()$table
    file_a <- unique(tbl$source_file[grepl("badgeA", tbl$source_file)])
    file_b <- unique(tbl$source_file[grepl("badgeB", tbl$source_file)])

    session$setInputs(plots_file_selector = file_a)
    badges_a <- renderui_html(session$getOutput("plots_major_metrics_ui"))
    expect_match(badges_a, "95.0%", fixed = TRUE)
    expect_match(badges_a, "label-success", fixed = TRUE)
    expect_false(grepl("label-danger", badges_a, fixed = TRUE))

    session$setInputs(plots_file_selector = file_b)
    badges_b <- renderui_html(session$getOutput("plots_major_metrics_ui"))
    expect_match(badges_b, "50.0%", fixed = TRUE)
    expect_match(badges_b, "label-danger", fixed = TRUE) # valid_raw_data 0.5 crosses the default 80% min threshold

    # Gaze Explorer's own badge output mirrors the same shared selection.
    gaze_badges_b <- renderui_html(session$getOutput("gaze_major_metrics_ui"))
    expect_match(gaze_badges_b, "50.0%", fixed = TRUE)
  })
})

# ---------------------------------------------------------------------------
# Windows MAX_PATH (260-character) long-path handling
# ---------------------------------------------------------------------------
#
# A real qcsummary.tsv failing to load with a readr "does not exist" error
# despite genuinely existing on disk -- the well-known Windows 260-character
# MAX_PATH limitation, hit in practice by this app's own recursive directory
# search combined with this package's nested derivatives/eyeQuality-v1/
# output convention and long BIDS-style filenames sitting under a
# deeply-nested (e.g. Box-synced) study tree; one real batch's output
# directory produced 291 files that failed this way. helpers.R's
# windows_long_path()/windows_safe_read_tsv()/windows_safe_write_tsv()/
# windows_safe_file_exists() (and .safe_basename(), a distinct, separately
# discovered long-path failure mode inside base R's own basename()) exist to
# fix this -- see their header comments in helpers.R for the full mechanism
# and for why a naive readr::read_tsv(windows_long_path(path)) call turns
# out NOT to be sufficient on its own.
#
# This is deliberately end-to-end against every read/write call site that
# touches a filesystem-discovered path, not a unit test of windows_long_path()
# in isolation: the single most load-bearing thing to confirm is that a real
# file at a real path over 260 characters actually loads through this app's
# real public functions, on whatever platform this suite runs on. On
# Windows, this exercises the actual fix; on other platforms, MAX_PATH
# doesn't apply and windows_long_path() is a documented no-op, so this same
# test still validates nothing regressed for a long path in general.
#
# Deliberately does NOT go through eyeQualityBatch()/eyeQuality(saveData =
# TRUE) to write the fixture files at this long path -- that would exercise
# R/saveFiles.R's own file-writing, a separate, non-overlapping long-path fix
# tracked independently of this app-layer one. Instead, a real preprocessed
# data.frame is obtained via eyeQuality(saveData = FALSE) (in-memory,
# unaffected by any write-path issue), and written to disk directly via this
# file's own windows_safe_write_tsv() -- the same mechanism save_notes_table()
# itself now uses -- so this test is self-contained and doesn't depend on
# the core package's own long-path fix having landed yet.
test_that("read_one_qcsummary/load_qcsummary_table/load_plot_data/load_gaze_trajectory_data/save_notes_table+load_notes_table all succeed against a real path over 260 characters", {
  skip_on_cran()

  base_dir <- tempfile("p_longpath_")
  dir.create(base_dir)
  on.exit(unlink(base_dir, recursive = TRUE), add = TRUE)

  # Deeply nested, long directory segments -- mirroring a real Box-synced
  # study tree (e.g. "IBIS INFANT EYE TRACKING PREPROCESSING/ARCHIVE/
  # IBIS-EP_DATA/UMN/01_Calibration_Verification_Start/AUDIT_PASSED/...")
  # -- until the eventual full qcsummary.tsv path is comfortably past 260
  # characters.
  segment <- "IBIS_EP_DATA_UMN_01_Calibration_Verification_Start_AUDIT_PASSED"
  nested <- base_dir
  i <- 0
  while (nchar(nested) < 170) {
    i <- i + 1
    nested <- file.path(nested, paste0(segment, "_", i))
  }
  deriv_dir <- file.path(nested, "sub-UMN7001", "ses-06", "derivatives", "eyeQuality-v1")
  # base R's own dir.create(..., recursive = TRUE) turns out to share the
  # same MAX_PATH-adjacent failure mode as basename()/file.exists() (see
  # helpers.R's .safe_basename()/windows_safe_file_exists() header comments)
  # -- confirmed directly: a raw (unprefixed) dir.create() call on this exact
  # ~250-character deriv_dir silently fails partway through the recursive
  # walk ("cannot create dir ... No such file or directory"), leaving deeper
  # levels missing. Routed through windows_long_path() here purely as test
  # scaffolding (this test isn't about directory creation) so the fixture
  # setup itself is reliable regardless of length.
  dir.create(windows_long_path(deriv_dir), recursive = TRUE)

  stem <- "sub-UMN7001_ses-06_task-A1CalibrationVerificationStart_ET_desc-01"
  qcsummary_path <- file.path(deriv_dir, paste0(stem, "_preproc_qcsummary.tsv"))
  preproc_path <- file.path(deriv_dir, paste0(stem, "_preproc.tsv"))
  events_path <- file.path(deriv_dir, paste0(stem, "_events.tsv"))
  skip_if_not(nchar(qcsummary_path) > 260, "test setup failed to build a genuinely >260-character path")

  raw_file <- system.file("extdata", "tobii_studio_sample.tsv", package = "eyeQuality")
  real_preproc_df <- suppressMessages(eyeQuality(
    raw_file,
    displayDimensionX_mm = 594,
    displayDimensionY_mm = 344,
    saveData = FALSE
  ))

  qc_df <- data.frame(
    qc_metric = c(
      "valid_raw_data", "robustness_proportion_valid_data_to_all_data",
      "interpolated_LeftEye", "interpolated_RightEye"
    ),
    n = c(200, 200, 0, 0),
    percent = c(1.0, 1.0, 0.0, 0.0)
  )
  events_df <- data.frame(
    event = c("TrialStart", "TrialEnd"),
    recordingTimestamp_ms = c(
      real_preproc_df$recordingTimestamp_ms[1],
      real_preproc_df$recordingTimestamp_ms[nrow(real_preproc_df)]
    )
  )

  windows_safe_write_tsv(qc_df, qcsummary_path)
  windows_safe_write_tsv(real_preproc_df, preproc_path)
  windows_safe_write_tsv(events_df, events_path)

  # read_one_qcsummary() -- exercises windows_safe_read_tsv() AND
  # .safe_basename() (via derive_recording_label()/derive_batch_name()).
  qc_row <- read_one_qcsummary(qcsummary_path)
  expect_s3_class(qc_row, "data.frame")
  expect_equal(nrow(qc_row), 4)
  expect_equal(qc_row$recording[1], "sub-UMN7001_ses-06_task-A1CalibrationVerificationStart_ET")
  expect_equal(qc_row$batch_name[1], "01")

  # load_qcsummary_table() -- exercises discover_qcsummary_files()'s
  # list.files(recursive = TRUE) over the whole long nested tree too.
  loaded <- load_qcsummary_table(base_dir, recursive = TRUE)
  expect_equal(loaded$n_files, 1L)
  expect_length(loaded$read_errors, 0)
  expect_s3_class(loaded$table, "data.frame")

  # load_plot_data() -- the exact reported-bug code path: resolves and reads
  # the sibling preproc.tsv, then calls the real generateEyeTrackingPlots().
  plot_result <- load_plot_data(qcsummary_path)
  expect_true(plot_result$ok)
  expect_length(plot_result$plots, 3)
  expect_equal(nrow(plot_result$data), nrow(real_preproc_df))

  # load_gaze_trajectory_data() -- reads preproc.tsv AND the sibling
  # events.tsv.
  gaze_result <- load_gaze_trajectory_data(qcsummary_path)
  expect_true(gaze_result$ok)
  expect_false(is.null(gaze_result$events))

  # save_notes_table()/load_notes_table() round trip -- notes.tsv sits at
  # base_dir's own root, itself a long path given how deeply nested base_dir's
  # own tail is.
  notes <- load_notes_table(base_dir)
  expect_equal(nrow(notes), 0)
  notes <- upsert_note(
    notes, derive_recording_label(qcsummary_path), derive_batch_name(qcsummary_path),
    "flagged for re-review"
  )
  save_notes_table(notes, base_dir)
  notes_reloaded <- load_notes_table(base_dir)
  expect_equal(nrow(notes_reloaded), 1)
  expect_equal(notes_reloaded$note[1], "flagged for re-review")

  # Negative control: a genuinely missing file at an equally long path must
  # still be correctly reported missing, not silently mistaken for existing.
  missing_qcsummary <- file.path(deriv_dir, "sub-DOES-NOT-EXIST_ses-99_task-x_desc-01_preproc_qcsummary.tsv")
  expect_error(read_one_qcsummary(missing_qcsummary))
  expect_false(windows_safe_file_exists(missing_qcsummary))
})
