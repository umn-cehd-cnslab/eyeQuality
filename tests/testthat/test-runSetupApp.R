# P9-01: regression tests for build_dry_run_preview(), the Shiny-free helper
# behind the Setup tab's dry-run preview panel (inst/shiny-apps/app/
# setup_helpers.R -- P10-12 merged the once-standalone Setup app into a
# "Setup & Run" tab of the combined eyeQuality app; setup_helpers.R is that
# tab's own helper file, sourced by inst/shiny-apps/app/app.R). Sourced via
# system.file() rather than a relative path, both because that's the only way
# to reach inst/ files portably from tests and because it doubles as a check
# that the app is packaged where runSetupApp() (R/runSetupApp.R) expects to
# find it.
#
# build_dry_run_preview() itself has no Shiny dependency, so these tests
# don't need a running Shiny session or shinyFiles at all.

helpers_path <- system.file("shiny-apps", "app", "setup_helpers.R", package = "eyeQuality")
if (!nzchar(helpers_path)) {
  stop("test-runSetupApp.R: could not locate inst/shiny-apps/app/setup_helpers.R via system.file()")
}
source(helpers_path, local = TRUE)

# Builds a small scratch directory tree with one file that matches a
# BIDS-like sub-XX/ses-XX layout and one stray top-level directory that
# doesn't match subjectPattern_regex, so bids-mode calls have something real
# to skip. Returns the tempdir path; caller is responsible for unlink().
build_scratch_tree <- function() {
  root <- tempfile("p901_scratch_")
  dir.create(file.path(root, "sub-01", "ses-01"), recursive = TRUE)
  dir.create(file.path(root, "sub-02", "ses-01"), recursive = TRUE)
  dir.create(file.path(root, "notasubject"), recursive = TRUE)
  file.create(file.path(root, "sub-01", "ses-01", "a.tsv"))
  file.create(file.path(root, "sub-02", "ses-01", "b.tsv"))
  file.create(file.path(root, "notasubject", "c.tsv"))
  root
}

test_that("build_dry_run_preview bids mode matches all four files in the checked-in fixture with nothing skipped", {
  result <- build_dry_run_preview(
    directory = testthat::test_path("fixtures", "bids"),
    layout = "bids"
  )

  expect_equal(result$matched_count, 4)
  expect_equal(result$skipped_count, 0)
  expect_length(result$skipped_items, 0)
  expect_true(all(grepl("\\.tsv$", result$matched_files)))
})

test_that("build_dry_run_preview bids mode reports a skipped directory that fails subjectPattern_regex", {
  root <- build_scratch_tree()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  result <- build_dry_run_preview(directory = root, layout = "bids")

  expect_equal(result$matched_count, 2)
  expect_equal(result$skipped_count, 1)
  expect_true(grepl("notasubject", result$skipped_items))
  expect_setequal(basename(result$matched_files), c("a.tsv", "b.tsv"))
})

test_that("build_dry_run_preview glob mode matches everything and reports nothing skipped when excludePattern_regex is unused", {
  root <- build_scratch_tree()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  result <- build_dry_run_preview(directory = root, layout = "glob", pathPattern = "**/*.tsv")

  expect_equal(result$matched_count, 3)
  expect_equal(result$skipped_count, 0)
})

test_that("build_dry_run_preview glob mode drops excludePattern_regex matches into skipped_items rather than the matched set", {
  root <- build_scratch_tree()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  result <- build_dry_run_preview(
    directory = root,
    layout = "glob",
    pathPattern = "**/*.tsv",
    excludePattern_regex = "notasubject"
  )

  expect_equal(result$matched_count, 2)
  expect_setequal(basename(result$matched_files), c("a.tsv", "b.tsv"))
  expect_equal(result$skipped_count, 1)
  expect_true(grepl("notasubject", result$skipped_items))
})

test_that("build_dry_run_preview reports a zero-match preview (not an error) when no files match", {
  root <- build_scratch_tree()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  result <- build_dry_run_preview(directory = root, layout = "glob", pathPattern = "**/*.csv")

  expect_equal(result$matched_count, 0)
  expect_length(result$matched_files, 0)
  expect_length(result$sample_files, 0)
  expect_equal(result$skipped_count, 0)
})

test_that("build_dry_run_preview errors clearly when the directory does not exist", {
  missing_dir <- file.path(tempdir(), "p901_does_not_exist_xyz")

  expect_error(
    build_dry_run_preview(directory = missing_dir, layout = "bids"),
    "does not exist"
  )
})

test_that("build_dry_run_preview errors when layout is 'glob' and pathPattern is NULL/empty", {
  root <- build_scratch_tree()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  expect_error(
    build_dry_run_preview(directory = root, layout = "glob", pathPattern = NULL),
    "pathPattern"
  )
})

test_that("build_dry_run_preview caps sample_files at sample_n while matched_count reflects the full total", {
  root <- tempfile("p901_many_")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  for (i in seq_len(12)) {
    file.create(file.path(root, sprintf("file_%02d.tsv", i)))
  }

  result <- build_dry_run_preview(directory = root, layout = "glob", pathPattern = "*.tsv", sample_n = 10)

  expect_equal(result$matched_count, 12)
  expect_length(result$sample_files, 10)
})

test_that("build_dry_run_preview rejects an unrecognized layout value", {
  root <- build_scratch_tree()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  expect_error(
    build_dry_run_preview(directory = root, layout = "flat"),
    "layout"
  )
})

# P7-06: manual QA against real (non-BIDS-named) nested study data found
# build_dry_run_preview() silently ignoring an explicit
# subjectPattern_regex/sessionPattern_regex = NULL (the app's own encoding
# of a blanked text field, per its "blank = every subfolder" hint) and
# falling back to listBidsFiles()'s built-in "sub-XX"/"ses-XX" defaults
# instead. Root cause: the args list passed to listBidsFiles() via do.call()
# was assembled incrementally with `args$name <- value`, and assigning NULL
# that way removes the list element rather than setting it to NULL, so
# do.call() never saw an explicit NULL to override listBidsFiles()'s own
# default argument value with.

build_non_bids_scratch_tree <- function() {
  root <- tempfile("p706_non_bids_scratch_")
  # non-BIDS subject naming (plain numeric/alphanumeric IDs), reproducing a
  # real-world nested study directory (dataset/site/task/audit-status/
  # subject) that doesn't fit the "sub-XX" convention
  dir.create(file.path(root, "1001"), recursive = TRUE)
  dir.create(file.path(root, "IBIS2002"), recursive = TRUE)
  file.create(file.path(root, "1001", "1001_et.tsv"))
  file.create(file.path(root, "IBIS2002", "IBIS2002_et.tsv"))
  root
}

test_that("build_dry_run_preview bids mode with default subjectPattern_regex matches nothing against non-BIDS subject folder names", {
  root <- build_non_bids_scratch_tree()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  out <- capture.output(result <- build_dry_run_preview(directory = root, layout = "bids"))

  expect_equal(result$matched_count, 0)
  expect_false(is.null(result$diagnostic_message))
  expect_match(result$diagnostic_message, "subjectPattern_regex", fixed = TRUE)
})

test_that("build_dry_run_preview bids mode with subjectPattern_regex = NULL actually searches every subfolder (regression: NULL was previously dropped, not passed through)", {
  root <- build_non_bids_scratch_tree()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  result <- build_dry_run_preview(
    directory = root,
    layout = "bids",
    subjectPattern_regex = NULL,
    sessionPattern_regex = NULL
  )

  expect_equal(result$matched_count, 2)
  expect_setequal(basename(result$matched_files), c("1001_et.tsv", "IBIS2002_et.tsv"))
  expect_null(result$diagnostic_message)
})

test_that("build_dry_run_preview glob mode finds the same non-BIDS-named files without needing subjectPattern_regex at all", {
  root <- build_non_bids_scratch_tree()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  result <- build_dry_run_preview(directory = root, layout = "glob", pathPattern = "**/*.tsv")

  expect_equal(result$matched_count, 2)
  expect_null(result$diagnostic_message)
})

test_that("build_dry_run_preview's diagnostic_message is NULL whenever at least one file matches", {
  result <- build_dry_run_preview(
    directory = testthat::test_path("fixtures", "bids"),
    layout = "bids"
  )

  expect_gt(result$matched_count, 0)
  expect_null(result$diagnostic_message)
})

# ---------------------------------------------------------------------------
# P9-04: save/load batch_config.yaml via the running app (shiny::testServer())
# ---------------------------------------------------------------------------
#
# These exercise app.R's server directly (not just helpers.R's plain
# functions), via shiny::testServer(app_dir, ...) -- same convention as
# test-runAnalyzeApp.R's live-reactive tests. shinyDirButton/shinySaveButton
# selections are simulated the same way test-runAnalyzeApp.R does: real
# tempfile() directories under fs::path_home() (so shinyFiles::parseDirPath()/
# parseSavePath() resolve them for real, not mocked), reduced to root/path
# segment lists relative to that root.
#
# Important limitation confirmed independently while writing these (not just
# asserted from the implementer's notes): shiny::testServer()'s
# MockShinySession$sendInputMessage() is a documented no-op --
# `body(shiny:::MockShinySession$new()$sendInputMessage)` literally contains
# the string "sendInputMessage is a noop." -- so update*Input() calls made by
# the "Load config" handler never actually change input$batchName etc. in
# this harness the way a real browser would. Tests below that need to
# exercise what happens AFTER a load (the resave round-trip) work around this
# by explicitly session$setInputs()-ing the fields a real client would have
# reflected, and call this out inline; this is a real gap in what testServer()
# alone can prove for this app, not a bug in the app or in these tests.

setup_app_dir <- system.file("shiny-apps", "app", package = "eyeQuality")
if (!nzchar(setup_app_dir)) {
  stop("test-runSetupApp.R: could not locate inst/shiny-apps/app/ via system.file()")
}

# rel_home_segments: converts an absolute path under fs::path_home() into the
# root/path-segment list shinyFiles::shinyDirChoose()/shinyFileSave() selections
# use, so a real directory/save-target can be simulated via session$setInputs()
# without a live browser picker. Same technique as test-runAnalyzeApp.R.
rel_home_segments <- function(path) {
  home <- normalizePath(fs::path_home(), winslash = "/")
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  testthat::skip_if_not(
    startsWith(path, home),
    "test tempdir is not under fs::path_home(); shinyFiles root-relative simulation would not resolve"
  )
  rel <- sub(paste0("^", home, "/?"), "", path)
  as.list(strsplit(rel, "/")[[1]])
}

test_that("Setup app save round-trip: form values survive write_batch_config()/read_batch_config() field-by-field", {
  skip_on_cran()

  data_dir <- tempfile("p904_save_data_")
  dir.create(data_dir, recursive = TRUE)
  on.exit(unlink(data_dir, recursive = TRUE), add = TRUE)
  data_segments <- rel_home_segments(data_dir)

  save_dir <- tempfile("p904_save_dest_")
  dir.create(save_dir, recursive = TRUE)
  on.exit(unlink(save_dir, recursive = TRUE), add = TRUE)
  save_segments <- rel_home_segments(save_dir)
  save_path <- file.path(normalizePath(save_dir, winslash = "/"), "roundtrip.yaml")

  shiny::testServer(setup_app_dir, {
    session$setInputs(directory = list(root = "Home", path = data_segments))
    session$setInputs(
      layout = "glob",
      pathPattern = "**/*.tsv",
      excludePattern_regex = "derivatives",
      modalityPattern_regex = "gaze",
      displayDimensionX_mm = 601,
      displayDimensionY_mm = 401,
      eyeSelection_method = "Strict",
      validityThreshold = 0.65,
      outputDir = "",
      batchName = "p904_save_roundtrip"
    )

    session$setInputs(save_config = list(
      root = "Home", path = save_segments, name = "roundtrip.yaml", type = "yaml"
    ))

    status <- config_io_status()
    expect_true(status$ok)
  })

  expect_true(file.exists(save_path))
  loaded <- read_batch_config(save_path)
  expect_equal(loaded$batchName, "p904_save_roundtrip")
  expect_equal(loaded$layout, "glob")
  expect_equal(loaded$pathPattern, "**/*.tsv")
  expect_equal(loaded$excludePattern_regex, "derivatives")
  expect_equal(loaded$modalityPattern_regex, "gaze")
  expect_equal(loaded$displayDimensionX_mm, 601)
  expect_equal(loaded$displayDimensionY_mm, 401)
  expect_equal(loaded$eyeSelection_method, "Strict")
  expect_equal(loaded$validityThreshold, 0.65)
  expect_null(loaded$outputDir)
  # numberCores has no form control at all; a fresh save (no config loaded
  # this session) must still record the real value "Start batch run" would
  # use, not NULL/NA -- regression guard for DEFAULT_GUI_NUMBER_CORES wiring.
  expect_equal(loaded$numberCores, 2)
  expect_null(loaded$adapterType)
})

test_that("Setup app load-then-resave: adapterType/numberCores survive via loaded_config_extra even with no form control for either", {
  skip_on_cran()

  data_dir <- tempfile("p904_loadresave_data_")
  dir.create(data_dir, recursive = TRUE)
  on.exit(unlink(data_dir, recursive = TRUE), add = TRUE)

  loaded_cfg_path <- tempfile("p904_loadresave_cfg_", fileext = ".yaml")
  on.exit(unlink(loaded_cfg_path), add = TRUE)
  write_batch_config(
    list(
      batchName = "loaded_before_resave",
      directoryBIDS = normalizePath(data_dir, winslash = "/"),
      layout = "bids",
      displayDimensionX_mm = 555,
      displayDimensionY_mm = 355,
      adapterType = "TobiiStudio",
      numberCores = 7
    ),
    loaded_cfg_path
  )

  save_dir <- tempfile("p904_loadresave_dest_")
  dir.create(save_dir, recursive = TRUE)
  on.exit(unlink(save_dir, recursive = TRUE), add = TRUE)
  save_segments <- rel_home_segments(save_dir)
  save_path <- file.path(normalizePath(save_dir, winslash = "/"), "resaved.yaml")

  shiny::testServer(setup_app_dir, {
    session$setInputs(load_config_file = data.frame(
      name = "loaded_before_resave.yaml",
      datapath = loaded_cfg_path,
      stringsAsFactors = FALSE
    ))

    load_status <- config_io_status()
    expect_true(load_status$ok)

    extra <- loaded_config_extra()
    expect_equal(extra$adapterType, "TobiiStudio")
    expect_equal(extra$numberCores, 7)

    # See the file-level comment above: update*Input() calls the load handler
    # just made are no-ops in this harness, so input$batchName/displayDimension*_mm/
    # layout/eyeSelection_method never actually changed here the way a real
    # browser would have reflected them. Set them explicitly to what the
    # config just loaded, standing in for that reflection, so the resave below
    # can succeed (these fields ARE form-owned and validate_batch_config()
    # requires batchName/displayDimension*_mm) -- this does not touch
    # adapterType/numberCores, which is the actual behavior under test.
    session$setInputs(
      layout = "bids",
      batchName = "loaded_before_resave",
      displayDimensionX_mm = 555,
      displayDimensionY_mm = 355,
      eyeSelection_method = "Maximize"
    )

    session$setInputs(save_config = list(
      root = "Home", path = save_segments, name = "resaved.yaml", type = "yaml"
    ))
    save_status <- config_io_status()
    expect_true(save_status$ok)
  })

  expect_true(file.exists(save_path))
  resaved <- read_batch_config(save_path)
  expect_equal(resaved$adapterType, "TobiiStudio")
  expect_equal(resaved$numberCores, 7)
})

test_that("Setup app invalid config upload: validate_batch_config()'s error surfaces and zero update*Input() calls happen (form never partially applied)", {
  skip_on_cran()

  # missing batchName/directoryBIDS/displayDimension*_mm -- validate_batch_config()
  # rejects this outright.
  invalid_cfg_path <- tempfile("p904_invalid_cfg_", fileext = ".yaml")
  on.exit(unlink(invalid_cfg_path), add = TRUE)
  yaml::write_yaml(list(schemaVersion = 1, layout = "glob"), invalid_cfg_path)

  shiny::testServer(setup_app_dir, {
    # P10-12: prime the session's first reactive flush BEFORE installing the
    # capture below. The merged app's Analyze tabs carry their own
    # unconditional per-file notes-textarea sync observer (qcflags_note_text,
    # analyze_helpers.R/app.R), which runs once on ANY session's very first
    # flush -- regardless of what input triggered it -- since it has no
    # explicit event binding. Confirmed directly: even setting an unrelated
    # input (e.g. batchName) with no config load at all pushes exactly one
    # qcflags_note_text update the first time. That's real, but has nothing
    # to do with the load handler under test here, so it's burned off here
    # rather than either weakening this assertion or misreporting an
    # unrelated tab's own startup behavior as "the config load handler
    # partially applied the form."
    session$flushReact()

    pushed_input_ids <- character(0)
    real_send <- session$sendInputMessage
    session$sendInputMessage <- function(inputId, message) {
      pushed_input_ids <<- c(pushed_input_ids, inputId)
      real_send(inputId, message)
    }

    session$setInputs(load_config_file = data.frame(
      name = "invalid.yaml",
      datapath = invalid_cfg_path,
      stringsAsFactors = FALSE
    ))

    status <- config_io_status()
    expect_false(status$ok)
    expect_match(status$message, "batchName", fixed = TRUE)
    expect_match(status$message, "directoryBIDS", fixed = TRUE)

    # No update*Input() call for ANY field -- a partially-applied form (e.g.
    # layout pushed through but batchName not) would be worse than none at
    # all, since it would look successful.
    expect_length(pushed_input_ids, 0)

    expect_null(loaded_config_extra())
  })
})

# ---------------------------------------------------------------------------
# P10-12: reconciled save/load -- one control now carries both Setup's own
# run parameters AND the Analyze tabs' live qc_threshold_* values
# ---------------------------------------------------------------------------
#
# Before the merge, "Save config"/"Load config" was two independent flows:
# this one (P9-04, Setup form fields only) and a second, QC-thresholds-only
# one on the standalone Analyze app's own sidebar (P10-07). P10-12 merged
# both into this ONE control: build_current_config() (app.R) overlays the
# Analyze tabs' current qc_thresholds() onto build_batch_config_from_form()'s
# output before every save, and the load handler now also pushes
# update*Input() calls for the Analyze tabs' own qc_threshold_* numericInputs,
# not just Setup's fields -- and does so with validate = TRUE for the WHOLE
# config, a deliberate behavior change from the old standalone Analyze app's
# validate = FALSE (which let a thresholds-only file partially load). See the
# last test in this section for that specific behavior change.
#
# capturing_update_session(): duplicated locally (rather than shared with
# test-runAnalyzeApp.R) per this file's own self-containedness convention --
# same technique as that file's own capturing_update_session(), records every
# update*Input() call's inputId/message rather than relying on
# MockShinySession's sendInputMessage() no-op reflecting into input$... (see
# this file's P9-04 header comment for the full explanation of that
# limitation). update*Input()'s own message$value is always sent as a plain
# character string (shiny serializes it for the client-side JS regardless of
# the input's own R-side numeric type) -- callers compare against a string,
# or wrap in as.numeric(), not a bare numeric literal.
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

# prime_and_clear: burns off the merged app's unconditional first-flush
# side effect (see the "invalid config upload" test above for the full
# explanation -- the Analyze tabs' own notes-textarea sync observer pushes
# exactly one qcflags_note_text update on ANY session's very first reactive
# flush, unrelated to whatever load/save flow a test actually cares about)
# before a capturing_update_session()-backed test starts asserting on
# `calls`. Must run INSIDE the testServer() expr (a plain session$flushReact()
# call from outside it does not have access to the server's own reactive
# graph).
prime_and_clear <- function(session, calls) {
  session$flushReact()
  rm(list = ls(calls), envir = calls)
}

test_that("Setup tab's reconciled save writes the Analyze tabs' live qc_threshold_* values into qcThresholds, not just Setup's own fields", {
  skip_on_cran()

  data_dir <- tempfile("p1012_save_thresh_data_")
  dir.create(data_dir, recursive = TRUE)
  on.exit(unlink(data_dir, recursive = TRUE), add = TRUE)
  data_segments <- rel_home_segments(data_dir)

  save_dir <- tempfile("p1012_save_thresh_dest_")
  dir.create(save_dir, recursive = TRUE)
  on.exit(unlink(save_dir, recursive = TRUE), add = TRUE)
  save_segments <- rel_home_segments(save_dir)
  save_path <- file.path(normalizePath(save_dir, winslash = "/"), "with_thresholds.yaml")

  shiny::testServer(setup_app_dir, {
    # directoryBIDS is a required field (validate_batch_config()) -- a real
    # picker selection is needed for "Save config" to succeed at all, same as
    # every other save test in this file.
    session$setInputs(directory = list(root = "Home", path = data_segments))
    session$setInputs(
      layout = "bids",
      batchName = "p1012_thresh_save",
      displayDimensionX_mm = 594,
      displayDimensionY_mm = 344,
      # MockShinySession does not reflect selectInput()'s own UI default
      # ("Maximize") the way a real browser session would -- left unset,
      # input$eyeSelection_method is NULL/NA and validate_batch_config()
      # rejects the save outright, same as every other save test in this
      # file that touches eyeSelection_method explicitly.
      eyeSelection_method = "Maximize",
      # These live on the Analyze tabs' own sidebar, not the Setup form --
      # setting them here directly is exactly what a real browser session
      # would already have done before the user switches to "Setup & Run"
      # and clicks "Save config...", since both tab groups share one input$.
      qc_threshold_valid_pct = 65,
      qc_threshold_robust_pct = 70,
      qc_threshold_interp_pct = 15
    )

    session$setInputs(save_config = list(
      root = "Home", path = save_segments, name = "with_thresholds.yaml", type = "yaml"
    ))

    status <- config_io_status()
    expect_true(status$ok)
  })

  expect_true(file.exists(save_path))
  loaded <- read_batch_config(save_path)
  expect_equal(loaded$batchName, "p1012_thresh_save")
  expect_equal(loaded$qcThresholds$valid_pct, 65)
  expect_equal(loaded$qcThresholds$robust_pct, 70)
  expect_equal(loaded$qcThresholds$interp_pct, 15)
})

test_that("Setup tab's reconciled save records each threshold's documented default when its own numericInput has never been touched this session", {
  skip_on_cran()

  data_dir <- tempfile("p1012_save_thresh_default_data_")
  dir.create(data_dir, recursive = TRUE)
  on.exit(unlink(data_dir, recursive = TRUE), add = TRUE)
  data_segments <- rel_home_segments(data_dir)

  save_dir <- tempfile("p1012_save_thresh_default_dest_")
  dir.create(save_dir, recursive = TRUE)
  on.exit(unlink(save_dir, recursive = TRUE), add = TRUE)
  save_segments <- rel_home_segments(save_dir)
  save_path <- file.path(normalizePath(save_dir, winslash = "/"), "default_thresholds.yaml")

  shiny::testServer(setup_app_dir, {
    session$setInputs(directory = list(root = "Home", path = data_segments))
    session$setInputs(
      layout = "bids",
      batchName = "p1012_thresh_default",
      displayDimensionX_mm = 594,
      displayDimensionY_mm = 344,
      eyeSelection_method = "Maximize"
    )
    # deliberately never touch qc_threshold_valid_pct/robust_pct/interp_pct

    session$setInputs(save_config = list(
      root = "Home", path = save_segments, name = "default_thresholds.yaml", type = "yaml"
    ))
    expect_true(config_io_status()$ok)
  })

  loaded <- read_batch_config(save_path)
  expect_equal(loaded$qcThresholds$valid_pct, 80)
  expect_equal(loaded$qcThresholds$robust_pct, 80)
  expect_equal(loaded$qcThresholds$interp_pct, 20)
})

test_that("Setup tab's reconciled load populates both Setup's own fields and the Analyze tabs' qc_threshold_* numericInputs from one file", {
  skip_on_cran()

  data_dir <- tempfile("p1012_load_both_data_")
  dir.create(data_dir, recursive = TRUE)
  on.exit(unlink(data_dir, recursive = TRUE), add = TRUE)

  cfg_path <- tempfile("p1012_load_both_cfg_", fileext = ".yaml")
  on.exit(unlink(cfg_path), add = TRUE)
  write_batch_config(
    list(
      batchName = "p1012_load_both",
      directoryBIDS = normalizePath(data_dir, winslash = "/"),
      layout = "bids",
      displayDimensionX_mm = 500,
      displayDimensionY_mm = 300,
      qcThresholds = list(valid_pct = 55, robust_pct = 60, interp_pct = 30)
    ),
    cfg_path
  )

  cap <- capturing_update_session()
  shiny::testServer(setup_app_dir, {
    prime_and_clear(session, cap$calls)

    session$setInputs(load_config_file = data.frame(
      name = "p1012_load_both.yaml",
      datapath = cfg_path,
      stringsAsFactors = FALSE
    ))

    status <- config_io_status()
    expect_true(status$ok)

    # Setup's own fields...
    expect_equal(cap$calls[["batchName"]][[1]]$value, "p1012_load_both")
    expect_equal(as.numeric(cap$calls[["displayDimensionX_mm"]][[1]]$value), 500)
    expect_equal(as.numeric(cap$calls[["displayDimensionY_mm"]][[1]]$value), 300)
    # ...AND the Analyze tabs' own qc_threshold_* numericInputs, from the
    # exact same file/load click -- the actual reconciliation under test.
    expect_equal(as.numeric(cap$calls[["qc_threshold_valid_pct"]][[1]]$value), 55)
    expect_equal(as.numeric(cap$calls[["qc_threshold_robust_pct"]][[1]]$value), 60)
    expect_equal(as.numeric(cap$calls[["qc_threshold_interp_pct"]][[1]]$value), 30)
  }, session = cap$session)
})

test_that("Setup tab's reconciled load falls back to each threshold's documented default for a config with no qcThresholds section at all", {
  skip_on_cran()

  data_dir <- tempfile("p1012_load_noqc_data_")
  dir.create(data_dir, recursive = TRUE)
  on.exit(unlink(data_dir, recursive = TRUE), add = TRUE)

  cfg_path <- tempfile("p1012_load_noqc_cfg_", fileext = ".yaml")
  on.exit(unlink(cfg_path), add = TRUE)
  write_batch_config(
    list(
      batchName = "p1012_load_noqc",
      directoryBIDS = normalizePath(data_dir, winslash = "/"),
      layout = "bids",
      displayDimensionX_mm = 500,
      displayDimensionY_mm = 300
    ),
    cfg_path
  )

  cap <- capturing_update_session()
  shiny::testServer(setup_app_dir, {
    prime_and_clear(session, cap$calls)

    session$setInputs(load_config_file = data.frame(
      name = "p1012_load_noqc.yaml",
      datapath = cfg_path,
      stringsAsFactors = FALSE
    ))

    expect_true(config_io_status()$ok)
    # every threshold_id's numericInput is still explicitly set (to its
    # documented default), not left alone -- "load" fully determines the
    # resulting threshold state, per build_current_config()'s own comment.
    expect_equal(as.numeric(cap$calls[["qc_threshold_valid_pct"]][[1]]$value), 80)
    expect_equal(as.numeric(cap$calls[["qc_threshold_robust_pct"]][[1]]$value), 80)
    expect_equal(as.numeric(cap$calls[["qc_threshold_interp_pct"]][[1]]$value), 20)
  }, session = cap$session)
})

test_that("Setup tab's reconciled load rejects an incomplete config outright, rather than partially applying just its qcThresholds (deliberate P10-12 behavior change)", {
  # Before P10-12, the standalone Analyze app's own load handler used
  # validate = FALSE specifically so a QC-thresholds-only config (missing
  # directoryBIDS/batchName/displayDimension*_mm entirely) could still load
  # its thresholds. The ONE reconciled handler now uses validate = TRUE for
  # everything, including thresholds -- this same hand-authored file, which
  # WOULD have partially loaded its qcThresholds under the old standalone
  # Analyze app, must now be rejected in full: config_io_status()$ok == FALSE
  # and ZERO update*Input() calls for anything, including qc_threshold_*.
  skip_on_cran()

  # Missing batchName/directoryBIDS/displayDimension*_mm entirely -- only a
  # qcThresholds section, exactly the shape the old standalone Analyze app's
  # own "thresholds only" save would have produced.
  thresholds_only_path <- tempfile("p1012_thresholds_only_", fileext = ".yaml")
  on.exit(unlink(thresholds_only_path), add = TRUE)
  yaml::write_yaml(
    list(schemaVersion = 1, qcThresholds = list(valid_pct = 66, robust_pct = 71, interp_pct = 18)),
    thresholds_only_path
  )

  cap <- capturing_update_session()
  shiny::testServer(setup_app_dir, {
    prime_and_clear(session, cap$calls)

    session$setInputs(load_config_file = data.frame(
      name = "thresholds_only.yaml",
      datapath = thresholds_only_path,
      stringsAsFactors = FALSE
    ))

    status <- config_io_status()
    expect_false(status$ok)
    expect_match(status$message, "batchName", fixed = TRUE)
    expect_match(status$message, "directoryBIDS", fixed = TRUE)

    expect_null(loaded_config_extra())
    # No update*Input() call for ANY field -- specifically including the
    # qc_threshold_* ones, which is the actual regression this guards
    # against: an earlier reconciliation could plausibly have validated
    # Setup's own fields first and only then bailed, while still having
    # already pushed qc_threshold_* updates from a "kept" qcThresholds
    # section evaluated before that check. Zero calls of ANY kind here rules
    # that out.
    expect_length(ls(cap$calls), 0)
  }, session = cap$session)
})

# ---------------------------------------------------------------------------
# P10-12: the two directory pickers (Setup's `directory` vs the Analyze
# tabs' `analyze_directory`) operate independently, with no cross-talk
# ---------------------------------------------------------------------------
test_that("the Setup tab's directory field and the Analyze tabs' analyze_directory field resolve independently with no cross-talk", {
  skip_on_cran()

  setup_dir <- tempfile("p1012_crosstalk_setup_")
  dir.create(file.path(setup_dir, "sub-01", "ses-01"), recursive = TRUE)
  file.create(file.path(setup_dir, "sub-01", "ses-01", "a.tsv"))
  on.exit(unlink(setup_dir, recursive = TRUE), add = TRUE)
  setup_segments <- rel_home_segments(setup_dir)

  analyze_dir <- tempfile("p1012_crosstalk_analyze_")
  dir.create(analyze_dir, recursive = TRUE)
  readr::write_tsv(
    data.frame(qc_metric = "valid_raw_data", percent = 1),
    file.path(analyze_dir, "sub-z_ses-1_desc-crosstalk_preproc_qcsummary.tsv")
  )
  on.exit(unlink(analyze_dir, recursive = TRUE), add = TRUE)
  analyze_segments <- rel_home_segments(analyze_dir)

  setup_dir_norm <- normalizePath(setup_dir, winslash = "/", mustWork = TRUE)
  analyze_dir_norm <- normalizePath(analyze_dir, winslash = "/", mustWork = TRUE)

  shiny::testServer(setup_app_dir, {
    session$setInputs(directory = list(root = "Home", path = setup_segments))
    session$setInputs(analyze_directory = list(root = "Home", path = analyze_segments))

    expect_equal(normalizePath(selected_dir(), winslash = "/"), setup_dir_norm)
    expect_equal(normalizePath(analyze_selected_dir(), winslash = "/"), analyze_dir_norm)
    expect_false(identical(selected_dir(), analyze_selected_dir()))

    # A save's directoryBIDS reflects Setup's own picker, never the Analyze
    # tabs' directory -- build_current_config() must not have picked up the
    # wrong one.
    config <- build_current_config()
    expect_equal(as.character(config$directoryBIDS), setup_dir_norm)

    # Loading the Analyze tabs' own qcsummary files must use analyze_directory,
    # never Setup's directory -- and must not disturb Setup's own selection.
    session$setInputs(analyze_recursiveSearch = FALSE)
    session$setInputs(load = 1)
    expect_equal(current_load_result()$n_files, 1L)
    expect_equal(normalizePath(selected_dir(), winslash = "/"), setup_dir_norm)
  })
})

# ---------------------------------------------------------------------------
# P9-04: "Start batch run" now forwards the same layout/pattern/processing
# parameters the dry-run preview above it was already built against
# ---------------------------------------------------------------------------
#
# Before this task, the launch handler's start_background_batch() call only
# ever passed directoryBIDS/batchName/numberCores -- layout, subjectPattern_regex/
# sessionPattern_regex/pathPattern/excludePattern_regex/recursiveSearch, and
# every processing-parameter field added alongside "Save config" were silently
# dropped, so a real run always used eyeQualityBatch()'s own bids-layout
# defaults regardless of what the preview above had actually matched against.
# This is verified end to end here (not mocked) -- start_background_batch()
# isn't a plain function reachable from a testServer() expr block (it's
# sourced local to app.R's own top-level environment, which
# shiny::testServer()'s expr-clone mechanism does not expose for
# reassignment -- confirmed while writing this test), and P9-05/06 already
# deliberately chose not to build a from-scratch async mocking harness for
# this exact call site (see test-background_run.R's file-level comment). A
# real (but fast: numberCores = 2, 2 tiny fixture files, ~few seconds) launch
# through the app is used instead, distinguishing glob-layout with a
# subject-restricting pathPattern (matches 2 of 4 fixture files) from what
# bids-layout's own defaults would have matched (all 4) -- if layout/pathPattern
# were dropped again, this run would silently process all 4 files instead of 2.
test_that("Setup app launch actually threads the current layout/pathPattern into the real batch run (not silently defaulting to bids layout)", {
  skip_on_cran()
  skip_if_not_installed("later") # used below to pump the event loop while the real future/promises launch resolves

  src <- testthat::test_path("fixtures", "bids")
  dest <- tempfile("p904_launch_")
  fs::dir_copy(src, dest)
  on.exit(unlink(dest, recursive = TRUE), add = TRUE)
  dest <- normalizePath(dest, winslash = "/", mustWork = TRUE)
  data_segments <- rel_home_segments(dest)

  shiny::testServer(setup_app_dir, {
    session$setInputs(directory = list(root = "Home", path = data_segments))
    session$setInputs(
      layout = "glob",
      pathPattern = "sub-1/**/*.tsv", # matches only sub-1's 2 files, not all 4
      excludePattern_regex = "",
      modalityPattern_regex = "",
      displayDimensionX_mm = 594,
      displayDimensionY_mm = 344,
      eyeSelection_method = "Maximize",
      validityThreshold = NA,
      outputDir = "",
      batchName = "p904_launch_e2e"
    )
    session$setInputs(preview = 1)
    expect_equal(preview_result()$matched_count, 2)

    session$setInputs(launch = 1)

    # future/promises resolution needs the later event loop pumped manually
    # in a script/test context (no running Shiny app to do it automatically);
    # poll with a generous timeout since this spawns real worker processes.
    deadline <- Sys.time() + 60
    repeat {
      later::run_now(timeoutSecs = 1)
      if (!identical(progress_state$status, "running")) break
      if (Sys.time() > deadline) break
    }
    expect_equal(progress_state$status, "done")
    expect_equal(progress_state$n_done, 2)
  })

  qc_files <- list.files(dest, pattern = "qcsummary\\.tsv$", recursive = TRUE, full.names = TRUE)
  # The regression this guards against: if layout/pathPattern were dropped
  # from the launch call again, eyeQualityBatch() would fall back to its own
  # bids-layout defaults and process all 4 fixture files, not just sub-1's 2.
  expect_length(qc_files, 2)
  expect_true(all(grepl("sub-1_", basename(qc_files))))
})

# ---------------------------------------------------------------------------
# P9-07: post-run summary linking to the Analyze app
# ---------------------------------------------------------------------------
#
# resolve_analyze_directory()/build_analyze_launch_command() are Shiny-free
# helpers (helpers.R) tested directly first, then the Setup app's own
# output$post_run_link panel (app.R) via shiny::testServer(). progress_state
# and run_info are reactiveValues objects defined inside server() -- directly
# readable/writable from within a testServer() expr block, the same way
# config_io_status()/loaded_config_extra() are read directly elsewhere in this
# file -- so these tests drive the panel straight to each status it cares
# about rather than needing a real background batch run just to reach "done".

test_that("resolve_analyze_directory returns outputDir when it is set", {
  expect_equal(resolve_analyze_directory("/out/dir", "/data/bids"), "/out/dir")
})

test_that("resolve_analyze_directory falls back to directoryBIDS when outputDir is NULL, matching eyeQualityBatch()'s own outputDir = NULL nested-derivatives convention", {
  # eyeQualityBatch()'s own docs (R/eyeQualityBatch.R): outputDir = NULL (the
  # default) writes each file's output into that file's own nested
  # derivatives/eyeQuality-v1/ subfolder underneath directoryBIDS, rather than
  # one central location -- so directoryBIDS itself (not some other path) is
  # the only correct fallback for "where did this run's outputs land."
  expect_equal(resolve_analyze_directory(NULL, "/data/bids"), "/data/bids")
})

test_that("resolve_analyze_directory falls back to directoryBIDS when outputDir is a blank or whitespace-only string", {
  # blank_to_null() treats an empty/whitespace-only textInput value the same
  # as NULL -- this is the actual shape input$outputDir takes when a user
  # leaves the Setup app's "Output directory" field untouched.
  expect_equal(resolve_analyze_directory("", "/data/bids"), "/data/bids")
  expect_equal(resolve_analyze_directory("   ", "/data/bids"), "/data/bids")
})

test_that("build_analyze_launch_command produces a syntactically valid, parseable eyeQuality::runAnalyzeApp() call pointed at the given directory", {
  cmd <- build_analyze_launch_command("/data/study/outputs")

  parsed <- str2lang(cmd)
  expect_equal(parsed[[1]], quote(eyeQuality::runAnalyzeApp))
  # eval()-ing just the unevaluated initialDirectory argument (a plain string
  # constant) is safe here -- it never calls runAnalyzeApp() itself.
  expect_equal(eval(parsed$initialDirectory), "/data/study/outputs")
})

test_that("build_analyze_launch_command escapes a Windows-style backslash path so the generated call round-trips through R's own parser back to the exact original path", {
  # The real regression this guards against: a raw (unescaped) backslash path
  # pasted into build_analyze_launch_command()'s sprintf() template would
  # either fail to parse at all (a lone backslash before certain characters)
  # or parse into a DIFFERENT string than the original directory (R's parser
  # treats "\d", "\U" etc. as escape sequences) -- silently handing the user a
  # command that opens the wrong directory, or none at all. Parsing the
  # generated command string with R's own parser and evaluating the resulting
  # string literal is what actually proves the escaping is correct, not just
  # that backslashes were doubled somewhere in the printed text.
  windows_path <- "C:\\Users\\test\\study data"
  cmd <- build_analyze_launch_command(windows_path)

  parsed <- str2lang(cmd)
  expect_equal(eval(parsed$initialDirectory), windows_path)
})

test_that("build_analyze_launch_command escapes an embedded double quote so the generated call still parses as valid R code", {
  tricky_path <- 'C:\\data\\"quoted"\\dir'
  cmd <- build_analyze_launch_command(tricky_path)

  parsed <- str2lang(cmd)
  expect_equal(eval(parsed$initialDirectory), tricky_path)
})

test_that("Setup app's post_run_link panel is hidden before any run, while a run is in progress, and after a coarse run-level failure", {
  skip_on_cran()

  shiny::testServer(setup_app_dir, {
    # "not started" is progress_state's own initial value -- no mutation needed
    expect_null(session$getOutput("post_run_link"))

    # run_info$directory/batchName must be set to something real before
    # flipping progress_state$status to "running": app.R's own live-polling
    # observe() (P9-05/06) is unconditionally armed and fires
    # poll_batch_progress(run_info$directory, run_info$batchName, ...) the
    # moment status == "running", regardless of which output this test cares
    # about. With both left NULL, poll_batch_progress()'s
    # file.path(NULL, ...) collapses to character(0), and
    # if (file.exists(character(0))) errors with "argument is of length
    # zero" inside that observer -- an uncaught reactive error that tears
    # down the whole mock session, breaking every assertion after it (not a
    # hypothetical: this is exactly what happened while writing this test).
    run_info$directory <- tempdir()
    run_info$batchName <- "p907_running_dummy"

    # Direct reactiveValues mutation (progress_state$status <- ...), unlike
    # session$setInputs(), does not itself trigger a reactive flush in this
    # harness -- session$getOutput() only re-flushes automatically when an
    # output's cached promise is still NULL (i.e. never evaluated), so an
    # explicit session$flushReact() is needed here for each mutation to
    # actually be picked up before the next getOutput() call, rather than
    # silently re-returning the previous (also-NULL) cached value below.
    progress_state$status <- "running"
    session$flushReact()
    expect_null(session$getOutput("post_run_link"))

    progress_state$status <- "failed"
    progress_state$message <- "a coarse validation error"
    session$flushReact()
    expect_null(session$getOutput("post_run_link"))
  })
})

test_that("the live-polling observe() counts completed files under run_info$outputDir, not just directoryBIDS, when a custom output directory was used for the run", {
  # Regression test: app.R's poll_batch_progress() call sites (both the
  # 2-second live-polling observe() and the final count read once the
  # promise resolves) previously omitted outputDir entirely, so
  # count_completed_qcsummary_files() always searched under directoryBIDS --
  # silently reporting n_done = 0 for the whole run (and "0 of N processed"
  # at completion) whenever a custom outputDir was set, even though every
  # file completed and wrote its qcsummary.tsv correctly to the real
  # outputDir location. run_info$outputDir is what carries input$outputDir's
  # value into that observe(), set once at launch time (observeEvent(input$launch)).
  skip_on_cran()

  directory_bids <- tempfile("p907_bids_")
  dir.create(directory_bids, recursive = TRUE)
  on.exit(unlink(directory_bids, recursive = TRUE), add = TRUE)
  output_dir <- tempfile("p907_out_")
  dir.create(output_dir, recursive = TRUE)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)
  # Sits only under output_dir, never under directory_bids -- pins down that
  # the observe() is searching the right root, not just falling back to one
  # that happens to also work.
  writeLines("", file.path(output_dir, "sub-01_desc-p907_preproc_qcsummary.tsv"))

  shiny::testServer(setup_app_dir, {
    run_info$directory <- directory_bids
    run_info$batchName <- "p907"
    run_info$outputDir <- output_dir
    run_info$n_expected <- 1L

    progress_state$status <- "running"
    session$flushReact()

    expect_equal(progress_state$n_done, 1L)
  })
})

test_that("Setup tab's post_run_link panel shows the outputDir-resolved directory and a 'Review results in Analyze tabs' button once a run reaches status == 'done'", {
  # P10-12: before the merge, this panel rendered a copy-pasteable
  # eyeQuality::runAnalyzeApp() command for a SEPARATE R console/session --
  # now that Setup & Run and Analyze / QC Explorer are tabs of one process,
  # it instead renders the resolved directory plus a real
  # actionButton("review_in_analyze", ...) that does the hand-off in-process
  # (see the "input$review_in_analyze" tests below). This test only pins down
  # what's rendered; the button's actual behavior is exercised separately.
  skip_on_cran()

  shiny::testServer(setup_app_dir, {
    session$setInputs(outputDir = "/central/outputs")
    run_info$directory <- "/data/study_a"
    progress_state$status <- "done"
    session$flushReact()

    html <- paste(as.character(session$getOutput("post_run_link")), collapse = "\n")
    expect_true(grepl("/central/outputs", html, fixed = TRUE))
    expect_true(grepl('id="review_in_analyze"', html, fixed = TRUE))
    expect_true(grepl("Review results in Analyze tabs", html, fixed = TRUE))
    # outputDir was set, so directoryBIDS (run_info$directory) must not be
    # what's shown -- pins down which of the two resolve_analyze_directory()
    # actually chose, not just that something plausible-looking was rendered.
    expect_false(grepl("/data/study_a", html, fixed = TRUE))
  })
})

test_that("Setup tab's post_run_link panel falls back to directoryBIDS when outputDir was left blank for the run", {
  skip_on_cran()

  shiny::testServer(setup_app_dir, {
    session$setInputs(outputDir = "")
    run_info$directory <- "/data/study_b"
    progress_state$status <- "done"
    session$flushReact()

    html <- paste(as.character(session$getOutput("post_run_link")), collapse = "\n")
    expect_true(grepl("/data/study_b", html, fixed = TRUE))
    expect_true(grepl('id="review_in_analyze"', html, fixed = TRUE))
  })
})

# ---------------------------------------------------------------------------
# P10-12: input$review_in_analyze -- the real in-process Setup -> Analyze
# hand-off
# ---------------------------------------------------------------------------
#
# Before P10-12 this was necessarily a copy-pasteable command for a second R
# session (see build_analyze_launch_command() above, still independently
# tested and still a real, supported thing to hand someone opening the
# Analyze tabs separately from a script-driven run -- just no longer what
# app.R itself renders). Now that both tab groups share one process, clicking
# "Review results in Analyze tabs" instead calls do_analyze_load() directly
# (the same function body a manual "Load qcsummary files" click on the
# Analyze tabs uses) and switches the visible tab.
#
# session$setInputs(top_nav = ...)/asserting input$top_nav changed doesn't
# work in this harness: MockShinySession$sendInputMessage() -- what
# updateNavbarPage() ultimately calls -- is a documented no-op (see this
# file's own P9-04 header comment on the same limitation for update*Input()
# calls). These tests instead assert the hand-off succeeded via the same
# reactive state a real tab switch would have made visible: has_loaded_once(),
# current_load_result()$n_files, and analyze_directory_override() -- the
# reactiveVal do_analyze_load() itself writes into, read directly from within
# the shared testServer() expr just like config_io_status()/loaded_config_extra()
# are read elsewhere in this file.
test_that("clicking 'Review results in Analyze tabs' loads the resolved output directory into the Analyze tabs' own reactive state", {
  skip_on_cran()

  out_dir <- tempfile("p1012_handoff_out_")
  dir.create(out_dir, recursive = TRUE)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  readr::write_tsv(
    data.frame(qc_metric = c("a", "b"), n = c(1, 2)),
    file.path(out_dir, "sub-a_ses-1_desc-p1012handoff_preproc_qcsummary.tsv")
  )
  out_dir <- normalizePath(out_dir, winslash = "/", mustWork = TRUE)

  shiny::testServer(setup_app_dir, {
    session$setInputs(outputDir = out_dir)
    run_info$directory <- "/data/never_used_since_outputDir_is_set"
    progress_state$status <- "done"
    session$flushReact()

    # Before the click: the Analyze tabs have never loaded anything.
    expect_false(has_loaded_once())
    expect_null(current_load_result())

    session$setInputs(review_in_analyze = 1)

    expect_true(has_loaded_once())
    expect_equal(current_load_result()$n_files, 1L)
    expect_equal(normalizePath(analyze_directory_override(), winslash = "/"), out_dir)
  })
})

test_that("clicking 'Review results in Analyze tabs' resolves via directoryBIDS, matching resolve_analyze_directory()'s own fallback, when outputDir was left blank", {
  skip_on_cran()

  data_dir <- tempfile("p1012_handoff_bids_")
  dir.create(data_dir, recursive = TRUE)
  on.exit(unlink(data_dir, recursive = TRUE), add = TRUE)
  readr::write_tsv(
    data.frame(qc_metric = "valid_raw_data", percent = 0.9),
    file.path(data_dir, "sub-b_ses-1_desc-p1012handoffbids_preproc_qcsummary.tsv")
  )
  data_dir <- normalizePath(data_dir, winslash = "/", mustWork = TRUE)

  shiny::testServer(setup_app_dir, {
    session$setInputs(outputDir = "")
    run_info$directory <- data_dir
    progress_state$status <- "done"
    session$flushReact()

    session$setInputs(review_in_analyze = 1)

    expect_true(has_loaded_once())
    expect_equal(current_load_result()$n_files, 1L)
    expect_equal(normalizePath(analyze_directory_override(), winslash = "/"), data_dir)
  })
})

test_that("input$review_in_analyze does nothing (no crash, no load) if clicked before resolve_analyze_directory() would have anything real to resolve", {
  # req(analyze_dir, nzchar(analyze_dir)) guards the observeEvent body --
  # run_info$directory is NULL until a run has actually launched, so
  # resolve_analyze_directory(NULL, NULL) falls through to a NULL
  # directoryBIDS. This mostly guards against a future regression where that
  # guard is accidentally dropped and do_analyze_load(NULL, ...) is called.
  skip_on_cran()

  shiny::testServer(setup_app_dir, {
    session$setInputs(review_in_analyze = 1)

    expect_false(has_loaded_once())
    expect_null(current_load_result())
  })
})

# ---------------------------------------------------------------------------
# P10-12: runSetupApp() resolves the merged app directory and sets
# app_initialTab before shiny::runApp()
# ---------------------------------------------------------------------------
#
# runSetupApp() itself calls shiny::runApp(), which blocks until the app is
# closed -- not something a unit test can call for real.
# testthat::local_mocked_bindings(..., .package = "shiny") stands in for it,
# same technique test-runAnalyzeApp.R already uses for runAnalyzeApp()'s own
# equivalent tests.

test_that("runSetupApp() sets shinyOptions(app_initialTab = 'Setup & Run') before shiny::runApp() would be reached", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("shinyFiles")
  skip_if_not_installed("DT")

  testthat::local_mocked_bindings(runApp = function(...) invisible(NULL), .package = "shiny")
  on.exit(shiny::shinyOptions(app_initialTab = NULL), add = TRUE)

  eyeQuality::runSetupApp()

  expect_equal(shiny::getShinyOption("app_initialTab", NULL), "Setup & Run")
})

test_that("runSetupApp() resolves the SAME merged app directory runAnalyzeApp() does (system.file('shiny-apps', 'app', ...)), not a separate 'setup' directory", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("shinyFiles")
  skip_if_not_installed("DT")

  captured_app_dir <- NULL
  testthat::local_mocked_bindings(
    runApp = function(appDir, ...) {
      captured_app_dir <<- appDir
      invisible(NULL)
    },
    .package = "shiny"
  )
  on.exit(shiny::shinyOptions(app_initialTab = NULL), add = TRUE)

  eyeQuality::runSetupApp()

  expect_equal(captured_app_dir, setup_app_dir)
  expect_true(file.exists(file.path(captured_app_dir, "app.R")))
})

# ---------------------------------------------------------------------------
# P9-08: expose P7-07's outputStructure/copyRawFile in the Setup tab UI
# (useBidsOutput/copyRawFile checkboxes)
# ---------------------------------------------------------------------------
#
# Coverage note on mechanism choice: this repo has no shinytest2 (or
# chromote/headless-browser) infrastructure at all -- confirmed directly
# (requireNamespace("shinytest2")/requireNamespace("chromote") both FALSE
# against the authoritative Windows R 4.2.1 install this suite runs
# against), and it isn't in DESCRIPTION's Suggests. Standing up that
# infrastructure (a new heavy Suggests dependency plus a downloaded headless
# Chrome binary) is a real scope expansion beyond this task's own test
# coverage, so it isn't added here without that being a deliberate call --
# flagging it explicitly rather than silently adding it.
#
# What's tested here instead, without a live browser:
#   - the actual conditionalPanel() wiring in the rendered `ui` object
#     (collect_display_if_chain() below) -- this is the real, single source
#     of truth for which condition string gates each checkbox's visibility,
#     and directly fails if that wrapping/condition/nesting is ever changed
#     or removed. What it does NOT prove is that shiny.js's own client-side
#     data-display-if handling actually toggles the DOM correctly at
#     runtime -- that's unmodified, long-standing shiny library behavior
#     already relied on by every other conditionalPanel in this same file
#     (layout bids/glob, qcflags_show_detail), not something this task's own
#     code could plausibly break without also breaking those.
#   - that nothing server-side (no observer keyed on input$outputDir)
#     programmatically resets useBidsOutput/copyRawFile when outputDir
#     toggles blank/non-blank -- the actual server-side half of the "hide,
#     don't disable" design decision (app.R's own comment on
#     useBidsOutput/copyRawFile), and the one part of flow 6 a
#     testServer()-level test genuinely can regress-guard.
#   - the full save/load/launch flows (3, 4, 5 in the originating task),
#     which are ordinary reactive-state-level testServer() coverage, no
#     different in kind from this file's existing P9-04/P10-12 tests above.

# collect_display_if_chain: walks a shiny.tag/shiny.tag.list structure
# looking for a tag whose `id` attribute equals `target_id` (checkboxInput()
# places `id` on the innermost <input> element, confirmed directly via
# as.character(checkboxInput(...))), accumulating every ancestor
# conditionalPanel()'s `data-display-if` condition string along the way (in
# outside-in order). Returns NULL if `target_id` isn't found anywhere in the
# tree, or a character vector of 1+ condition strings (outermost first) if it
# is -- character(0) would mean "found, but not inside any conditionalPanel",
# which doesn't happen for either checkbox under test here.
collect_display_if_chain <- function(tag, target_id, chain = character(0)) {
  if (is.null(tag)) {
    return(NULL)
  }
  if (inherits(tag, "shiny.tag")) {
    new_chain <- chain
    if (!is.null(tag$attribs[["data-display-if"]])) {
      new_chain <- c(chain, tag$attribs[["data-display-if"]])
    }
    if (!is.null(tag$attribs[["id"]]) && identical(tag$attribs[["id"]], target_id)) {
      return(new_chain)
    }
    for (child in tag$children) {
      result <- collect_display_if_chain(child, target_id, new_chain)
      if (!is.null(result)) {
        return(result)
      }
    }
    return(NULL)
  }
  if (is.list(tag)) {
    for (child in tag) {
      result <- collect_display_if_chain(child, target_id, chain)
      if (!is.null(result)) {
        return(result)
      }
    }
  }
  NULL
}

# load_setup_app_ui: sources app.R's top-level `ui <- navbarPage(...)`
# assignment into a fresh environment, the same way shiny::runApp()/
# shiny::testServer() do internally (both require the app directory to be
# the working directory, since app.R's own source("setup_helpers.R", local =
# TRUE) etc. calls are relative paths resolved against getwd(), not against
# app.R's own location) -- but WITHOUT starting a real Shiny session, since
# building the `ui` object itself has no Shiny-session dependency at all.
load_setup_app_ui <- function() {
  old_wd <- setwd(setup_app_dir)
  on.exit(setwd(old_wd), add = TRUE)
  ui_env <- new.env()
  source("app.R", local = ui_env)
  ui_env$ui
}

test_that("useBidsOutput checkbox is gated by conditionalPanel(input.outputDir != '') (flows 1/2: hidden until outputDir is non-blank)", {
  skip_on_cran()

  ui <- load_setup_app_ui()
  chain <- collect_display_if_chain(ui, "useBidsOutput")

  expect_equal(chain, "input.outputDir != ''")
})

test_that("copyRawFile checkbox is nested inside BOTH conditionalPanel(outputDir set) AND conditionalPanel(useBidsOutput checked) (flow 2: only revealed once useBidsOutput is checked)", {
  skip_on_cran()

  ui <- load_setup_app_ui()
  chain <- collect_display_if_chain(ui, "copyRawFile")

  expect_equal(chain, c("input.outputDir != ''", "input.useBidsOutput == true"))
})

test_that("toggling input$outputDir blank -> non-blank -> blank never triggers a server-side update*Input() on useBidsOutput/copyRawFile, and input$useBidsOutput itself is left untouched (flow 6: 'hide, don't disable' has no server-side reset to regress)", {
  skip_on_cran()

  cap <- capturing_update_session()
  shiny::testServer(setup_app_dir, {
    prime_and_clear(session, cap$calls)

    session$setInputs(outputDir = "/some/dir", useBidsOutput = TRUE)
    session$setInputs(outputDir = "")
    session$setInputs(outputDir = "/some/other/dir")

    expect_null(cap$calls[["useBidsOutput"]])
    expect_null(cap$calls[["copyRawFile"]])
    # no server-side observer silently unsets the underlying input value either
    expect_true(isolate(input$useBidsOutput))
  }, session = cap$session)
})

test_that("Setup app launch with useBidsOutput + copyRawFile checked produces BIDS-structured derivatives output plus a raw file copy on disk, exactly per P7-07's acceptance criteria (flow 3)", {
  skip_on_cran()
  skip_if_not_installed("later")

  src <- testthat::test_path("fixtures", "bids")
  dest <- tempfile("p908_bidsrun_")
  fs::dir_copy(src, dest)
  on.exit(unlink(dest, recursive = TRUE), add = TRUE)
  dest <- normalizePath(dest, winslash = "/", mustWork = TRUE)
  data_segments <- rel_home_segments(dest)

  out_dir <- tempfile("p908_bidsrun_out_")
  dir.create(out_dir, recursive = TRUE)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  shiny::testServer(setup_app_dir, {
    session$setInputs(directory = list(root = "Home", path = data_segments))
    session$setInputs(
      layout = "bids",
      subjectPattern_regex = "sub-[A-Z0-9]+",
      sessionPattern_regex = "ses-[0-9]+",
      recursiveSearch = FALSE,
      modalityPattern_regex = "",
      displayDimensionX_mm = 594,
      displayDimensionY_mm = 344,
      eyeSelection_method = "Maximize",
      validityThreshold = NA,
      outputDir = out_dir,
      useBidsOutput = TRUE,
      copyRawFile = TRUE,
      batchName = "p908bidsrun"
    )
    session$setInputs(preview = 1)
    expect_equal(preview_result()$matched_count, 4)

    session$setInputs(launch = 1)

    # future/promises resolution needs the later event loop pumped manually
    # (no running Shiny app to do it automatically) -- same technique as the
    # P9-04 launch e2e test above.
    deadline <- Sys.time() + 60
    repeat {
      later::run_now(timeoutSecs = 1)
      if (!identical(progress_state$status, "running")) break
      if (Sys.time() > deadline) break
    }
    expect_equal(progress_state$status, "done")
    expect_equal(progress_state$n_done, 4)
  })

  # Derivative outputs land under <outputDir>/derivatives/eyeQuality-v1/sub-XX/ses-XX/,
  # never flat directly under out_dir.
  qcsummary_files <- list.files(
    file.path(out_dir, "derivatives", "eyeQuality-v1"),
    pattern = "qcsummary\\.tsv$", recursive = TRUE, full.names = TRUE
  )
  expect_length(qcsummary_files, 4)
  flat_leftover <- list.files(out_dir, pattern = "qcsummary\\.tsv$", recursive = FALSE, full.names = TRUE)
  expect_length(flat_leftover, 0)

  # Raw copies land at <outputDir>/sub-XX/ses-XX/<original filename>, sibling
  # to derivatives/eyeQuality-v1/, unmodified.
  for (sub in c("sub-1", "sub-2")) {
    for (ses in c("ses-1", "ses-2")) {
      filename <- sprintf("%s_%s_recording-eyetracking_physio.tsv", sub, ses)
      raw_copy <- file.path(out_dir, sub, ses, filename)
      expect_true(file.exists(raw_copy), info = raw_copy)
      original <- file.path(dest, sub, ses, filename)
      expect_equal(readLines(raw_copy), readLines(original))
    }
  }
})

test_that("Setup tab's reconciled save/load round-trip preserves useBidsOutput/copyRawFile checked state (flow 4)", {
  skip_on_cran()

  data_dir <- tempfile("p908_save_bids_data_")
  dir.create(data_dir, recursive = TRUE)
  on.exit(unlink(data_dir, recursive = TRUE), add = TRUE)
  data_segments <- rel_home_segments(data_dir)

  save_dir <- tempfile("p908_save_bids_dest_")
  dir.create(save_dir, recursive = TRUE)
  on.exit(unlink(save_dir, recursive = TRUE), add = TRUE)
  save_segments <- rel_home_segments(save_dir)
  save_path <- file.path(normalizePath(save_dir, winslash = "/"), "bids_both_checked.yaml")

  shiny::testServer(setup_app_dir, {
    session$setInputs(directory = list(root = "Home", path = data_segments))
    session$setInputs(
      layout = "bids",
      batchName = "p908_save_bids",
      displayDimensionX_mm = 594,
      displayDimensionY_mm = 344,
      eyeSelection_method = "Maximize",
      outputDir = "/some/central/outputs",
      useBidsOutput = TRUE,
      copyRawFile = TRUE
    )

    session$setInputs(save_config = list(
      root = "Home", path = save_segments, name = "bids_both_checked.yaml", type = "yaml"
    ))
    expect_true(config_io_status()$ok)
  })

  expect_true(file.exists(save_path))
  loaded <- read_batch_config(save_path)
  expect_equal(loaded$outputStructure, "bids")
  expect_true(loaded$copyRawFile)

  # ...and reloading that same file pushes both checkboxes back to checked.
  cap <- capturing_update_session()
  shiny::testServer(setup_app_dir, {
    prime_and_clear(session, cap$calls)

    session$setInputs(load_config_file = data.frame(
      name = "bids_both_checked.yaml",
      datapath = save_path,
      stringsAsFactors = FALSE
    ))

    expect_true(config_io_status()$ok)
    expect_true(as.logical(cap$calls[["useBidsOutput"]][[1]]$value))
    expect_true(as.logical(cap$calls[["copyRawFile"]][[1]]$value))
  }, session = cap$session)
})

test_that("Setup tab's reconciled load falls back to flat/no-copy (unchecked) defaults for a pre-P9-08 config missing outputStructure/copyRawFile entirely (flow 5)", {
  skip_on_cran()

  data_dir <- tempfile("p908_load_predate_data_")
  dir.create(data_dir, recursive = TRUE)
  on.exit(unlink(data_dir, recursive = TRUE), add = TRUE)

  # a hand-authored, pre-P9-08/P7-07-style config: no outputStructure/
  # copyRawFile keys at all, exactly the shape every config saved before
  # this task existed would have.
  cfg_path <- tempfile("p908_load_predate_cfg_", fileext = ".yaml")
  on.exit(unlink(cfg_path), add = TRUE)
  yaml::write_yaml(list(
    schemaVersion = 1,
    batchName = "predates_p908",
    directoryBIDS = normalizePath(data_dir, winslash = "/"),
    layout = "bids",
    displayDimensionX_mm = 500,
    displayDimensionY_mm = 300
  ), cfg_path)

  cap <- capturing_update_session()
  shiny::testServer(setup_app_dir, {
    prime_and_clear(session, cap$calls)

    session$setInputs(load_config_file = data.frame(
      name = "predates_p908.yaml",
      datapath = cfg_path,
      stringsAsFactors = FALSE
    ))

    # loads cleanly -- no error surfaced, same as any other complete-enough config
    status <- config_io_status()
    expect_true(status$ok)

    expect_false(as.logical(cap$calls[["useBidsOutput"]][[1]]$value))
    expect_false(as.logical(cap$calls[["copyRawFile"]][[1]]$value))
  }, session = cap$session)
})
