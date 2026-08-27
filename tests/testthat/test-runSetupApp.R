# P9-01: regression tests for build_dry_run_preview(), the Shiny-free helper
# behind the Setup app's dry-run preview panel (inst/shiny-apps/setup/
# helpers.R). Sourced via system.file() rather than a relative path, both
# because that's the only way to reach inst/ files portably from tests and
# because it doubles as a check that the app is packaged where
# runSetupApp() (R/runSetupApp.R) expects to find it.
#
# build_dry_run_preview() itself has no Shiny dependency, so these tests
# don't need a running Shiny session or shinyFiles at all.

helpers_path <- system.file("shiny-apps", "setup", "helpers.R", package = "eyeQuality")
if (!nzchar(helpers_path)) {
  stop("test-runSetupApp.R: could not locate inst/shiny-apps/setup/helpers.R via system.file()")
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

setup_app_dir <- system.file("shiny-apps", "setup", package = "eyeQuality")
if (!nzchar(setup_app_dir)) {
  stop("test-runSetupApp.R: could not locate inst/shiny-apps/setup/ via system.file()")
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

test_that("Setup app's post_run_link panel shows the outputDir-resolved directory and a matching launch command once a run reaches status == 'done'", {
  skip_on_cran()

  shiny::testServer(setup_app_dir, {
    session$setInputs(outputDir = "/central/outputs")
    run_info$directory <- "/data/study_a"
    progress_state$status <- "done"
    session$flushReact()

    html <- paste(as.character(session$getOutput("post_run_link")), collapse = "\n")
    expect_true(grepl("/central/outputs", html, fixed = TRUE))
    expect_true(grepl('eyeQuality::runAnalyzeApp(initialDirectory = "/central/outputs")', html, fixed = TRUE))
    # outputDir was set, so directoryBIDS (run_info$directory) must not be
    # what's shown/linked -- pins down which of the two resolve_analyze_directory()
    # actually chose, not just that something plausible-looking was rendered.
    expect_false(grepl("/data/study_a", html, fixed = TRUE))
  })
})

test_that("Setup app's post_run_link panel falls back to directoryBIDS when outputDir was left blank for the run", {
  skip_on_cran()

  shiny::testServer(setup_app_dir, {
    session$setInputs(outputDir = "")
    run_info$directory <- "/data/study_b"
    progress_state$status <- "done"
    session$flushReact()

    html <- paste(as.character(session$getOutput("post_run_link")), collapse = "\n")
    expect_true(grepl("/data/study_b", html, fixed = TRUE))
    expect_true(grepl('eyeQuality::runAnalyzeApp(initialDirectory = "/data/study_b")', html, fixed = TRUE))
  })
})
