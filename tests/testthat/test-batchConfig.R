# P7-02: read/write/validate machinery for batch_config.yaml, letting a
# batch run's parameters be saved and reloaded exactly. Field names are
# meant to match the real listBidsFiles()/eyeQuality()/eyeQualityBatch()
# parameters they feed -- these tests guard both the round-trip mechanics
# (explicit NULL handling, default-filling, true/false YAML serialization)
# and validate_batch_config()'s completeness (every failure mode caught,
# multiple simultaneous problems reported together).

minimal_valid_config <- function(overrides = list()) {
  cfg <- modifyList(default_batch_config(), list(
    batchName = "unittest_run",
    directoryBIDS = "/data/unittest",
    displayDimensionX_mm = 500,
    displayDimensionY_mm = 300
  ))
  modifyList(cfg, overrides, keep.null = TRUE)
}

# --- default_batch_config() --------------------------------------------

test_that("default_batch_config returns a list with schemaVersion set and layout='bids' default", {
  cfg <- default_batch_config()
  expect_equal(cfg$schemaVersion, 1L)
  expect_equal(cfg$layout, "bids")
  expect_equal(cfg$eyeSelection_method, "Maximize")
  expect_false(cfg$recursiveSearch)
  expect_null(cfg$batchName)
  expect_null(cfg$directoryBIDS)
})

# --- round trip: bids mode with explicit NULL override -------------------

test_that("write_batch_config/read_batch_config round-trips bids-mode config, preserving explicit NULL override", {
  path <- tempfile("batchconfig_bids_", fileext = ".yaml")
  on.exit(unlink(path), add = TRUE)

  cfg <- minimal_valid_config(list(
    batchName = "roundtrip_bids",
    directoryBIDS = "/data/proj_alpha",
    subjectPattern_regex = NULL, # explicit override of the non-NULL default
    displayDimensionX_mm = 480,
    displayDimensionY_mm = 270
  ))

  write_batch_config(cfg, path)
  loaded <- read_batch_config(path)

  expect_equal(loaded$batchName, "roundtrip_bids")
  expect_equal(loaded$directoryBIDS, "/data/proj_alpha")
  expect_equal(loaded$layout, "bids")
  expect_null(loaded$subjectPattern_regex)
  # sessionPattern_regex wasn't in cfg at all -- should be filled from default
  expect_equal(loaded$sessionPattern_regex, "ses-[0-9]+")
  expect_equal(loaded$displayDimensionX_mm, 480)
  expect_equal(loaded$displayDimensionY_mm, 270)
})

test_that("write_batch_config/read_batch_config round-trips glob-mode config", {
  path <- tempfile("batchconfig_glob_", fileext = ".yaml")
  on.exit(unlink(path), add = TRUE)

  cfg <- minimal_valid_config(list(
    batchName = "roundtrip_glob",
    directoryBIDS = "/data/proj_beta",
    layout = "glob",
    pathPattern = "**/*.tsv",
    excludePattern_regex = "derivatives",
    numberCores = 3
  ))

  write_batch_config(cfg, path)
  loaded <- read_batch_config(path)

  expect_equal(loaded$layout, "glob")
  expect_equal(loaded$pathPattern, "**/*.tsv")
  expect_equal(loaded$excludePattern_regex, "derivatives")
  expect_equal(loaded$numberCores, 3)
  # bids-only fields still filled from defaults even though layout is glob
  expect_equal(loaded$subjectPattern_regex, "sub-[A-Z0-9]+")
})

test_that("read_batch_config fills every field's default for a minimal config missing optional fields", {
  path <- tempfile("batchconfig_minimal_", fileext = ".yaml")
  on.exit(unlink(path), add = TRUE)

  yaml::write_yaml(list(
    schemaVersion = 1,
    batchName = "minimal_run",
    directoryBIDS = "/data/proj_gamma",
    displayDimensionX_mm = 100,
    displayDimensionY_mm = 100
  ), path)

  loaded <- read_batch_config(path)

  expect_equal(loaded$layout, "bids")
  expect_equal(loaded$eyeSelection_method, "Maximize")
  expect_false(loaded$recursiveSearch)
  expect_null(loaded$numberCores)
  expect_null(loaded$outputDir)
  expect_null(loaded$validityThreshold)
})

# --- YAML serialization: true/false not yes/no ---------------------------

test_that("write_batch_config serializes logical fields as true/false, not YAML-1.1 yes/no", {
  path <- tempfile("batchconfig_bool_", fileext = ".yaml")
  on.exit(unlink(path), add = TRUE)

  cfg <- minimal_valid_config(list(recursiveSearch = TRUE))
  write_batch_config(cfg, path)

  raw_lines <- readLines(path)
  recursive_line <- grep("^recursiveSearch:", raw_lines, value = TRUE)

  expect_length(recursive_line, 1)
  expect_match(recursive_line, "recursiveSearch: true", fixed = TRUE)
  expect_false(any(grepl("recursiveSearch:\\s*yes", raw_lines)))
})

test_that("write_batch_config serializes a FALSE logical field as false, not no", {
  path <- tempfile("batchconfig_bool_false_", fileext = ".yaml")
  on.exit(unlink(path), add = TRUE)

  cfg <- minimal_valid_config(list(recursiveSearch = FALSE))
  write_batch_config(cfg, path)

  raw_lines <- readLines(path)
  recursive_line <- grep("^recursiveSearch:", raw_lines, value = TRUE)

  expect_match(recursive_line, "recursiveSearch: false", fixed = TRUE)
})

# --- validate_batch_config(): individual failure modes -------------------

test_that("validate_batch_config errors when a required field (batchName) is missing", {
  cfg <- minimal_valid_config()
  cfg$batchName <- NULL

  expect_error(validate_batch_config(cfg), "batchName.*required")
})

test_that("validate_batch_config errors when a field has the wrong type", {
  cfg <- minimal_valid_config(list(displayDimensionX_mm = "not_a_number"))

  expect_error(validate_batch_config(cfg), "displayDimensionX_mm")
})

test_that("validate_batch_config errors on an invalid layout value", {
  cfg <- minimal_valid_config(list(layout = "flat"))

  expect_error(validate_batch_config(cfg), "`layout` must be one of")
})

test_that("validate_batch_config errors when layout is 'glob' but pathPattern is missing", {
  cfg <- minimal_valid_config(list(layout = "glob"))

  expect_error(validate_batch_config(cfg), "pathPattern.*required.*glob")
})

test_that("validate_batch_config errors on an unregistered adapterType", {
  cfg <- minimal_valid_config(list(adapterType = "SomeMadeUpTracker"))

  expect_error(validate_batch_config(cfg), "SomeMadeUpTracker.*not a currently registered adapter")
})

test_that("validate_batch_config accepts every currently registered adapterType", {
  for (adapter_name in names(registered_adapters())) {
    cfg <- minimal_valid_config(list(adapterType = adapter_name))
    expect_true(validate_batch_config(cfg))
  }
})

test_that("validate_batch_config errors on an invalid eyeSelection_method", {
  cfg <- minimal_valid_config(list(eyeSelection_method = "Average"))

  expect_error(validate_batch_config(cfg), "eyeSelection_method")
})

test_that("validate_batch_config errors on an out-of-range validityThreshold", {
  cfg <- minimal_valid_config(list(validityThreshold = 1.5))

  expect_error(validate_batch_config(cfg), "validityThreshold")
})

test_that("validate_batch_config errors on a non-positive numberCores", {
  cfg <- minimal_valid_config(list(numberCores = 0))

  expect_error(validate_batch_config(cfg), "numberCores")
})

test_that("validate_batch_config errors on a schemaVersion newer than the package supports", {
  cfg <- minimal_valid_config(list(schemaVersion = 999))

  expect_error(validate_batch_config(cfg), "newer than this package version supports")
})

test_that("validate_batch_config returns TRUE invisibly for a fully valid config", {
  cfg <- minimal_valid_config()
  expect_true(validate_batch_config(cfg))
})

# --- validate_batch_config(): multiple simultaneous problems -------------

test_that("validate_batch_config reports multiple simultaneous problems in one error, not just the first", {
  cfg <- list(
    schemaVersion = "bad",
    layout = "not_a_layout",
    eyeSelection_method = "Bogus",
    recursiveSearch = "yes"
    # batchName, directoryBIDS, displayDimensionX_mm, displayDimensionY_mm
    # all missing too
  )

  err <- tryCatch(validate_batch_config(cfg), error = function(e) conditionMessage(e))

  expect_match(err, "schemaVersion")
  expect_match(err, "batchName")
  expect_match(err, "directoryBIDS")
  expect_match(err, "displayDimensionX_mm")
  expect_match(err, "displayDimensionY_mm")
  expect_match(err, "`layout` must be one of")
  expect_match(err, "recursiveSearch")
  expect_match(err, "eyeSelection_method")
  # confirm this genuinely accumulated several distinct problems, not one
  n_bullets <- lengths(regmatches(err, gregexpr("\n  - ", err)))
  expect_gte(n_bullets, 7)
})

# --- write_batch_config(): refuses invalid config, no partial file -------

test_that("write_batch_config refuses to write an invalid config and leaves no file behind", {
  path <- tempfile("batchconfig_invalid_write_", fileext = ".yaml")
  on.exit(unlink(path), add = TRUE)

  expect_error(write_batch_config(list(batchName = "incomplete_only"), path), "required")
  expect_false(file.exists(path))
})

# --- read_batch_config(): validate = TRUE vs FALSE ------------------------

test_that("read_batch_config(validate = FALSE) loads an incomplete config without erroring", {
  path <- tempfile("batchconfig_incomplete_", fileext = ".yaml")
  on.exit(unlink(path), add = TRUE)

  yaml::write_yaml(list(schemaVersion = 1, batchName = "incomplete"), path)

  loaded <- read_batch_config(path, validate = FALSE)
  expect_equal(loaded$batchName, "incomplete")
  expect_null(loaded$directoryBIDS)
})

test_that("read_batch_config(validate = TRUE) errors on the same incomplete config", {
  path <- tempfile("batchconfig_incomplete2_", fileext = ".yaml")
  on.exit(unlink(path), add = TRUE)

  yaml::write_yaml(list(schemaVersion = 1, batchName = "incomplete"), path)

  expect_error(read_batch_config(path, validate = TRUE), "directoryBIDS")
})

test_that("read_batch_config errors on a nonexistent file", {
  expect_error(
    read_batch_config(tempfile("batchconfig_missing_", fileext = ".yaml")),
    "does not exist"
  )
})

# --- template file ---------------------------------------------------------

# --- qcThresholds (P10-07) ------------------------------------------------

test_that("default_batch_config includes qcThresholds = NULL", {
  cfg <- default_batch_config()
  expect_true("qcThresholds" %in% names(cfg))
  expect_null(cfg$qcThresholds)
})

test_that(".known_qc_threshold_ids matches the Analyze tabs' qc_threshold_config exactly (single source of truth)", {
  # inst/shiny-apps/app/analyze_helpers.R: moved there from
  # inst/shiny-apps/analyze/helpers.R by P10-12's app merge.
  helpers_path <- system.file("shiny-apps", "app", "analyze_helpers.R", package = "eyeQuality")
  expect_true(nzchar(helpers_path))

  helpers_env <- new.env()
  source(helpers_path, local = helpers_env)
  expected_ids <- unique(helpers_env$qc_threshold_config$threshold_id)

  expect_setequal(.known_qc_threshold_ids(), expected_ids)
  # pin the concrete values too, so a change to qc_threshold_config's ids is
  # visible here even if this test's helper-sourcing somehow drifted
  expect_setequal(.known_qc_threshold_ids(), c("valid_pct", "robust_pct", "interp_pct"))
})

test_that("validate_batch_config treats a config with no qcThresholds section as fully valid (backward compat)", {
  cfg <- minimal_valid_config()
  expect_null(cfg$qcThresholds)
  expect_true(validate_batch_config(cfg))
})

test_that("validate_batch_config accepts recognized qcThresholds ids with in-range values", {
  cfg <- minimal_valid_config(list(qcThresholds = list(valid_pct = 75, robust_pct = 60, interp_pct = 15)))
  expect_true(validate_batch_config(cfg))
})

test_that("validate_batch_config accepts qcThresholds boundary values 0 and 100 (inclusive range)", {
  cfg_zero <- minimal_valid_config(list(qcThresholds = list(valid_pct = 0)))
  cfg_hundred <- minimal_valid_config(list(qcThresholds = list(valid_pct = 100)))
  expect_true(validate_batch_config(cfg_zero))
  expect_true(validate_batch_config(cfg_hundred))
})

test_that("validate_batch_config errors on a qcThresholds value just outside the 0-100 range", {
  cfg_neg <- minimal_valid_config(list(qcThresholds = list(valid_pct = -1)))
  cfg_over <- minimal_valid_config(list(qcThresholds = list(valid_pct = 101)))

  expect_error(validate_batch_config(cfg_neg), "qcThresholds.*valid_pct.*between 0 and 100")
  expect_error(validate_batch_config(cfg_over), "qcThresholds.*valid_pct.*between 0 and 100")
})

test_that("validate_batch_config errors on an unrecognized qcThresholds threshold_id", {
  cfg <- minimal_valid_config(list(qcThresholds = list(made_up_metric = 50)))
  expect_error(validate_batch_config(cfg), "qcThresholds.*unrecognized threshold_id.*made_up_metric")
})

test_that("validate_batch_config errors when qcThresholds is not a named list", {
  cfg_unnamed <- minimal_valid_config(list(qcThresholds = list(50, 60)))
  cfg_not_list <- minimal_valid_config(list(qcThresholds = "not a list"))

  expect_error(validate_batch_config(cfg_unnamed), "qcThresholds.*must all be named")
  expect_error(validate_batch_config(cfg_not_list), "qcThresholds.*must be NULL or a named list")
})

test_that("validate_batch_config errors when a qcThresholds value is non-numeric", {
  cfg <- minimal_valid_config(list(qcThresholds = list(valid_pct = "eighty")))
  expect_error(validate_batch_config(cfg), "qcThresholds.*valid_pct.*between 0 and 100")
})

test_that("qcThresholds validation fails open (does not block an unrecognized id) when the id list can't be sourced", {
  # Reproduces the "helpers.R can't be located" branch of .known_qc_threshold_ids()
  # without touching the filesystem: stub the internal id-lookup to return
  # character(0), the exact value that function returns when system.file()
  # can't find inst/shiny-apps/app/analyze_helpers.R (e.g. an unusual install).
  # validate_batch_config() must treat that as "can't verify, don't block"
  # rather than rejecting every qcThresholds entry as unrecognized.
  testthat::local_mocked_bindings(
    .known_qc_threshold_ids = function() character(0)
  )

  cfg <- minimal_valid_config(list(qcThresholds = list(anything_at_all = 50)))
  expect_true(validate_batch_config(cfg))

  # a value still out of range must still be caught even when ids can't be verified
  cfg_bad_value <- minimal_valid_config(list(qcThresholds = list(anything_at_all = 150)))
  expect_error(validate_batch_config(cfg_bad_value), "between 0 and 100")
})

test_that("write_batch_config/read_batch_config round-trips qcThresholds", {
  path <- tempfile("batchconfig_qcthresholds_", fileext = ".yaml")
  on.exit(unlink(path), add = TRUE)

  cfg <- minimal_valid_config(list(qcThresholds = list(valid_pct = 72, robust_pct = 65, interp_pct = 18)))
  write_batch_config(cfg, path)
  loaded <- read_batch_config(path)

  expect_equal(loaded$qcThresholds$valid_pct, 72)
  expect_equal(loaded$qcThresholds$robust_pct, 65)
  expect_equal(loaded$qcThresholds$interp_pct, 18)
})

test_that("write_batch_config/read_batch_config round-trips a config predating qcThresholds as NULL (forward-compat, no section written)", {
  path <- tempfile("batchconfig_no_qcthresholds_", fileext = ".yaml")
  on.exit(unlink(path), add = TRUE)

  # a config saved with no qcThresholds key at all -- as every config written
  # before this field existed would be
  yaml::write_yaml(list(
    schemaVersion = 1,
    batchName = "predates_qcthresholds",
    directoryBIDS = "/data/pre",
    displayDimensionX_mm = 200,
    displayDimensionY_mm = 200
  ), path)

  loaded <- read_batch_config(path)
  expect_null(loaded$qcThresholds)
  expect_true(validate_batch_config(loaded))
})

test_that("the bundled batch_config_template.yaml parses and validates cleanly", {
  template_path <- system.file("templates", "batch_config_template.yaml", package = "eyeQuality")
  expect_true(nzchar(template_path))

  loaded <- read_batch_config(template_path)
  expect_equal(loaded$layout, "bids")
  expect_equal(loaded$batchName, "study1_run1")
  expect_equal(loaded$displayDimensionX_mm, 594)
  expect_equal(loaded$displayDimensionY_mm, 344)
})
