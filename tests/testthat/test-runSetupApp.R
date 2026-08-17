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
