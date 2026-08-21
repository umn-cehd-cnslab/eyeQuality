# Regression tests for windows_long_path() and the windows_safe_read_*()
# wrappers (R/windowsLongPath.R), which together let this package's readr
# call sites keep working once a full path exceeds Windows' classic ~260-
# character MAX_PATH limit -- confirmed to be a real, reachable case (a Box-
# synced study tree combined with this package's own nested
# derivatives/eyeQuality-v1/ output convention and long BIDS-style filenames)
# where file.exists()/list.files() (a different Windows API, no such limit)
# still find the file while readr reports it as "does not exist". See
# R/windowsLongPath.R's roxygen docs for the full explanation, including why
# windows_long_path() alone (just prefixing the path string) is NOT enough --
# readr/vroom's own pre-open existence check does not honor a `\\?\`-prefixed
# path, which is why the actual read call sites go through
# windows_safe_read_csv()/windows_safe_read_tsv()/windows_safe_read_lines()
# instead, which open the file themselves via a base R connection.
#
# windows_long_path() takes an `os_type` argument (defaulting to
# `.Platform$OS.type`) purely so its Windows-only branches can be exercised
# here on whatever platform the test suite happens to run on -- this repo's
# actual CI/dev machines are non-Windows (WSL R), so this is the only way to
# get real coverage of the \\?\ / \\?\UNC\ logic without a native Windows
# test run. The connection-opening behavior inside windows_safe_read_*()
# itself can only be exercised for real on native Windows (verified
# separately, by hand, against a real >260-character path fixture -- not
# reproducible here since WSL/Linux has no MAX_PATH-style limit to trigger
# in the first place); what's tested below is the non-Windows passthrough
# branch those functions take on this machine's actual platform, which is
# also exactly the branch every non-Windows CI/dev run of this suite
# exercises for real.

test_that("windows_long_path() returns path unchanged on non-Windows platforms", {
  input <- "/home/user/some/deeply/nested/path/file.tsv"
  expect_equal(windows_long_path(input, os_type = "unix"), input)
})

test_that("windows_long_path() does not double-prefix an already-extended-length path", {
  already_prefixed <- "\\\\?\\C:\\Users\\foo\\bar\\baz.tsv"
  expect_equal(windows_long_path(already_prefixed, os_type = "windows"), already_prefixed)

  already_prefixed_unc <- "\\\\?\\UNC\\server\\share\\baz.tsv"
  expect_equal(windows_long_path(already_prefixed_unc, os_type = "windows"), already_prefixed_unc)
})

test_that("windows_long_path() prefixes a plain absolute path with \\\\?\\", {
  expect_equal(
    windows_long_path("C:/Users/foo/bar/baz.tsv", os_type = "windows"),
    "\\\\?\\C:\\Users\\foo\\bar\\baz.tsv"
  )

  # a path already using backslash separators should come out identically
  expect_equal(
    windows_long_path("C:\\Users\\foo\\bar\\baz.tsv", os_type = "windows"),
    "\\\\?\\C:\\Users\\foo\\bar\\baz.tsv"
  )
})

test_that("windows_long_path() rewrites a UNC path to the \\\\?\\UNC\\ form", {
  expect_equal(
    windows_long_path("\\\\server\\share\\sub\\file.tsv", os_type = "windows"),
    "\\\\?\\UNC\\server\\share\\sub\\file.tsv"
  )

  # forward-slash UNC form should resolve to the same result
  expect_equal(
    windows_long_path("//server/share/sub/file.tsv", os_type = "windows"),
    "\\\\?\\UNC\\server\\share\\sub\\file.tsv"
  )
})

test_that("windows_long_path() resolves a relative path to absolute before prefixing", {
  base_dir <- tempfile("windows_long_path_rel_")
  dir.create(file.path(base_dir, "sub"), recursive = TRUE)
  on.exit(unlink(base_dir, recursive = TRUE), add = TRUE)
  writeLines("x", file.path(base_dir, "sub", "a.tsv"))

  old_wd <- setwd(base_dir)
  on.exit(setwd(old_wd), add = TRUE)

  result <- windows_long_path(file.path("sub", "a.tsv"), os_type = "windows")

  expect_true(startsWith(result, "\\\\?\\"))
  # the resolved, backslash-converted absolute path must actually be present
  # inside the prefixed result -- proves resolution happened rather than the
  # relative form simply being glued onto the prefix as-is.
  expected_abs <- gsub("/", "\\", normalizePath(base_dir, winslash = "/", mustWork = TRUE), fixed = TRUE)
  expect_true(grepl(expected_abs, result, fixed = TRUE))
  expect_true(endsWith(result, "sub\\a.tsv"))
})

# The three helpers below back windows_long_path()'s manual fallback --
# needed because normalizePath(mustWork = FALSE) can silently fail to
# resolve a path whose tail doesn't yet exist on disk (confirmed directly
# against native Windows R), rather than erroring or actually resolving it.
# Tested directly, on hand-written Windows-style strings, rather than only
# indirectly through windows_long_path() + getwd() -- getwd() returns a
# POSIX-style path with no drive letter on this (non-Windows) test machine,
# which would confound a getwd()-routed test of this fallback logic with a
# platform difference that can't actually occur on real Windows (where
# getwd() always returns a drive-letter path).
test_that(".windows_force_backslash() converts every '/' to a single '\\'", {
  expect_equal(.windows_force_backslash("C:/Users/foo/bar"), "C:\\Users\\foo\\bar")
  expect_equal(nchar(.windows_force_backslash("a/b")), 3L) # not doubled
})

test_that(".windows_is_absolute_path() recognizes drive-letter and UNC forms only", {
  expect_true(.windows_is_absolute_path("C:\\Users\\foo"))
  expect_true(.windows_is_absolute_path("\\\\server\\share"))
  expect_false(.windows_is_absolute_path("relative\\path"))
  expect_false(.windows_is_absolute_path("..\\relative"))
})

test_that(".windows_collapse_dot_segments() collapses '.'/'..' while preserving the root", {
  expect_equal(
    .windows_collapse_dot_segments("C:\\a\\b\\..\\c\\.\\d"),
    "C:\\a\\c\\d"
  )
  expect_equal(
    .windows_collapse_dot_segments("\\\\server\\share\\a\\..\\b"),
    "\\\\server\\share\\b"
  )
})

# windows_safe_read_csv()/windows_safe_read_tsv()/windows_safe_read_lines()
# (R/windowsLongPath.R): on any non-Windows platform -- which is what this
# suite actually runs on -- these are a plain passthrough to the
# corresponding readr function, with no connection-opening detour. That
# passthrough branch is real, reachable code (not just a stub for the
# Windows-only branch), so it gets real coverage here rather than being
# assumed correct.
test_that("windows_safe_read_csv()/read_tsv()/read_lines() match plain readr calls on this (non-Windows) platform", {
  skip_if(.Platform$OS.type == "windows", "this test targets the non-Windows passthrough branch specifically")

  tmp_dir <- tempfile("windows_safe_read_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  csv_path <- file.path(tmp_dir, "a.csv")
  tsv_path <- file.path(tmp_dir, "a.tsv")
  writeLines(c("a,b", "1,2"), csv_path)
  writeLines(c("a\tb", "1\t2"), tsv_path)

  expect_equal(
    windows_safe_read_csv(csv_path, show_col_types = FALSE),
    readr::read_csv(csv_path, show_col_types = FALSE)
  )
  expect_equal(
    windows_safe_read_tsv(tsv_path, show_col_types = FALSE),
    readr::read_tsv(tsv_path, show_col_types = FALSE)
  )
  expect_equal(
    windows_safe_read_lines(csv_path),
    readr::read_lines(csv_path)
  )
})

# ---------------------------------------------------------------------------
# Core-package end-to-end long-path integration tests (qa-verifier Gap 2)
# ---------------------------------------------------------------------------
#
# The tests above cover windows_long_path()/windows_safe_read_*() in
# isolation (plus their documented non-Windows passthrough branch). Nothing
# in this file previously exercised the real public functions efa3c69
# actually patched -- importData() (R/importData.R) and
# parsePreprocessingBatchSummary() (R/parsePreprocessingBatchSummary.R) --
# against a genuinely long path; that connection-opening behavior was, per
# this file's own header comment above, verified by hand rather than via an
# automated test. This mirrors the equivalent Shiny-app-layer test added
# alongside 4baea31 in test-runAnalyzeApp.R's "Windows MAX_PATH" section --
# same reasoning, same real >260-character construction, applied to the core
# package's own read call sites instead of the app's.
#
# Every eyeQualityBatch() run below routes its OUTPUT (qcsummary/preproc/
# events/runtimes, written via saveFiles()->create_new_filename()) to a
# short, separate outputDir -- NOT because that write side is untested (see
# test-saveFiles.R's own Gap 1 coverage for that), but because
# create_new_filename() assembles its path via fs::path()/fs::dir_create(),
# which enforce their OWN separate <260-char ceiling ("Total path length
# must be less than PATH_MAX: 260", confirmed directly on Windows R)
# regardless of windows_long_path()'s "\\?\" prefixing -- an unrelated,
# already-documented gap (see create_new_filename()'s own comment in
# R/saveFiles.R), not something these tests are trying to exercise.
#
# The raw INPUT file's own path is deliberately kept UNDER 260 characters in
# the eyeQualityBatch()-based tests below -- a second, previously
# undiscovered limitation, confirmed directly here: eyeQualityBatch()'s
# resumability check calls get_qcsummary_output_path() on every candidate
# file unconditionally (R/eyeQualityBatch.R), which -- like
# create_new_filename() -- computes `.safe_basename(fs::path_ext_remove(inputFile))`
# and, separately, `fs::path_dir(inputFile)`. `.safe_basename()`'s whole
# purpose is to avoid base R's own basename()/dirname() "path too long"
# error (see its header comment in R/windowsLongPath.R), but
# `fs::path_ext_remove()`/`fs::path_dir()` call base R's `dirname()`
# *internally*, on the raw `inputFile` string, *before* `.safe_basename()`
# ever runs -- so a long `inputFile` still crashes here with base R's own
# "path too long" error, regardless of `.safe_basename()`. Confirmed via a
# targeted probe on this project's Windows R environment that this specific
# error triggers at exactly 260 characters (not "around 300", as
# .safe_basename()'s own header comment estimates elsewhere in this
# package -- that estimate appears to have been imprecise). This is a real,
# currently-open gap in efa3c69's fix, out of this test's scope to close;
# these tests route around it (raw input path kept safely under 260 chars,
# a long, padded `batchName` used instead to push the batch summary path
# over 260) so they can still exercise what IS fixed and in scope: the
# actual read call sites. importData() itself is tested directly against a
# genuinely long (>260-char) raw file path further down, since importData()
# alone never touches get_qcsummary_output_path()/create_new_filename() and
# is unaffected by this gap.
#
# What IS long and load-bearing in the eyeQualityBatch()-based tests below:
# the batch run summary file itself (`batch_run_summary` in
# R/eyeQualityBatch.R is a plain paste0() string, not fs::path()-assembled
# and never passed through dirname()/basename(), so nothing caps its length)
# that parsePreprocessingBatchSummary() reads back afterward.

# Builds a real, deeply-nested directory whose own path is at least
# target_len characters, via plain file.path() (no fs involved, so it isn't
# subject to fs::path()'s separate 260-char ceiling). dir.create() itself is
# routed through windows_long_path() -- confirmed directly (matching
# test-runAnalyzeApp.R's identical need) that a raw, unprefixed dir.create()
# can itself silently fail partway through a sufficiently long recursive
# path.
p_gap2_build_long_dir <- function(base_dir, target_len = 210) {
  segment <- "eyeQuality_windows_longpath_gap2_segment"
  nested <- base_dir
  i <- 0
  while (nchar(nested) < target_len) {
    i <- i + 1
    nested <- file.path(nested, paste0(segment, "_", i))
  }
  dir.create(windows_long_path(nested), recursive = TRUE, showWarnings = FALSE)
  nested
}

# Builds a one-subject/one-session BIDS-like tree, runs a real
# eyeQualityBatch() against it (outputDir short, per this section's header
# comment), and returns the paths callers need. file.copy() is used (rather
# than hand-writing the fixture) after confirming directly on Windows R that
# it correctly honors a windows_long_path()-prefixed destination.
#
# directoryBIDS itself is kept modest (target_len below), and the raw input
# file's basename kept short, so raw_file (directoryBIDS + "/sub-01/ses-01/"
# + basename) stays safely under the 260-character get_qcsummary_output_path()
# ceiling described in this section's header comment. batch_name is instead
# padded by the caller (its own length adds directly to batch_run_summary's
# length, and NOT to raw_file's) so batch_run_summary -- the file this
# section's tests actually need to be long -- still comes out genuinely over
# 260 characters.
p_gap2_run_long_path_batch <- function(batch_name) {
  base_dir <- tempfile("p_gap2_batch_")
  dir.create(base_dir)

  directoryBIDS <- p_gap2_build_long_dir(base_dir, target_len = 180)
  session_dir <- file.path(directoryBIDS, "sub-01", "ses-01")
  dir.create(windows_long_path(session_dir), recursive = TRUE, showWarnings = FALSE)

  raw_fixture <- testthat::test_path("fixtures", "tobii_studio_sample.tsv")
  raw_file <- file.path(session_dir, "physio.tsv")
  file.copy(raw_fixture, windows_long_path(raw_file))
  # sanity margin against the 260-char get_qcsummary_output_path() ceiling
  # this helper is specifically designed to stay clear of
  stopifnot(nchar(raw_file) < 250)

  out_dir <- tempfile("p_gap2_batch_out_")

  eyeQualityBatch(
    directoryBIDS,
    batchName = batch_name,
    numberCores = 1,
    outputDir = out_dir,
    displayDimensionX_mm = 594,
    displayDimensionY_mm = 344
  )

  summary_file <- file.path(directoryBIDS, paste0("preprocessing_batch_summary_desc-", batch_name, ".txt"))

  list(
    base_dir = base_dir,
    directoryBIDS = directoryBIDS,
    raw_file = raw_file,
    out_dir = out_dir,
    summary_file = summary_file
  )
}

# A long, otherwise-harmless batchName -- see p_gap2_run_long_path_batch()'s
# header comment for why padding batchName (rather than the raw input path)
# is what pushes batch_run_summary over 260 characters here.
p_gap2_long_batch_name <- function(label) {
  paste0(label, strrep("x", 60))
}

test_that("eyeQualityBatch() end-to-end: a real batch run succeeds and parsePreprocessingBatchSummary() reads its batch summary at a real path over 260 characters", {
  skip_on_cran()

  batch_name <- p_gap2_long_batch_name("pgap2main")
  fx <- p_gap2_run_long_path_batch(batch_name)
  on.exit(unlink(c(fx$base_dir, fx$out_dir), recursive = TRUE), add = TRUE)

  expect_true(nchar(fx$summary_file) > 260)

  qcsummary_out <- create_new_filename(
    fx$raw_file, paste0("_desc-", batch_name, "_preproc_qcsummary"), ".tsv",
    outputDir = fx$out_dir
  )
  expect_true(file.exists(qcsummary_out))
  # the output actually reflects a real, successful read+process of the raw
  # input file at its own path (importData() inside eyeQuality()), not just
  # an empty/error placeholder
  qc_content <- read.table(qcsummary_out, header = TRUE, sep = "\t")
  expect_true("valid_raw_data" %in% qc_content$qc_metric)

  summary_result <- parsePreprocessingBatchSummary(fx$summary_file, "summary")
  expect_equal(summary_result$nfiles, 1)
  expect_equal(summary_result$nPreprocessed, 1)
  expect_equal(summary_result$nFailed, 0)

  successful_result <- parsePreprocessingBatchSummary(fx$summary_file, "successfulfiles")
  expect_length(successful_result, 1)
  expect_equal(normalizePath(successful_result), normalizePath(qcsummary_out))

  failed_result <- parsePreprocessingBatchSummary(fx$summary_file, "failedfiles")
  expect_equal(failed_result, tibble::tibble(file = character(0), error = character(0)))
})

test_that("importData() reads a real file correctly at a genuinely >260-character path, matching a normal-length-path read of the same fixture", {
  skip_on_cran()

  base_dir <- tempfile("p_gap2_importdata_")
  dir.create(base_dir)
  on.exit(unlink(base_dir, recursive = TRUE), add = TRUE)

  nested <- p_gap2_build_long_dir(base_dir, target_len = 220)
  raw_fixture <- testthat::test_path("fixtures", "tobii_studio_sample.tsv")
  long_path <- file.path(nested, "sub-01_ses-01_recording-eyetracking_physio.tsv")
  file.copy(raw_fixture, windows_long_path(long_path))
  expect_true(nchar(long_path) > 260)

  short_result <- importData(raw_fixture)
  long_result <- importData(long_path)

  expect_equal(long_result, short_result)
})

test_that("importData()'s long-path protection is actually necessary -- a plain readr::read_tsv() call fails at the same long path where importData() succeeds (negative control)", {
  skip_on_cran()
  skip_if_not(
    .Platform$OS.type == "windows",
    "MAX_PATH is Windows-specific; this negative control needs real Windows execution to be meaningful (no such limit exists to trigger on this platform)"
  )

  base_dir <- tempfile("p_gap2_importdata_neg_")
  dir.create(base_dir)
  on.exit(unlink(base_dir, recursive = TRUE), add = TRUE)

  nested <- p_gap2_build_long_dir(base_dir, target_len = 220)
  raw_fixture <- testthat::test_path("fixtures", "tobii_studio_sample.tsv")
  long_path <- file.path(nested, "sub-01_ses-01_recording-eyetracking_physio.tsv")
  file.copy(raw_fixture, windows_long_path(long_path))
  expect_true(nchar(long_path) > 260)

  expect_no_error(importData(long_path))
  expect_error(readr::read_tsv(long_path, show_col_types = FALSE))
})

test_that("parsePreprocessingBatchSummary()'s long-path protection is actually necessary -- plain readr calls fail at the same long summary path where it succeeds (negative control)", {
  skip_on_cran()
  skip_if_not(
    .Platform$OS.type == "windows",
    "MAX_PATH is Windows-specific; this negative control needs real Windows execution to be meaningful (no such limit exists to trigger on this platform)"
  )

  fx <- p_gap2_run_long_path_batch(p_gap2_long_batch_name("pgap2neg"))
  on.exit(unlink(c(fx$base_dir, fx$out_dir), recursive = TRUE), add = TRUE)

  expect_no_error(parsePreprocessingBatchSummary(fx$summary_file, "summary"))
  expect_error(readr::read_tsv(fx$summary_file, col_names = FALSE, show_col_types = FALSE))
  expect_error(readr::read_lines(fx$summary_file))
})
