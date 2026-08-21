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
