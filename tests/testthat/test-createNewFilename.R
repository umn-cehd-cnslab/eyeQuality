# P2-10: path-style robustness tests for create_new_filename() (R/saveFiles.R).
#
# `create_new_filename()` receives `inputfile` as a raw string with no
# normalization applied by any call site -- eyeQuality()'s `filepath`
# argument (see R/eyeQuality.R) is whatever the caller typed, and flows
# straight through to create_new_filename() without ever passing through
# fs::path() first. So both POSIX-style paths ("/home/foo/bar/baz.tsv") and
# Windows-style paths -- both the forward-slash form that list.files()/
# fs::path()/tempfile() always produce, even on native Windows R, and the
# raw backslash form a user might paste in directly (e.g. from Explorer's
# address bar, or a literal path typed at the console) -- are realistic,
# reachable inputs. This file checks all of them.
#
# create_new_filename() internally does:
#   filename  <- basename(fs::path_ext_remove(inputfile))
#   directory <- fs::path_dir(inputfile)
#   file_extension <- fs::path_ext(inputfile)
# `fs::path_ext_remove()`/`fs::path_ext()` normalize "\\" to "/" internally
# regardless of host OS (verified directly against both a native-Windows R
# 4.2.1 session and a POSIX R 3.6.3 session), so filename/extension parsing
# is separator-style-agnostic everywhere. `fs::path_dir()`, however, is NOT:
# on native Windows it correctly treats "\\" as a separator (deferring to
# the OS's own path semantics), but on POSIX platforms a literal
# "C:\\Users\\foo\\bar\\baz.tsv" resolves to "." instead of the intended
# directory. That divergence only matters for the default (outputDir = NULL)
# case, where the output directory is derived from inputfile's own
# directory -- see the last test below.

test_that("create_new_filename() extracts filename and extension from a POSIX-style input path when outputDir is supplied", {
  out_dir <- tempfile("p210_posix_out_")
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  inputfile <- "/home/foo/bar/baz.tsv"

  result <- create_new_filename(inputfile, "_desc-preproc", ".tsv", outputDir = out_dir)

  expect_equal(fs::path(result), fs::path(out_dir, "baz_desc-preproc.tsv"))
})

test_that("create_new_filename() extracts filename and extension from a Windows-style forward-slash input path when outputDir is supplied", {
  out_dir <- tempfile("p210_winfwd_out_")
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  inputfile <- "C:/Users/foo/bar/baz.tsv"

  result <- create_new_filename(inputfile, "_desc-preproc", ".tsv", outputDir = out_dir)

  expect_equal(fs::path(result), fs::path(out_dir, "baz_desc-preproc.tsv"))
})

test_that("create_new_filename() extracts filename and extension from a raw Windows-style backslash input path when outputDir is supplied", {
  # Using outputDir here sidesteps fs::path_dir()'s POSIX/backslash
  # limitation (see file header) entirely, since the output directory is
  # taken from outputDir rather than derived from inputfile. This isolates
  # and confirms that filename/extension parsing specifically survives a raw
  # backslash-separated input, independent of host OS -- this test runs
  # (and must pass) on every platform, not just Windows.
  out_dir <- tempfile("p210_winback_out_")
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  inputfile <- "C:\\Users\\foo\\bar\\baz.tsv"

  result <- create_new_filename(inputfile, "_desc-preproc", ".tsv", outputDir = out_dir)

  expect_equal(fs::path(result), fs::path(out_dir, "baz_desc-preproc.tsv"))
})

test_that("create_new_filename() with default outputDir derives the correct output directory from a real forward-slash absolute path", {
  base_dir <- tempfile("p210_fwd_base_")
  dir.create(base_dir)
  on.exit(unlink(base_dir, recursive = TRUE), add = TRUE)

  # tempfile() always returns forward-slash paths, even in a native Windows R
  # session -- this is exactly the "Windows-style forward-slash absolute
  # path" form described in the task, and the same form list.files()/
  # fs::path() output takes. On POSIX hosts this is simply a POSIX-style
  # absolute path. Either way, this exercises the default (outputDir = NULL)
  # branch, where the output directory is derived from inputfile itself via
  # fs::path_dir() -- the one piece test-createNewFilename.R's other tests
  # above don't reach, because they all override outputDir.
  expect_false(grepl("\\\\", base_dir, fixed = TRUE))

  inputfile <- fs::path(base_dir, "baz.tsv")

  result <- create_new_filename(inputfile, "_desc-preproc", ".tsv")

  expect_equal(
    fs::path(result),
    fs::path(base_dir, "derivatives", "eyeQuality-v1", "baz_desc-preproc.tsv")
  )
  expect_true(fs::dir_exists(fs::path(base_dir, "derivatives", "eyeQuality-v1")))
})

test_that("create_new_filename() with default outputDir derives the correct output directory from a raw backslash absolute path (Windows only)", {
  # A literal backslash-separated Windows path is only ever a real, existing
  # filesystem location when actually running on Windows (a POSIX host has
  # no drive letters or backslash-separated filesystem to speak of), so we
  # only assert *correctness* here when running natively on Windows -- the
  # one platform where this input is both reachable and meaningful -- and
  # skip elsewhere rather than asserting the broken POSIX fallback
  # (fs::path_dir() resolving to ".") as though it were correct behavior.
  #
  # BUG FINDING (not fixed here -- Phase 2 is test-only): on POSIX hosts,
  # create_new_filename(inputfile = "C:\\Users\\foo\\bar\\baz.tsv") silently
  # resolves its default output directory to "./derivatives/eyeQuality-v1"
  # (relative to the current working directory) instead of erroring or
  # resolving the intended location, because fs::path_dir() does not treat
  # "\\" as a separator on POSIX. Confirmed directly against both a native
  # Windows R 4.2.1 session and a POSIX R 3.6.3 session. Low practical
  # impact for real usage (a "C:\\..." path corresponds to a real file only
  # when the code is actually running on Windows in the first place, so the
  # mismatched-OS scenario mostly can't arise for a genuine data file), but
  # a silent wrong-directory write instead of a loud error is still worth
  # flagging.
  skip_if_not(
    .Platform$OS.type == "windows",
    "raw backslash-path directory derivation is only correct (and only reachable with a real file) on native Windows -- see BUG FINDING comment above"
  )

  base_dir <- tempfile("p210_back_base_")
  dir.create(base_dir)
  on.exit(unlink(base_dir, recursive = TRUE), add = TRUE)

  backslash_base_dir <- gsub("/", "\\\\", base_dir)
  inputfile <- paste0(backslash_base_dir, "\\baz.tsv")

  result <- create_new_filename(inputfile, "_desc-preproc", ".tsv")

  expect_equal(
    fs::path(result),
    fs::path(base_dir, "derivatives", "eyeQuality-v1", "baz_desc-preproc.tsv")
  )
  expect_true(fs::dir_exists(fs::path(base_dir, "derivatives", "eyeQuality-v1")))
})
