# Regression tests for P1-12 (third scope note, 2026-08-03):
# getFileRunLogName() is an exported function with batchName = NULL as its
# documented default, so it is directly reachable by any caller using the
# plainest default call -- nothing internal to this package currently calls
# it, but that makes it more important to test directly, not less.
#
# Two distinct bugs were fixed in this function:
#   1. Line ~26: `ifelse(is.null(batchName), NULL, paste0(batchName, "_"))`
#      -- base::ifelse() cannot return NULL for a branch, so this threw
#      "replacement has length zero" whenever batchName was left at its NULL
#      default.
#   2. Line ~24 (pre-existing, separate bug folded into the same task): the
#      descriptor string was built with the bare identifier `basename` (R's
#      builtin function) instead of the local variable `base` (computed two
#      lines earlier via `base <- basename(path_ext_remove(filename))`).
#      Coercing a closure to character throws a distinct crash
#      ("cannot coerce type 'closure' to vector of type 'character'"), so
#      getFileRunLogName() would still have failed after fixing bug #1 alone,
#      just later in the same expression -- a test that only checks
#      "does not error" would not distinguish "both bugs fixed" from
#      "bug #1 fixed, bug #2 still broken" unless it also inspects the
#      returned path's contents, which is why the tests below assert on the
#      actual basename of the input file appearing in the result, not just
#      the absence of an error.

test_that("getFileRunLogName() with no batchName argument succeeds and returns a path containing the input file's base name", {
  f <- "/some/bids/dir/sub-01_task-test_recording-eyetracking_physio.tsv"

  result <- NULL
  expect_no_error(result <- getFileRunLogName(f))

  result <- as.character(result)

  # the input file's base name (extension stripped) must appear in the
  # returned path -- this is the direct regression check for bug #2 above:
  # if `basename` (the builtin function) were used instead of the local
  # `base` variable, this would either error outright (closure -> character
  # coercion) or, if it somehow succeeded, would not contain this string.
  expect_true(grepl("sub-01_task-test_recording-eyetracking_physio", result, fixed = TRUE))

  # no leftover "function"/closure-related garbage from the basename/base mixup
  expect_false(grepl("function", result, fixed = TRUE))

  # the run log name itself should be present, with no batch-label infix
  expect_true(grepl("_desc-preproc_runlog2\\.txt$", result))
  expect_false(grepl("_desc-NULL_", result, fixed = TRUE))
  expect_false(grepl("_desc-_", result, fixed = TRUE))

  # expected directory structure: <dir>/derivatives/eyeQuality-v1/<log>
  expect_true(grepl("derivatives/eyeQuality-v1", result, fixed = TRUE))
})

test_that("getFileRunLogName() with batchName = 'x' still works and includes the batch-label infix", {
  f <- "/some/bids/dir/sub-01_task-test_recording-eyetracking_physio.tsv"

  result <- NULL
  expect_no_error(result <- getFileRunLogName(f, batchName = "x"))

  result <- as.character(result)

  expect_true(grepl("sub-01_task-test_recording-eyetracking_physio", result, fixed = TRUE))
  expect_true(grepl("_desc-x_preproc_runlog2\\.txt$", result))
})

test_that("getFileRunLogName() base name extraction strips the input extension, not just appends to it", {
  f <- "/some/bids/dir/sub-02_task-other_physio.tsv"

  result <- as.character(getFileRunLogName(f))

  # the .tsv extension of the input file must not leak into the middle of
  # the returned run-log path
  expect_false(grepl("physio.tsv_desc", result, fixed = TRUE))
  expect_true(grepl("physio_desc-preproc_runlog2\\.txt$", result))
})
