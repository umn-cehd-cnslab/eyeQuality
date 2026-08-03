# P1-08: listBidsFiles() previously only reported skipped, non-matching
# subject-/session-level directories via print() - invisible to any
# programmatic caller. Skipped paths are now also accumulated and attached
# to the return value as attr(result, "skipped"), without changing the
# return type (still a plain character vector of file paths).

test_that("listBidsFiles reports a non-matching subject directory in attr(result, 'skipped') without dropping the valid file", {
  base_dir <- tempfile("p108_subject_skip_")
  dir.create(base_dir)
  on.exit(unlink(base_dir, recursive = TRUE), add = TRUE)

  # valid sub-XX/ses-XX/ structure containing a real file
  valid_session_dir <- file.path(base_dir, "sub-01", "ses-01")
  dir.create(valid_session_dir, recursive = TRUE)
  valid_file <- file.path(valid_session_dir, "sub-01_ses-01_recording-eyetracking.tsv")
  file.create(valid_file)

  # sibling directory that does NOT match subjectPattern_regex ("sub-[A-Z0-9]+")
  bad_subject_dir <- file.path(base_dir, "notasubject")
  dir.create(bad_subject_dir)

  result <- listBidsFiles(base_dir)

  expect_equal(normalizePath(result), normalizePath(valid_file))
  expect_equal(normalizePath(attr(result, "skipped")), normalizePath(bad_subject_dir))
})

test_that("listBidsFiles reports a non-matching session directory in attr(result, 'skipped') without dropping the valid file", {
  base_dir <- tempfile("p108_session_skip_")
  dir.create(base_dir)
  on.exit(unlink(base_dir, recursive = TRUE), add = TRUE)

  subject_dir <- file.path(base_dir, "sub-01")
  dir.create(subject_dir)

  # valid ses-XX/ session containing a real file
  valid_session_dir <- file.path(subject_dir, "ses-01")
  dir.create(valid_session_dir)
  valid_file <- file.path(valid_session_dir, "sub-01_ses-01_recording-eyetracking.tsv")
  file.create(valid_file)

  # sibling session-level directory that does NOT match sessionPattern_regex ("ses-[0-9]+")
  bad_session_dir <- file.path(subject_dir, "notasession")
  dir.create(bad_session_dir)

  result <- listBidsFiles(base_dir)

  expect_equal(normalizePath(result), normalizePath(valid_file))
  expect_equal(normalizePath(attr(result, "skipped")), normalizePath(bad_session_dir))
})

test_that("listBidsFiles returns attr(result, 'skipped') == character(0) when nothing is skipped", {
  base_dir <- tempfile("p108_no_skip_")
  dir.create(base_dir)
  on.exit(unlink(base_dir, recursive = TRUE), add = TRUE)

  valid_session_dir <- file.path(base_dir, "sub-01", "ses-01")
  dir.create(valid_session_dir, recursive = TRUE)
  valid_file <- file.path(valid_session_dir, "sub-01_ses-01_recording-eyetracking.tsv")
  file.create(valid_file)

  result <- listBidsFiles(base_dir)

  expect_equal(normalizePath(result), normalizePath(valid_file))
  expect_identical(attr(result, "skipped"), character(0))
})

test_that("listBidsFiles's returned file vector shape is unaffected by skip-reporting (purely additive attribute)", {
  base_dir <- tempfile("p108_shape_")
  dir.create(base_dir)
  on.exit(unlink(base_dir, recursive = TRUE), add = TRUE)

  # two valid sub-XX/ses-XX/ structures, each with one file, plus one
  # non-matching sibling subject directory to skip
  for (sub in c("sub-01", "sub-02")) {
    session_dir <- file.path(base_dir, sub, "ses-01")
    dir.create(session_dir, recursive = TRUE)
    file.create(file.path(session_dir, paste0(sub, "_ses-01_recording-eyetracking.tsv")))
  }
  dir.create(file.path(base_dir, "not_a_subject_dir"))

  result <- listBidsFiles(base_dir)

  # core file-finding behavior: a plain character vector of length 2,
  # unattributed apart from the new "skipped" attribute
  expect_type(result, "character")
  expect_length(result, 2)
  expect_setequal(
    basename(result),
    c("sub-01_ses-01_recording-eyetracking.tsv", "sub-02_ses-01_recording-eyetracking.tsv")
  )
  expect_equal(names(attributes(result)), "skipped")
  expect_equal(basename(attr(result, "skipped")), "not_a_subject_dir")
})
