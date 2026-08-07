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

# P7-01: listBidsFiles() gains an opt-in layout = c("bids", "glob") argument.
# layout = "bids" preserves the original fixed two-level sub-XX/ses-XX
# regex-driven behavior exactly (own code branch, unchanged), while
# layout = "glob" adds a depth-aware glob pathPattern + excludePattern_regex
# path for directory structures that don't fit that fixed hierarchy. These
# tests cover the coverage gaps flagged when that mode was implemented.

test_that("listBidsFiles with layout = 'bids' explicit matches the layout-omitted default", {
  base_dir <- tempfile("p701_bids_default_")
  dir.create(base_dir)
  on.exit(unlink(base_dir, recursive = TRUE), add = TRUE)

  session_dir <- file.path(base_dir, "sub-01", "ses-01")
  dir.create(session_dir, recursive = TRUE)
  file.create(file.path(session_dir, "sub-01_ses-01_recording-eyetracking.tsv"))
  dir.create(file.path(base_dir, "notasubject"))

  result_default <- listBidsFiles(base_dir)
  result_explicit <- listBidsFiles(base_dir, layout = "bids")

  expect_identical(as.character(result_default), as.character(result_explicit))
  expect_identical(attr(result_default, "skipped"), attr(result_explicit, "skipped"))
})

test_that("listBidsFiles with layout = 'glob' and pathPattern = '**/*.tsv' finds files at depths other than 2", {
  base_dir <- tempfile("p701_glob_depths_")
  dir.create(base_dir)
  on.exit(unlink(base_dir, recursive = TRUE), add = TRUE)

  # 1 level deep
  dir.create(file.path(base_dir, "subA"))
  one_level_file <- file.path(base_dir, "subA", "file1.tsv")
  file.create(one_level_file)

  # 4 levels deep
  four_level_dir <- file.path(base_dir, "a", "b", "c", "d")
  dir.create(four_level_dir, recursive = TRUE)
  four_level_file <- file.path(four_level_dir, "file4.tsv")
  file.create(four_level_file)

  result <- listBidsFiles(base_dir, layout = "glob", pathPattern = "**/*.tsv")

  expect_setequal(
    normalizePath(result),
    normalizePath(c(one_level_file, four_level_file))
  )
})

test_that("listBidsFiles with layout = 'glob' and excludePattern_regex drops paired-in-place derivatives files into attr(result, 'skipped')", {
  base_dir <- tempfile("p701_glob_paired_")
  dir.create(base_dir)
  on.exit(unlink(base_dir, recursive = TRUE), add = TRUE)

  participant_dir <- file.path(base_dir, "sub-01")
  dir.create(participant_dir)
  raw_file <- file.path(participant_dir, "raw.tsv")
  file.create(raw_file)

  derivatives_dir <- file.path(participant_dir, "derivatives")
  dir.create(derivatives_dir)
  derivative_file <- file.path(derivatives_dir, "processed.tsv")
  file.create(derivative_file)

  result <- listBidsFiles(
    base_dir,
    layout = "glob",
    pathPattern = "**/*.tsv",
    excludePattern_regex = "derivatives"
  )

  expect_equal(normalizePath(result), normalizePath(raw_file))
  expect_equal(normalizePath(attr(result, "skipped")), normalizePath(derivative_file))
  expect_false(normalizePath(derivative_file) %in% normalizePath(result))
})

test_that("listBidsFiles with layout = 'glob' ignores a centralized output directory sitting as a sibling of the scanned root", {
  parent_dir <- tempfile("p701_glob_sibling_")
  dir.create(parent_dir)
  on.exit(unlink(parent_dir, recursive = TRUE), add = TRUE)

  raw_root <- file.path(parent_dir, "raw")
  raw_subject_dir <- file.path(raw_root, "sub-01")
  dir.create(raw_subject_dir, recursive = TRUE)
  raw_file <- file.path(raw_subject_dir, "data.tsv")
  file.create(raw_file)

  output_dir <- file.path(parent_dir, "output")
  dir.create(output_dir)
  file.create(file.path(output_dir, "summary.tsv"))

  result <- listBidsFiles(raw_root, layout = "glob", pathPattern = "**/*.tsv")

  expect_equal(normalizePath(result), normalizePath(raw_file))
  expect_identical(attr(result, "skipped"), character(0))
})

test_that("listBidsFiles with layout = 'glob' and excludePattern_regex drops a centralized output directory nested inside the scanned root", {
  root_dir <- tempfile("p701_glob_centralized_")
  dir.create(root_dir)
  on.exit(unlink(root_dir, recursive = TRUE), add = TRUE)

  dir.create(file.path(root_dir, "sub-01"))
  file1 <- file.path(root_dir, "sub-01", "data1.tsv")
  file.create(file1)

  dir.create(file.path(root_dir, "sub-02"))
  file2 <- file.path(root_dir, "sub-02", "data2.tsv")
  file.create(file2)

  dir.create(file.path(root_dir, "output"))
  output_file <- file.path(root_dir, "output", "summary.tsv")
  file.create(output_file)

  result <- listBidsFiles(
    root_dir,
    layout = "glob",
    pathPattern = "**/*.tsv",
    excludePattern_regex = "^output/"
  )

  expect_setequal(normalizePath(result), normalizePath(c(file1, file2)))
  expect_equal(normalizePath(attr(result, "skipped")), normalizePath(output_file))
})

test_that("listBidsFiles with layout = 'glob' pathPattern = '*/*.tsv' is depth-strict and does not match files 2+ levels deep", {
  base_dir <- tempfile("p701_glob_depth_strict_")
  dir.create(base_dir)
  on.exit(unlink(base_dir, recursive = TRUE), add = TRUE)

  # exactly one level deep -- should match
  dir.create(file.path(base_dir, "subA"))
  one_level_file <- file.path(base_dir, "subA", "file1.tsv")
  file.create(one_level_file)

  # exactly two levels deep -- should NOT match "*/*.tsv"
  two_level_dir <- file.path(base_dir, "subA", "subB")
  dir.create(two_level_dir)
  two_level_file <- file.path(two_level_dir, "file2.tsv")
  file.create(two_level_file)

  result <- listBidsFiles(base_dir, layout = "glob", pathPattern = "*/*.tsv")

  expect_equal(normalizePath(result), normalizePath(one_level_file))
  expect_false(normalizePath(two_level_file) %in% normalizePath(result))
})

test_that("listBidsFiles with layout = 'glob' applies modalityPattern_regex within the glob matches", {
  base_dir <- tempfile("p701_glob_modality_")
  dir.create(base_dir)
  on.exit(unlink(base_dir, recursive = TRUE), add = TRUE)

  participant_dir <- file.path(base_dir, "sub-01")
  dir.create(participant_dir)
  eyetracking_file <- file.path(participant_dir, "eyetracking.tsv")
  file.create(eyetracking_file)
  events_file <- file.path(participant_dir, "events.tsv")
  file.create(events_file)

  result <- listBidsFiles(
    base_dir,
    layout = "glob",
    pathPattern = "*/*.tsv",
    modalityPattern_regex = "eyetracking"
  )

  expect_equal(normalizePath(result), normalizePath(eyetracking_file))
})

test_that("listBidsFiles with layout = 'glob' errors when pathPattern is missing, NULL, or empty", {
  base_dir <- tempfile("p701_glob_pattern_errors_")
  dir.create(base_dir)
  on.exit(unlink(base_dir, recursive = TRUE), add = TRUE)
  file.create(file.path(base_dir, "data.tsv"))

  expect_error(listBidsFiles(base_dir, layout = "glob"), "pathPattern")
  expect_error(listBidsFiles(base_dir, layout = "glob", pathPattern = NULL), "pathPattern")
  expect_error(listBidsFiles(base_dir, layout = "glob", pathPattern = ""), "pathPattern")
})

test_that("listBidsFiles errors on an invalid layout value via match.arg instead of silently misbehaving", {
  base_dir <- tempfile("p701_layout_invalid_")
  dir.create(base_dir)
  on.exit(unlink(base_dir, recursive = TRUE), add = TRUE)

  expect_error(listBidsFiles(base_dir, layout = "not-a-real-layout"))
})

test_that("listBidsFiles with layout = 'glob' returns character(0) with a non-NULL skipped attribute when pathPattern matches nothing", {
  base_dir <- tempfile("p701_glob_empty_")
  dir.create(base_dir)
  on.exit(unlink(base_dir, recursive = TRUE), add = TRUE)
  file.create(file.path(base_dir, "data.csv"))

  out <- capture.output(
    result <- listBidsFiles(base_dir, layout = "glob", pathPattern = "*.tsv")
  )

  expect_length(result, 0)
  expect_type(result, "character")
  expect_false(is.null(attr(result, "skipped")))
  expect_identical(attr(result, "skipped"), character(0))
})

test_that("listBidsFiles with layout = 'glob' treats regex metacharacters in pathPattern literally, not as regex", {
  base_dir <- tempfile("p701_glob_literal_chars_")
  dir.create(base_dir)
  on.exit(unlink(base_dir, recursive = TRUE), add = TRUE)

  # a literal "." in the pattern must not act as regex "any character": a
  # file whose name substitutes some other character for that position
  # should not match.
  literal_dot_file <- file.path(base_dir, "data.tsv")
  file.create(literal_dot_file)
  decoy_file <- file.path(base_dir, "dataXtsv")
  file.create(decoy_file)

  result_dot <- listBidsFiles(base_dir, layout = "glob", pathPattern = "data.tsv")
  expect_equal(normalizePath(result_dot), normalizePath(literal_dot_file))

  # literal parentheses must also be matched literally, not as a regex group
  paren_dir <- tempfile("p701_glob_literal_parens_")
  dir.create(paren_dir)
  on.exit(unlink(paren_dir, recursive = TRUE), add = TRUE)
  paren_file <- file.path(paren_dir, "data(1).tsv")
  file.create(paren_file)

  result_paren <- listBidsFiles(paren_dir, layout = "glob", pathPattern = "data(1).tsv")
  expect_equal(normalizePath(result_paren), normalizePath(paren_file))
})

test_that("globToRegex() escapes regex metacharacters while preserving glob wildcard semantics", {
  # literal "." is escaped, so it only matches a literal dot, not any character
  regex_dot <- eyeQuality:::globToRegex("data.tsv")
  expect_true(grepl(paste0("^", regex_dot, "$"), "data.tsv", perl = TRUE))
  expect_false(grepl(paste0("^", regex_dot, "$"), "dataXtsv", perl = TRUE))

  # literal parentheses are escaped, not treated as a capture group
  regex_paren <- eyeQuality:::globToRegex("data(1).tsv")
  expect_true(grepl(paste0("^", regex_paren, "$"), "data(1).tsv", perl = TRUE))
  expect_false(grepl(paste0("^", regex_paren, "$"), "data1.tsv", perl = TRUE))

  # "*" still matches within-segment wildcards, and does not cross "/"
  regex_star <- eyeQuality:::globToRegex("*/*.tsv")
  expect_true(grepl(paste0("^", regex_star, "$"), "sub-01/data.tsv", perl = TRUE))
  expect_false(grepl(paste0("^", regex_star, "$"), "sub-01/ses-01/data.tsv", perl = TRUE))

  # "**" matches zero or more whole path segments (arbitrary depth)
  regex_doublestar <- eyeQuality:::globToRegex("**/*.tsv")
  expect_true(grepl(paste0("^", regex_doublestar, "$"), "data.tsv", perl = TRUE))
  expect_true(grepl(paste0("^", regex_doublestar, "$"), "a/b/c/d/data.tsv", perl = TRUE))
})
