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

# P7-06: zero-match calls in either layout mode attach a richer
# attr(result, "diagnostics") list (n_* counts, example names, and a
# human-readable "hint" string) so a caller with no console to read the
# print()/message() diagnostics from (e.g. a Shiny app) can still explain
# *why* nothing matched. Only attached when the result is empty, so it's
# purely additive and doesn't change attr(result, "skipped") or the shape of
# a successful, non-empty result (see the "shape is unaffected" test above).

test_that("listBidsFiles bids mode attaches diagnostics when subfolders exist but none match subjectPattern_regex", {
  base_dir <- tempfile("p706_diag_subject_")
  dir.create(base_dir)
  on.exit(unlink(base_dir, recursive = TRUE), add = TRUE)

  # non-BIDS subject folder naming (plain numeric IDs), reproducing the
  # real-world report this diagnostic exists for
  for (id in c("1001", "1002")) {
    d <- file.path(base_dir, id)
    dir.create(d)
    file.create(file.path(d, paste0(id, "_et.tsv")))
  }

  out <- capture.output(result <- listBidsFiles(base_dir))
  diag <- attr(result, "diagnostics")

  expect_length(result, 0)
  expect_false(is.null(diag))
  expect_equal(diag$n_subfolders_found, 2)
  expect_equal(diag$n_subfolders_matched, 0)
  expect_setequal(diag$example_subfolders, c("1001", "1002"))
  expect_match(diag$hint, "subjectPattern_regex", fixed = TRUE)
  expect_match(diag$hint, "recursiveSearch", fixed = TRUE)
  expect_match(diag$hint, "glob", fixed = TRUE)
})

# P7-06 follow-up: manual QA against real IBIS-EP data clarified that its
# subject folders ARE properly BIDS-named ("sub-XX/ses-XX") -- the real
# structure is one or more separate BIDS roots (each e.g.
# <site>/<task>/AUDIT_PASSED/sub-XX/ses-XX/...) nested underneath
# non-BIDS-named wrapper folders (dataset/site/task/audit-status). Pointed
# directly at a single site's BIDS root, "bids" mode's defaults already work
# with zero configuration (no bug there). Pointed at the wrapper root
# spanning multiple sites, "bids" mode's fixed subject/session depth cannot
# see the deeply-nested "sub-XX" folders directly -- these tests confirm the
# two documented workarounds (blank patterns + recursiveSearch = TRUE, and
# layout = "glob" with a "**" pathPattern) both correctly reach files at
# arbitrary depth through those wrapper folders.

build_nested_bids_roots_tree <- function() {
  root <- tempfile("p706_nested_bids_roots_")
  sites <- list(c("Texas", "02_Dancing_Ladies"), c("Missouri", "05_Singing_Birds"))
  for (site in sites) {
    audit_dir <- file.path(root, "IBIS-EP_DATA", site[1], site[2], "AUDIT_PASSED")
    for (sub in c("sub-01", "sub-02")) {
      session_dir <- file.path(audit_dir, sub, "ses-01")
      dir.create(session_dir, recursive = TRUE)
      file.create(file.path(session_dir, paste0(sub, "_ses-01_et.tsv")))
    }
  }
  root
}

test_that("listBidsFiles bids mode with defaults works with zero configuration when pointed directly at a single properly-BIDS-named root", {
  root <- build_nested_bids_roots_tree()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  single_site_root <- file.path(root, "IBIS-EP_DATA", "Texas", "02_Dancing_Ladies", "AUDIT_PASSED")

  result <- listBidsFiles(single_site_root)

  expect_length(result, 2)
  expect_null(attr(result, "diagnostics"))
})

test_that("listBidsFiles bids mode with blank patterns + recursiveSearch = TRUE finds BIDS-named files nested under multiple non-BIDS wrapper levels", {
  root <- build_nested_bids_roots_tree()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  multi_site_root <- file.path(root, "IBIS-EP_DATA")

  # blank patterns alone are not enough at this depth (only one wrapper
  # level -- the site folder -- gets searched, non-recursively)
  result_no_recursive <- listBidsFiles(
    multi_site_root,
    subjectPattern_regex = NULL,
    sessionPattern_regex = NULL,
    recursiveSearch = FALSE
  )
  expect_length(result_no_recursive, 0)

  result_recursive <- listBidsFiles(
    multi_site_root,
    subjectPattern_regex = NULL,
    sessionPattern_regex = NULL,
    recursiveSearch = TRUE
  )
  expect_length(result_recursive, 4)
})

test_that("listBidsFiles layout = 'glob' with a '**' pathPattern finds BIDS-named files nested under multiple non-BIDS wrapper levels", {
  root <- build_nested_bids_roots_tree()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  multi_site_root <- file.path(root, "IBIS-EP_DATA")

  result <- listBidsFiles(multi_site_root, layout = "glob", pathPattern = "**/*.tsv")

  expect_length(result, 4)
})

test_that("listBidsFiles bids mode attaches diagnostics when a subject matches but no session subfolder matches sessionPattern_regex", {
  base_dir <- tempfile("p706_diag_session_")
  dir.create(base_dir)
  on.exit(unlink(base_dir, recursive = TRUE), add = TRUE)

  visit_dir <- file.path(base_dir, "sub-01", "2024-01-15_visit")
  dir.create(visit_dir, recursive = TRUE)
  file.create(file.path(visit_dir, "sub-01_et.tsv"))

  out <- capture.output(result <- listBidsFiles(base_dir))
  diag <- attr(result, "diagnostics")

  expect_length(result, 0)
  expect_equal(diag$n_subfolders_found, 1)
  expect_equal(diag$n_subfolders_matched, 1)
  expect_equal(diag$n_directories_searched, 0)
  expect_match(diag$hint, "sessionPattern_regex", fixed = TRUE)
})

test_that("listBidsFiles bids mode attaches diagnostics when subject/session dirs are searched but contain no matching files", {
  base_dir <- tempfile("p706_diag_recursive_")
  dir.create(base_dir)
  on.exit(unlink(base_dir, recursive = TRUE), add = TRUE)

  # subject and session dirs match, but the file sits one level deeper
  # (needs recursiveSearch = TRUE or layout = "glob" to find it)
  nested_dir <- file.path(base_dir, "sub-01", "ses-01", "extra")
  dir.create(nested_dir, recursive = TRUE)
  file.create(file.path(nested_dir, "sub-01_ses-01_et.tsv"))

  out <- capture.output(result <- listBidsFiles(base_dir))
  diag <- attr(result, "diagnostics")

  expect_length(result, 0)
  expect_equal(diag$n_subfolders_found, 1)
  expect_equal(diag$n_subfolders_matched, 1)
  expect_equal(diag$n_directories_searched, 1)
  expect_match(diag$hint, "recursiveSearch", fixed = TRUE)
})

test_that("listBidsFiles bids mode attaches diagnostics when there are no subfolders at all", {
  base_dir <- tempfile("p706_diag_flat_")
  dir.create(base_dir)
  on.exit(unlink(base_dir, recursive = TRUE), add = TRUE)

  out <- capture.output(result <- listBidsFiles(base_dir))
  diag <- attr(result, "diagnostics")

  expect_length(result, 0)
  expect_equal(diag$n_subfolders_found, 0)
  expect_match(diag$hint, "No subfolders found", fixed = TRUE)
})

test_that("listBidsFiles bids mode does not attach diagnostics when files are matched", {
  base_dir <- tempfile("p706_diag_success_")
  dir.create(base_dir)
  on.exit(unlink(base_dir, recursive = TRUE), add = TRUE)
  dir.create(file.path(base_dir, "sub-01", "ses-01"), recursive = TRUE)
  file.create(file.path(base_dir, "sub-01", "ses-01", "sub-01_ses-01_et.tsv"))

  result <- listBidsFiles(base_dir)

  expect_length(result, 1)
  expect_null(attr(result, "diagnostics"))
})

test_that("listBidsFiles glob mode attaches diagnostics when pathPattern matches nothing", {
  base_dir <- tempfile("p706_diag_glob_nomatch_")
  dir.create(base_dir)
  on.exit(unlink(base_dir, recursive = TRUE), add = TRUE)
  file.create(file.path(base_dir, "data.csv"))

  out <- capture.output(
    result <- listBidsFiles(base_dir, layout = "glob", pathPattern = "*.tsv")
  )
  diag <- attr(result, "diagnostics")

  expect_length(result, 0)
  expect_equal(diag$n_files_scanned, 1)
  expect_equal(diag$n_path_pattern_matches, 0)
  expect_match(diag$hint, "pathPattern", fixed = TRUE)
})

test_that("listBidsFiles glob mode attaches diagnostics when pathPattern matches but excludePattern_regex drops everything", {
  base_dir <- tempfile("p706_diag_glob_excluded_")
  dir.create(base_dir)
  on.exit(unlink(base_dir, recursive = TRUE), add = TRUE)
  file.create(file.path(base_dir, "data.tsv"))

  out <- capture.output(
    result <- listBidsFiles(
      base_dir,
      layout = "glob",
      pathPattern = "*.tsv",
      excludePattern_regex = "."
    )
  )
  diag <- attr(result, "diagnostics")

  expect_length(result, 0)
  expect_equal(diag$n_path_pattern_matches, 1)
  expect_match(diag$hint, "excludePattern_regex", fixed = TRUE)
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
