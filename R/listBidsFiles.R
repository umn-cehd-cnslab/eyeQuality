#' listBidsFiles - get list of all ET like files.
#'
#' @param directory path for data file as .tsv
#' @param subjectPattern_regex regex match pattern for subjects, or NULL if you are not using subject directories in 'directory' path. Only used when `layout = "bids"`.
#' @param sessionPattern_regex regex match pattern for sessions, or NULL if you are not using session directories in 'subject_directory' paths. Only used when `layout = "bids"`.
#' @param modalityPattern_regex regex match pattern for specific modality, or NULL which will default to searching '.tsv' files. Applied against each candidate file's base name, in either `layout` mode.
#' @param recursiveSearch boolean to search file directory recursively. Only used when `layout = "bids"`.
#' @param layout one of `"bids"` (default) or `"glob"`. `"bids"` preserves the
#'   original fixed two-level `sub-XX/ses-XX/...` directory-matching behavior
#'   driven by `subjectPattern_regex`/`sessionPattern_regex`, and is
#'   unaffected by `pathPattern`/`excludePattern_regex` below. `"glob"` is a
#'   more general alternative for directory structures that don't fit a fixed
#'   two-level hierarchy: it matches files anywhere under `directory` using a
#'   glob-style `pathPattern` (arbitrary depth via `**`), optionally dropping
#'   matches via `excludePattern_regex`. See Details.
#' @param pathPattern glob-style pattern (using `/` as the path separator,
#'   regardless of OS) describing which files under `directory` to include.
#'   Required when `layout = "glob"`; ignored otherwise. `*` matches any
#'   characters within one path segment (not `/`), `?` matches a single
#'   character within one segment, and `**` matches zero or more path
#'   segments (i.e. arbitrary depth, including none). Matched against the
#'   file path relative to `directory`. Examples: `"*.tsv"` (files directly
#'   in `directory`), `"*/*.tsv"` (exactly one directory level deep, e.g. a
#'   flat per-participant layout with no session subfolder), `"*/*/*.tsv"`
#'   (exactly two levels deep), `"**/*.tsv"` (any depth).
#' @param excludePattern_regex optional regex applied to each `pathPattern`
#'   match's path (relative to `directory`) to drop from the result. Only
#'   used when `layout = "glob"`. Excluded paths are reported via
#'   `attr(result, "skipped")` rather than silently dropped. Useful for
#'   layouts where a participant's derivative/processed output lives inside
#'   their own raw-data folder (e.g. `excludePattern_regex = "derivatives"`)
#'   or where a centralized output directory happens to sit inside
#'   `directory` alongside per-participant raw folders.
#' @param ... additional parameters that may get passed from wrapper functions
#'
#' @details
#' `listBidsFiles()` supports two `layout` modes, chosen to cover the
#' directory structures this package's users have in practice:
#'
#' - **BIDS-like** (`layout = "bids"`, the default): a fixed two-level
#'   `sub-XX/ses-XX/...` hierarchy, matched via `subjectPattern_regex`/
#'   `sessionPattern_regex`. This is the original behavior and is unchanged.
#' - **Paired-in-place** (participant folders holding both raw and
#'   processed/derivative data together, rather than a separate
#'   `derivatives/` subtree) and **centralized-output** (all derivative
#'   files across every participant collected into one shared directory,
#'   separate from per-participant raw folders) layouts are both supported
#'   via `layout = "glob"`, since neither fits the fixed two-level
#'   subject/session assumption in general. For paired-in-place data, set
#'   `pathPattern` to match raw files at whatever depth they sit (e.g.
#'   `"*/*.tsv"` for one level, `"**/*.tsv"` for arbitrary depth) and use
#'   `excludePattern_regex` (e.g. `"derivatives"`) to skip each
#'   participant's own processed-output subfolder so it isn't re-ingested as
#'   raw data. For centralized-output data, point `directory` at the raw-data
#'   root and set `pathPattern` to the raw-file layout under it; if the
#'   shared output directory happens to live inside that same root,
#'   `excludePattern_regex` can exclude it by name.
#'
#' @importFrom stringr str_glue
#'
#' @return list of raw ET data files to in BIDS-like directory, with
#'   `attr(result, "skipped")` set to any directories (`"bids"` mode) or
#'   paths (`"glob"` mode) that were found but excluded, and, when zero files
#'   were matched, `attr(result, "diagnostics")` set to a list (`hint` plus
#'   supporting counts) explaining why -- e.g. that subfolders exist but none
#'   matched `subjectPattern_regex`, versus no subfolders existing at all.
#' @export
#'
listBidsFiles <-
  function(directory,
           subjectPattern_regex = "sub-[A-Z0-9]+",
           sessionPattern_regex = "ses-[0-9]+",
           modalityPattern_regex = NULL,
           recursiveSearch = FALSE,
           layout = c("bids", "glob"),
           pathPattern = NULL,
           excludePattern_regex = NULL,
           ...) {
    layout <- match.arg(layout)

    if (layout == "glob") {
      return(
        listBidsFiles_glob(
          directory = directory,
          pathPattern = pathPattern,
          modalityPattern_regex = modalityPattern_regex,
          excludePattern_regex = excludePattern_regex
        )
      )
    }

    # layout == "bids": original fixed two-level regex-based behavior,
    # preserved exactly for zero-regression against existing callers.

    # check to see if there are any nested folders?
    subject_dirs <-
      list.dirs(directory, full.names = TRUE, recursive = FALSE)

    # initialize list of files to append as we search directory
    files <- list()
    directories_for_files <- list()
    skipped_dirs <- character(0)
    # Tracked separately from skipped_dirs (which stays subject- and
    # session-level skips combined, for attr(result, "skipped")
    # back-compat) so a zero-match diagnostic below can tell "0 subject
    # directories matched subjectPattern_regex" apart from "subjects
    # matched fine, but every session subfolder was skipped" -- these need
    # different hints (P7-06).
    n_subject_dirs_matched <- 0L

    # Next we find the list the directories to check.
    if (identical(subject_dirs, character(0))) {
      # If there are no subject directories,
      # we assume BIDS files are all in one directory
      # which is the directory specified in the function call
      directories_for_files <- c(directories_for_files, directory)
    } else {
      # IF subject_dirs has subdirectories, we loop through those.
      for (subject_dir in subject_dirs) {
        if (is.null(subjectPattern_regex)) {
          # if the subjectPattern_regex was specified as NULL
          # we assume that we will go through every subfolder in directory for files
          n_subject_dirs_matched <- n_subject_dirs_matched + 1L
          directories_for_files <-
            c(directories_for_files, subject_dir)
        } else if (grepl(subjectPattern_regex, subject_dir)) {
          n_subject_dirs_matched <- n_subject_dirs_matched + 1L
          # if subjectPattern_regex was specified
          # we only check the subfolders that match subjectPattern_regex

          if (is.null(sessionPattern_regex)) {
            # if sessionPattern_regex was specified as NULL
            # we assume one session, and will search subject_dir for files
            directories_for_files <-
              c(directories_for_files, subject_dir)
          } else {
            # pull subdirectories in subject_dir
            session_dirs <-
              list.dirs(subject_dir,
                full.names = TRUE,
                recursive = FALSE
              )
            # loop through session directories to match
            for (session_dir in session_dirs) {
              # if the session_dir matches sessionPattern_regex
              # we add that session_dir to check for files
              if (grepl(sessionPattern_regex, session_dir)) {
                directories_for_files <- c(directories_for_files, session_dir)
              } else {
                print(
                  stringr::str_glue(
                    "Subdirectory name doesn't match sessionPattern_regex. Skipping {session_dir}"
                  )
                )
                skipped_dirs <- c(skipped_dirs, session_dir)
                next
              }
            } # end for loop for session_dirs
          } # end else where sessionPattern_regex defined
        } else {
          # Otherwise, a subdirectory does not match subjectPattern_regex
          print(
            stringr::str_glue(
              "Directory name doesn't match subjectPattern_regex. Skipping {subject_dir}"
            )
          )
          skipped_dirs <- c(skipped_dirs, subject_dir)
          next
        }
      } # end for loop for subject_dirs
    }

    # now we have our list of directories to check for files
    for (filedir in directories_for_files) {
      if (is.null(modalityPattern_regex)) {
        tsv_files <- list.files(
          filedir,
          pattern = "\\.tsv$",
          recursive = recursiveSearch,
          full.names = TRUE
        )
        files <- c(files, tsv_files)
      } else {
        tsv_files <- list.files(
          filedir,
          pattern = modalityPattern_regex,
          recursive = recursiveSearch,
          full.names = TRUE
        )
        files <- c(files, tsv_files)
      }
    }

    result <- unlist(files)
    if (is.null(result)) {
      result <- character(0)
    }
    attr(result, "skipped") <- skipped_dirs

    if (length(files) == 0) {
      print("WARNING: No files found. If you expect to have data files in your directory, please check your directory structure.")
      print("Confirm subjectPattern_regex and session_Pattern_regex correctly find file(s) in your subject/session directories.")
      print("Otherwise specify a modalityPattern_regex with the naming convention for your eyetracking files.")
      print("If you have 'subject_dir/session_dir/additional_directory/data_files.tsv' structure, specify recursiveSearch = TRUE.")

      attr(result, "diagnostics") <- bids_zero_match_diagnostics(
        directory = directory,
        subject_dirs = subject_dirs,
        n_subject_dirs_matched = n_subject_dirs_matched,
        n_directories_searched = length(directories_for_files),
        subjectPattern_regex = subjectPattern_regex,
        sessionPattern_regex = sessionPattern_regex
      )

      return(result)
    } else {
      return(result)
    }
  }

#' bids_zero_match_diagnostics - build a human-actionable explanation for why
#' `layout = "bids"` matched zero files
#'
#' Internal helper for `listBidsFiles()`. Distinguishes the distinct
#' zero-match scenarios (P7-06) -- no subfolders at all, subfolders present
#' but none matched `subjectPattern_regex`, subjects matched but no session
#' subfolder matched `sessionPattern_regex`, or directories were searched but
#' contained no matching files -- since each needs a different fix, and a
#' bare "0 files found" is not actionable from a GUI that has no console to
#' read `print()`/`message()` output from.
#'
#' @param directory the directory `listBidsFiles()` was called on
#' @param subject_dirs full result of `list.dirs(directory, recursive = FALSE)`
#' @param n_subject_dirs_matched count of `subject_dirs` treated as usable
#'   subject directories (either `subjectPattern_regex` matched, or it was NULL)
#' @param n_directories_searched `length(directories_for_files)` -- how many
#'   directories were actually handed to `list.files()`
#' @param subjectPattern_regex,sessionPattern_regex as passed to `listBidsFiles()`
#'
#' @return a list with `n_subfolders_found`, `n_subfolders_matched`,
#'   `example_subfolders` (up to 5 basenames), `n_directories_searched`, and
#'   `hint` (a single human-readable string)
#' @keywords internal
#' @noRd
bids_zero_match_diagnostics <- function(directory,
                                         subject_dirs,
                                         n_subject_dirs_matched,
                                         n_directories_searched,
                                         subjectPattern_regex,
                                         sessionPattern_regex) {
  n_subfolders_found <- length(subject_dirs)
  example_subfolders <- utils::head(basename(subject_dirs), 5)

  hint <- if (n_subfolders_found == 0) {
    stringr::str_glue(
      "No subfolders found directly under '{directory}'; searched it directly ",
      "for matching files, and found none."
    )
  } else if (n_subject_dirs_matched == 0) {
    stringr::str_glue(
      "Found {n_subfolders_found} subfolder(s) under '{directory}' (e.g. ",
      "{paste(example_subfolders, collapse = ', ')}), but none matched ",
      "subjectPattern_regex ('{subjectPattern_regex}'). If these are wrapper ",
      "folders (e.g. site/task/audit-status) rather than actual subject ",
      "folders, your real subject/session folders may be nested further in: ",
      "try blanking subjectPattern_regex and sessionPattern_regex together ",
      "with recursiveSearch = TRUE (searches every subfolder at any depth), ",
      "or switch to layout = 'glob' with a pathPattern like '**/*.tsv'."
    )
  } else if (n_directories_searched == 0) {
    stringr::str_glue(
      "Found {n_subject_dirs_matched} subject subfolder(s) matching ",
      "subjectPattern_regex, but none of their session subfolders matched ",
      "sessionPattern_regex ('{sessionPattern_regex}'). Try blanking ",
      "sessionPattern_regex or switching to layout = 'glob'."
    )
  } else {
    stringr::str_glue(
      "Searched {n_directories_searched} director(y/ies) under '{directory}' ",
      "but found 0 matching files. If files sit in an additional nested ",
      "subfolder (e.g. a session or visit-date folder), try recursiveSearch ",
      "= TRUE, or switch to layout = 'glob' with a pathPattern like ",
      "'**/*.tsv'."
    )
  }

  list(
    n_subfolders_found = n_subfolders_found,
    n_subfolders_matched = n_subject_dirs_matched,
    example_subfolders = example_subfolders,
    n_directories_searched = n_directories_searched,
    hint = as.character(hint)
  )
}

#' listBidsFiles_glob - arbitrary-depth glob/path-template file matching
#'
#' Internal helper implementing `listBidsFiles(..., layout = "glob")`. Not
#' exported: reachable only via `listBidsFiles()`.
#'
#' @param directory path to search under
#' @param pathPattern glob pattern relative to `directory`; see
#'   `listBidsFiles()` for syntax
#' @param modalityPattern_regex optional regex applied to each match's base name
#' @param excludePattern_regex optional regex applied to each match's path
#'   (relative to `directory`) to drop from the result
#'
#' @return character vector of matched file paths, with `attr(result, "skipped")`
#'   set to the paths that matched `pathPattern` but were dropped by
#'   `excludePattern_regex`
#' @keywords internal
#' @noRd
listBidsFiles_glob <- function(directory,
                                pathPattern,
                                modalityPattern_regex = NULL,
                                excludePattern_regex = NULL) {
  if (is.null(pathPattern) ||
    !is.character(pathPattern) ||
    length(pathPattern) != 1 ||
    is.na(pathPattern) ||
    nchar(pathPattern) == 0) {
    stop(
      'listBidsFiles: \'pathPattern\' must be a non-empty glob string when layout = "glob" ',
      '(e.g. "*/*.tsv" for one directory level, "**/*.tsv" for arbitrary depth).'
    )
  }

  all_files_rel <- list.files(directory, recursive = TRUE, full.names = FALSE)
  # list.files() always separates path components with "/" regardless of OS,
  # but normalize defensively before matching against the glob-derived regex.
  all_files_rel_norm <- gsub("\\\\", "/", all_files_rel)

  pattern_regex <- globToRegex(pathPattern)
  path_matches <- grepl(paste0("^", pattern_regex, "$"), all_files_rel_norm, perl = TRUE)
  candidate_rel <- all_files_rel[path_matches]

  if (!is.null(modalityPattern_regex)) {
    candidate_rel <- candidate_rel[grepl(modalityPattern_regex, basename(candidate_rel))]
  }

  candidate_rel_norm <- gsub("\\\\", "/", candidate_rel)

  if (!is.null(excludePattern_regex)) {
    excluded_mask <- grepl(excludePattern_regex, candidate_rel_norm)
  } else {
    excluded_mask <- rep(FALSE, length(candidate_rel))
  }

  result <- file.path(directory, candidate_rel[!excluded_mask])
  skipped <- file.path(directory, candidate_rel[excluded_mask])

  if (length(result) == 0) {
    result <- character(0)
  }
  attr(result, "skipped") <- skipped

  if (length(result) == 0) {
    print("WARNING: No files found. If you expect to have data files in your directory, please check your directory structure.")
    print("Confirm pathPattern matches file(s) at the depth they actually sit under 'directory' (use '**' for arbitrary depth).")
    print("If files were matched but dropped, check excludePattern_regex - dropped paths are reported in attr(result, 'skipped').")

    attr(result, "diagnostics") <- glob_zero_match_diagnostics(
      directory = directory,
      pathPattern = pathPattern,
      all_files_rel = all_files_rel,
      n_path_matches = sum(path_matches),
      n_skipped = length(skipped)
    )
  }

  result
}

#' glob_zero_match_diagnostics - build a human-actionable explanation for why
#' `layout = "glob"` matched zero files
#'
#' Internal helper for `listBidsFiles_glob()`. See `bids_zero_match_diagnostics()`
#' for the "bids" mode counterpart and the rationale (P7-06).
#'
#' @param directory the directory `listBidsFiles()` was called on
#' @param pathPattern the glob pattern that was applied
#' @param all_files_rel every file found anywhere under `directory`
#'   (`list.files(directory, recursive = TRUE)`), before `pathPattern` filtering
#' @param n_path_matches how many of `all_files_rel` matched `pathPattern`
#'   (before `modalityPattern_regex`/`excludePattern_regex` filtering)
#' @param n_skipped how many `pathPattern` matches were dropped by
#'   `excludePattern_regex`
#'
#' @return a list with `n_files_scanned`, `n_path_pattern_matches`,
#'   `example_files_found` (up to 5 relative paths), and `hint`
#' @keywords internal
#' @noRd
glob_zero_match_diagnostics <- function(directory,
                                         pathPattern,
                                         all_files_rel,
                                         n_path_matches,
                                         n_skipped) {
  n_files_scanned <- length(all_files_rel)
  example_files <- utils::head(all_files_rel, 5)

  hint <- if (n_files_scanned == 0) {
    stringr::str_glue(
      "No files at all were found under '{directory}' (searched recursively)."
    )
  } else if (n_path_matches == 0) {
    stringr::str_glue(
      "Found {n_files_scanned} file(s) under '{directory}' (e.g. ",
      "{paste(example_files, collapse = ', ')}), but none matched pathPattern ",
      "('{pathPattern}'). Check the depth ('*' is one directory level, '**' is ",
      "any depth) and file extension in the pattern."
    )
  } else if (n_skipped == n_path_matches) {
    stringr::str_glue(
      "pathPattern matched {n_path_matches} file(s), but excludePattern_regex ",
      "dropped all of them -- see attr(result, 'skipped') for what was excluded."
    )
  } else {
    stringr::str_glue(
      "pathPattern matched {n_path_matches} file(s), but modalityPattern_regex ",
      "excluded all of them."
    )
  }

  list(
    n_files_scanned = n_files_scanned,
    n_path_pattern_matches = n_path_matches,
    example_files_found = example_files,
    hint = as.character(hint)
  )
}

#' globToRegex - translate a restricted glob syntax to a POSIX/PCRE regex
#'
#' Supports `*` (any characters within one path segment), `?` (single
#' character within one path segment), and `**` (zero or more path segments,
#' i.e. arbitrary depth including none - so `"**/*.tsv"` matches both
#' `"raw.tsv"` and `"a/b/raw.tsv"`). All other characters are treated
#' literally (regex metacharacters are escaped). Path separators must be `/`.
#'
#' @param glob glob-style pattern string
#' @return character regex string (intended for use with `grepl(..., perl = TRUE)`)
#' @keywords internal
#' @noRd
globToRegex <- function(glob) {
  special_chars <- c(".", "(", ")", "^", "$", "+", "{", "}", "[", "]", "|")
  for (ch in special_chars) {
    glob <- gsub(ch, paste0("\\", ch), glob, fixed = TRUE)
  }

  placeholder_zero_or_more_dirs <- "ZERO_OR_MORE_DIRS"
  placeholder_anything <- "ANYTHING"

  # "**/" (zero or more whole path segments) must be substituted before
  # bare "*" so it isn't consumed by the single-segment-wildcard rule below.
  glob <- gsub("**/", placeholder_zero_or_more_dirs, glob, fixed = TRUE)
  glob <- gsub("**", placeholder_anything, glob, fixed = TRUE)
  glob <- gsub("*", "[^/]*", glob, fixed = TRUE)
  glob <- gsub("?", "[^/]", glob, fixed = TRUE)

  glob <- gsub(placeholder_zero_or_more_dirs, "(?:.*/)?", glob, fixed = TRUE)
  glob <- gsub(placeholder_anything, ".*", glob, fixed = TRUE)

  glob
}
