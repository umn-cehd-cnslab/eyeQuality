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
#' @return list of raw ET data files to in BIDS-like directory
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
          directories_for_files <-
            c(directories_for_files, subject_dir)
        } else if (grepl(subjectPattern_regex, subject_dir)) {
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

      return(result)
    } else {
      return(result)
    }
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
  }

  result
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
