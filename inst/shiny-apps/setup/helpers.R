# Setup app helper functions.
#
# Kept separate from app.R and free of Shiny-specific code (no reactives, no
# input/output objects) so build_dry_run_preview() can be sourced and called
# directly against a real directory outside of a running Shiny session, for
# testing or scripting.

# build_dry_run_preview: run listBidsFiles() against a candidate directory
# and summarize the result the way the Setup app's preview panel needs --
# matched file count, a small sample of matched filenames, and the
# skipped-item count/detail (attr(result, "skipped"), see listBidsFiles()).
#
# directory: path to the top-level data directory to scan
# layout: "bids" or "glob", passed through to listBidsFiles()
# subjectPattern_regex, sessionPattern_regex, recursiveSearch: bids-layout
#   options, passed through to listBidsFiles() when layout == "bids"
# pathPattern, excludePattern_regex: glob-layout options, passed through to
#   listBidsFiles() when layout == "glob"
# modalityPattern_regex: used in either layout mode
# sample_n: how many sample filenames to include in the returned preview
#
# Returns a list: matched_count, matched_files (full paths), sample_files
# (basenames only, length <= sample_n), skipped_count, skipped_items (full
# detail of what was skipped, e.g. skipped directories or excluded paths),
# and diagnostic_message (NULL, or a single human-readable string when
# matched_count == 0 explaining *why* -- e.g. "found N subfolders but none
# matched subjectPattern_regex" -- since the app's UI has no console for a
# user to read listBidsFiles()'s print()/message() diagnostics from; see
# attr(matched, "diagnostics") on listBidsFiles()'s return value).
build_dry_run_preview <- function(directory,
                                  layout = "bids",
                                  subjectPattern_regex = "sub-[A-Z0-9]+",
                                  sessionPattern_regex = "ses-[0-9]+",
                                  modalityPattern_regex = NULL,
                                  recursiveSearch = FALSE,
                                  pathPattern = NULL,
                                  excludePattern_regex = NULL,
                                  sample_n = 10) {
  if (!is.character(directory) || length(directory) != 1 || is.na(directory) || !nzchar(directory)) {
    stop("build_dry_run_preview: 'directory' must be a non-empty single path")
  }
  if (!dir.exists(directory)) {
    stop("build_dry_run_preview: directory does not exist: ", directory)
  }
  if (!layout %in% c("bids", "glob")) {
    stop("build_dry_run_preview: 'layout' must be 'bids' or 'glob'")
  }

  # NOTE (P7-06): the bids-layout options below are built into a single
  # list(...) call, not assembled incrementally via `args$name <- value`.
  # `args$name <- NULL` *removes* that element from the list rather than
  # setting it to NULL, so an incremental build silently drops a NULL
  # subjectPattern_regex/sessionPattern_regex -- meaning do.call() below
  # would fall back to listBidsFiles()'s own non-NULL defaults
  # ("sub-[A-Z0-9]+"/"ses-[0-9]+") instead of actually passing NULL through.
  # That defeats the UI's own "blank = every subfolder" hint text: a user
  # blanking the field would see the preview keep failing with no clue why.
  # A single list(...) call preserves NULL-valued elements correctly.
  args <- if (identical(layout, "bids")) {
    list(
      directory = directory,
      layout = layout,
      modalityPattern_regex = modalityPattern_regex,
      subjectPattern_regex = subjectPattern_regex,
      sessionPattern_regex = sessionPattern_regex,
      recursiveSearch = recursiveSearch
    )
  } else {
    list(
      directory = directory,
      layout = layout,
      modalityPattern_regex = modalityPattern_regex,
      pathPattern = pathPattern,
      excludePattern_regex = excludePattern_regex
    )
  }

  matched <- do.call(eyeQuality::listBidsFiles, args)
  skipped <- attr(matched, "skipped")
  if (is.null(skipped)) {
    skipped <- character(0)
  }
  diagnostics <- attr(matched, "diagnostics")

  list(
    matched_count = length(matched),
    matched_files = as.character(matched),
    sample_files = utils::head(basename(as.character(matched)), sample_n),
    skipped_count = length(skipped),
    skipped_items = as.character(skipped),
    diagnostic_message = if (is.null(diagnostics)) NULL else diagnostics$hint
  )
}
