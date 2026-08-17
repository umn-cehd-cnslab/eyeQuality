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
# detail of what was skipped, e.g. skipped directories or excluded paths).
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

  args <- list(
    directory = directory,
    layout = layout,
    modalityPattern_regex = modalityPattern_regex
  )
  if (identical(layout, "bids")) {
    args$subjectPattern_regex <- subjectPattern_regex
    args$sessionPattern_regex <- sessionPattern_regex
    args$recursiveSearch <- recursiveSearch
  } else {
    args$pathPattern <- pathPattern
    args$excludePattern_regex <- excludePattern_regex
  }

  matched <- do.call(eyeQuality::listBidsFiles, args)
  skipped <- attr(matched, "skipped")
  if (is.null(skipped)) {
    skipped <- character(0)
  }

  list(
    matched_count = length(matched),
    matched_files = as.character(matched),
    sample_files = utils::head(basename(as.character(matched)), sample_n),
    skipped_count = length(skipped),
    skipped_items = as.character(skipped)
  )
}
