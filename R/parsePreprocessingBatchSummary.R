#' parsePreprocessingBatchSummary
#'
#' parse preprocessing_batch_summary files
#'
#' @param batch_summary_file filepath to preprocessing_batch_summary.tsv file
#' @param info_to_extract string with value of "summary" | "failedfiles" | "successfulfiles" | "skippedfiles"
#'
#' @return For `info_to_extract = "summary"`, a one-row dataframe of the batch
#'   run's summary fields. For `"successfulfiles"`/`"skippedfiles"`, a
#'   character vector of filepaths (`character(0)` if the section is empty).
#'   For `"failedfiles"` (P7-04), a `file`/`error` tibble with one row per
#'   failed file - `error` is that file's captured error message (or a
#'   placeholder for a failed file with no error detail captured this run;
#'   see `eyeQualityBatch()`'s "Failure detail" section) - or a zero-row
#'   tibble with those same two columns if nothing failed.
#' @export
#'
#' @importFrom stringr str_glue
#' @importFrom stringr str_extract
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @importFrom tibble tibble
#'
#'
parsePreprocessingBatchSummary <-
  function(batch_summary_file, info_to_extract = "summary") {
    if (info_to_extract == "summary") {
      # windows_safe_read_tsv(): see R/windowsLongPath.R -- readr's own
      # pre-open existence check can report a real, over-~260-char path
      # (deeply nested Box/OneDrive study trees plus this package's own
      # nested derivatives/eyeQuality-v1/ output convention get there
      # easily) as "does not exist" even though file.exists()/list.files()
      # found it, and a bare windows_long_path()-prefixed string does not
      # fix this on its own (see that function's roxygen docs) -- hence the
      # dedicated wrapper rather than just wrapping the path here.
      windows_safe_read_tsv(
        str_glue(batch_summary_file),
        col_names = FALSE,
        show_col_types = FALSE
      ) %>%
        tail(n = 1) %>%
        mutate(
          directory = str_extract(.data[["X1"]], "directory: ([\\S]+),", group = 1),
          datasize = as.numeric(
            str_extract(.data[["X1"]], "data size \\(MB\\): ([\\S]+),", group = 1)
          ),
          nfiles = as.numeric(str_extract(
            .data[["X1"]], "n \\(ET Files\\): ([\\S]+),",
            group = 1
          )),
          nPreprocessed = as.numeric(
            str_extract(.data[["X1"]], "n \\(preprocessed\\): ([\\S]+),", group = 1)
          ),
          nFailed = as.numeric(
            str_extract(.data[["X1"]], "n \\(failed preprocessing\\): ([\\S]+),", group = 1)
          ),
          runDuration = str_extract(.data[["X1"]], "run duration: ([\\S\\s]+),", group = 1),
          runTime = as.numeric(str_extract(
            .data[["X1"]], "runtime \\(s\\): ([\\S]+)$",
            group = 1
          )),
        ) %>%
        select(
          "directory",
          "datasize",
          "nfiles",
          "nPreprocessed",
          "nFailed",
          "runDuration",
          "runTime"
        ) %>%
        return()
    } else if (info_to_extract %in% c("failedfiles", "successfulfiles", "skippedfiles")) {
      # windows_safe_read_lines(): same long-path exposure as the
      # windows_safe_read_tsv() call above -- see R/windowsLongPath.R.
      lines <- windows_safe_read_lines(str_glue(batch_summary_file))

      header_pattern <- if (info_to_extract == "successfulfiles") {
        "^------ Successfully processed files \\(n = (\\d+)\\):"
      } else if (info_to_extract == "skippedfiles") {
        # P7-03: files eyeQualityBatch() skipped this run because a matching
        # qcsummary output already existed for the given batchName (and
        # outputDir, if set) - i.e. resumability skips, not failures.
        "^------ Skipped files \\(already processed, resumability; n = (\\d+)\\):"
      } else {
        "^------ Files that failed processing \\(n = (\\d+)\\):"
      }

      header_idx <- grep(header_pattern, lines)

      if (length(header_idx) == 0) {
        stop(
          "parsePreprocessingBatchSummary: could not find a '",
          info_to_extract,
          "' section in '",
          batch_summary_file,
          "'"
        )
      }

      # eyeQualityBatch() appends to batch_run_summary (print_or_save(...,
      # savedata = TRUE) always opens with append = TRUE) rather than
      # overwriting it, so a summary file for a directory/batchName that has
      # been run more than once - e.g. resuming a partially-completed batch,
      # P7-03's resumability feature - can contain more than one occurrence
      # of this section header, one per run. The most recent run's section is
      # always the one that reflects current on-disk reality, so take the
      # last match, not the first.
      header_idx <- header_idx[length(header_idx)]
      n_files <- as.integer(str_extract(lines[header_idx], header_pattern, group = 1))

      if (info_to_extract == "failedfiles") {
        # P7-04: unlike successfulfiles/skippedfiles (one line per file),
        # each failed file occupies two lines in the batch summary - its
        # filepath, then an indented "  error: <message>" line directly
        # beneath it (written by eyeQualityBatch()'s "Files that failed
        # processing" section; see get_qcsummary_output_path() and the
        # parLapply() worker in R/eyeQualityBatch.R for where that error
        # detail is captured). n_files in the header still counts files, not
        # lines, so the line range spans 2 * n_files lines. Returned as a
        # data frame (file/error columns) rather than a bare character
        # vector: a bare vector has nowhere to carry the second piece of
        # per-file information (the error message) this branch now exposes.
        if (is.na(n_files) || n_files == 0) {
          return(tibble::tibble(file = character(0), error = character(0)))
        }

        entry_lines <- lines[(header_idx + 1):(header_idx + 2 * n_files)]
        file_lines <- entry_lines[seq(1, length(entry_lines), by = 2)]
        error_lines <- entry_lines[seq(2, length(entry_lines), by = 2)]

        return(
          tibble::tibble(
            file = file_lines,
            error = sub("^  error: ", "", error_lines)
          )
        )
      }

      if (is.na(n_files) || n_files == 0) {
        return(character(0))
      }

      lines[(header_idx + 1):(header_idx + n_files)] %>%
        return()
    } else {
      stop(
        "parsePreprocessingBatchSummary: unrecognized 'info_to_extract' value '",
        info_to_extract,
        "'"
      )
    }
  }
