# Internal helpers for filterByEvent() ---------------------------------------
#
# Kept separate from the exported function below so the actual
# window-derivation/row-filtering logic is directly unit-testable against
# plain in-memory data.frames, with no file I/O involved -- the same split
# this package already uses elsewhere (e.g. calculateOutputMetrics.R's
# .select_fixation_rows()/.calculate_precision_metrics()).

# .pair_event_windows: pair up startEvent/endEvent occurrences into one
# window per actual presentation, matched by shared eventValue. Generalizes
# the same FIFO-per-eventValue pairing the Analyze app's Gaze Explorer tab
# uses for its "Jump to stimulus" feature (VideoStimulusStart/
# VideoStimulusEnd specifically) to an arbitrary caller-supplied label pair,
# so a study using its own custom start/end marker names (not just the Tobii
# video-stimulus convention) still gets correct multi-presentation windows.
#
# Pairing rule: events are walked in ascending recordingTimestamp_ms order;
# each startEvent occurrence pushes its timestamp onto a per-eventValue FIFO
# queue, and each endEvent occurrence pops the OLDEST still-open start for
# that same eventValue to close a presentation -- correct even when the same
# eventValue is presented twice with something else in between (Start A,
# Start B, End B, End A still pairs correctly), not just for a single,
# non-overlapping run of presentations. An endEvent with no open start for
# its eventValue (an orphan end) is skipped; a start left open once every row
# has been walked (a truncated recording) never produces a window -- neither
# case is an error, both are simply excluded.
#
# events: a data.frame with "event", "eventValue", "recordingTimestamp_ms"
#   columns, or NULL/anything not shaped that way.
# Returns NULL if `events` isn't usable, or 0 complete pairs are found;
# otherwise a data.frame with columns stimulus/start_ms/end_ms/duration_ms,
# one row per presentation, sorted by start_ms.
.pair_event_windows <- function(events, startEvent, endEvent) {
  required_cols <- c("event", "eventValue", "recordingTimestamp_ms")
  if (is.null(events) || !is.data.frame(events) || !all(required_cols %in% names(events))) {
    return(NULL)
  }

  ev <- events[
    !is.na(events$event) & events$event %in% c(startEvent, endEvent) &
      !is.na(events$eventValue) & !is.na(events$recordingTimestamp_ms),
    required_cols,
    drop = FALSE
  ]
  if (nrow(ev) == 0) {
    return(NULL)
  }
  ev <- ev[order(ev$recordingTimestamp_ms), , drop = FALSE]

  open_starts <- list()
  rows <- list()
  for (i in seq_len(nrow(ev))) {
    label <- ev$event[i]
    value <- as.character(ev$eventValue[i])
    ts <- ev$recordingTimestamp_ms[i]
    if (label == startEvent) {
      open_starts[[value]] <- c(open_starts[[value]], ts)
    } else {
      queue <- open_starts[[value]]
      if (!is.null(queue) && length(queue) > 0) {
        start_ts <- queue[1]
        open_starts[[value]] <- queue[-1]
        rows[[length(rows) + 1]] <- data.frame(
          stimulus = value, start_ms = start_ts, end_ms = ts,
          duration_ms = ts - start_ts, stringsAsFactors = FALSE
        )
      }
    }
  }
  if (length(rows) == 0) {
    return(NULL)
  }
  out <- do.call(rbind, rows)
  out[order(out$start_ms), , drop = FALSE]
}

# .point_event_windows: one zero-duration window per occurrence of a single
# event label -- used when filterByEvent() is given no endEvent, i.e. the
# caller wants an instantaneous marker (e.g. "TrialStart" logged once per
# trial with no matching end) rather than a start/end pair. eventValue, when
# present, becomes `stimulus`; a schema without eventValue at all still works
# (`stimulus` is NA throughout), matching .pair_event_windows()'s NULL-safe
# contract but for the single-label case.
#
# Returns NULL if `events` isn't usable, or 0 rows match startEvent;
# otherwise a data.frame with columns stimulus/start_ms/end_ms/duration_ms
# (start_ms == end_ms, duration_ms == 0 throughout), sorted by start_ms.
.point_event_windows <- function(events, startEvent) {
  required_cols <- c("event", "recordingTimestamp_ms")
  if (is.null(events) || !is.data.frame(events) || !all(required_cols %in% names(events))) {
    return(NULL)
  }
  matched <- !is.na(events$event) & events$event == startEvent & !is.na(events$recordingTimestamp_ms)
  ev <- events[matched, , drop = FALSE]
  if (nrow(ev) == 0) {
    return(NULL)
  }
  ev <- ev[order(ev$recordingTimestamp_ms), , drop = FALSE]
  stimulus <- if ("eventValue" %in% names(ev)) as.character(ev$eventValue) else NA_character_
  data.frame(
    stimulus = stimulus,
    start_ms = ev$recordingTimestamp_ms,
    end_ms = ev$recordingTimestamp_ms,
    duration_ms = 0,
    stringsAsFactors = FALSE
  )
}

# .event_windows_for_file: dispatches to .pair_event_windows()/
# .point_event_windows() depending on whether endEvent was supplied, then
# applies the optional eventValue restriction shared by both paths. Returns
# NULL under the same "nothing usable" conditions those two functions do, or
# after eventValue filtering leaves 0 rows.
.event_windows_for_file <- function(events, startEvent, endEvent, eventValue) {
  windows <- if (is.null(endEvent)) {
    .point_event_windows(events, startEvent)
  } else {
    .pair_event_windows(events, startEvent, endEvent)
  }
  if (is.null(windows)) {
    return(NULL)
  }
  if (!is.null(eventValue)) {
    windows <- windows[windows$stimulus %in% eventValue, , drop = FALSE]
    if (nrow(windows) == 0) {
      return(NULL)
    }
  }
  windows
}

# .sample_period_ms: this file's own actual sample spacing, in ms, derived
# from calculateFrequency_hz() -- what includeBefore_n/includeAfter_n (sample
# counts) are converted through, since sampling rate is a property of the
# recording, not a value filterByEvent() should assume. Returns NA if it
# can't be determined (e.g. < 2 usable timestamps, or a degenerate/
# non-finite frequency) -- callers treat NA the same as "0 extra padding"
# rather than erroring, since a 0- or 1-row recording has no meaningful
# sample-count-based padding to apply anyway.
.sample_period_ms <- function(data) {
  if (!"recordingTimestamp_ms" %in% names(data) || nrow(data) < 2) {
    return(NA_real_)
  }
  hz <- calculateFrequency_hz(data)
  if (is.na(hz) || !is.finite(hz) || hz <= 0) {
    return(NA_real_)
  }
  1000 / hz
}

# .rows_in_window: the gaze-stream rows of `data` falling inside one
# already-padded [start_ms, end_ms] window, tagged with the identifying
# columns (source_file, stimulus, occurrence, event_start_ms, event_end_ms,
# window_start_ms, window_end_ms) prepended ahead of every column already
# present in `data` -- so any column `data` happens to carry, including
# pipeline intermediate columns from eyeQuality(includeIntermediates = TRUE),
# survives untouched into the result, ready to hand straight to
# calculateOutputMetrics() for per-stimulus (rather than whole-file)
# precision/robustness metrics. Returns a 0-row data.frame (with the correct
# columns, not NULL) when nothing in `data` falls inside the window, so
# repeated dplyr::bind_rows() calls across windows/files never need a NULL
# check.
.rows_in_window <- function(data, source_file, stimulus, occurrence,
                             event_start_ms, event_end_ms,
                             window_start_ms, window_end_ms) {
  in_window <- !is.na(data$recordingTimestamp_ms) &
    data$recordingTimestamp_ms >= window_start_ms &
    data$recordingTimestamp_ms <= window_end_ms
  matched <- data[in_window, , drop = FALSE]
  ids <- data.frame(
    source_file = source_file,
    stimulus = stimulus,
    occurrence = occurrence,
    event_start_ms = event_start_ms,
    event_end_ms = event_end_ms,
    window_start_ms = window_start_ms,
    window_end_ms = window_end_ms,
    stringsAsFactors = FALSE
  )
  # ids is normally built from scalar identifying values (one row) recycled
  # against however many rows of `data` actually fall in the window -- but a
  # real, legitimately-empty window (e.g. an event logged after the last
  # usable gaze sample, or padding that pushes the window past the end of
  # the recording) has 0 matching rows, and cbind()-ing a 1-row identifying
  # frame against a 0-row `matched` used to error on the row-count mismatch
  # instead of degrading to a 0-row result like every other "nothing
  # matched" case in this file. Explicitly resizing ids to nrow(matched)
  # (rather than relying on cbind's own recycling) fixes that, and leaves
  # the already-correct N-row and 0-row/0-row (empty vector args) cases
  # unaffected.
  ids <- ids[rep(seq_len(nrow(ids)), length.out = nrow(matched)), , drop = FALSE]
  cbind(ids, matched, row.names = NULL)
}

# .filter_by_event_data: the pure, file-I/O-free core of filterByEvent() for
# ONE file's already-loaded gaze/events data.frames -- everything below the
# path-resolution/reading layer, directly testable without touching disk.
#
# data: the gaze-stream data.frame (e.g. a loaded *_preproc.tsv), must have
#   "recordingTimestamp_ms".
# events: the sibling events data.frame (e.g. a loaded *_events.tsv), or
#   NULL/unusable -- treated as "no matching windows", same as an events.tsv
#   with 0 matching rows, not an error (some recordings legitimately log no
#   events at all, or none of the requested label(s)).
#
# Returns a data.frame (0 rows, but with the full column set, when nothing
# matches) combining every matched window's rows -- see .rows_in_window()'s
# own comment for the prepended identifying columns.
.filter_by_event_data <- function(data, events, startEvent, endEvent = NULL, eventValue = NULL,
                                   includeBefore_ms = 0, includeAfter_ms = 0,
                                   includeBefore_n = 0, includeAfter_n = 0,
                                   source_file = NA_character_) {
  empty_result <- .rows_in_window(data[0, , drop = FALSE], character(0), character(0), integer(0),
    numeric(0), numeric(0), numeric(0), numeric(0)
  )

  windows <- .event_windows_for_file(events, startEvent, endEvent, eventValue)
  if (is.null(windows)) {
    return(empty_result)
  }

  period_ms <- .sample_period_ms(data)
  extra_before_ms <- if (is.na(period_ms)) 0 else includeBefore_n * period_ms
  extra_after_ms <- if (is.na(period_ms)) 0 else includeAfter_n * period_ms

  # occurrence is 1-based PER STIMULUS (see filterByEvent()'s own @return
  # doc: "occurrence (1-based, per file and stimulus, in presentation
  # order)"), not a single running counter across every window regardless of
  # stimulus -- windows is already sorted by start_ms, so tracking each
  # stimulus's own count as we walk it in that order reproduces presentation
  # order correctly even when different stimuli's windows interleave (e.g.
  # A, B, A -- A's windows must land on occurrence 1 and 2, not 1 and 3).
  stimulus_counts <- new.env()
  next_occurrence <- function(stimulus_value) {
    key <- if (is.na(stimulus_value)) "NA" else stimulus_value
    current <- if (is.null(stimulus_counts[[key]])) 0L else stimulus_counts[[key]]
    stimulus_counts[[key]] <- current + 1L
    current + 1L
  }

  window_results <- lapply(seq_len(nrow(windows)), function(i) {
    .rows_in_window(
      data,
      source_file = source_file,
      stimulus = windows$stimulus[i],
      occurrence = next_occurrence(windows$stimulus[i]),
      event_start_ms = windows$start_ms[i],
      event_end_ms = windows$end_ms[i],
      window_start_ms = windows$start_ms[i] - includeBefore_ms - extra_before_ms,
      window_end_ms = windows$end_ms[i] + includeAfter_ms + extra_after_ms
    )
  })
  dplyr::bind_rows(window_results)
}

# .preproc_suffix_regex: matches a *_preproc.tsv filename's trailing
# "preproc.tsv", preceded by either "-" (saveFiles()'s NULL-batchName form,
# "..._desc-preproc.tsv") or "_" (its batchName-supplied form,
# "..._desc-<batchName>_preproc.tsv") -- both are real, common outputs of
# the SAME saveFiles() call depending only on whether batchName was passed,
# confirmed directly against create_new_filename()'s actual output for both
# cases (NOT just inferred from reading saveFiles()'s source). The
# `(^|[^A-Za-z0-9])` boundary (captured, so a substitution can preserve it)
# additionally guards against matching some unrelated file that merely ends
# in the letters "preproc.tsv" without a real separator before them.
.preproc_suffix_regex <- "(^|[^A-Za-z0-9])preproc\\.tsv$"

# .is_preproc_path: TRUE if `path`'s basename matches .preproc_suffix_regex.
.is_preproc_path <- function(path) {
  grepl(.preproc_suffix_regex, path)
}

# .resolve_events_path_from_preproc: derive a *_preproc.tsv path's sibling
# *_events.tsv path, the same "preproc" -> "events" stem substitution
# resolve_events_data_path() (inst/shiny-apps/app/analyze_helpers.R) uses --
# see saveFiles.R, which writes both from the same batchName-derived stem but
# as two independent words ("_events"/"_preproc"), not one suffixing the
# other, confirmed against real eyeQualityBatch() output (both the
# NULL-batchName and batchName-supplied naming forms -- see
# .preproc_suffix_regex's own comment). Returns NA if `preproc_path` doesn't
# actually match that pattern (callers validate this up front and error
# clearly; this helper stays a pure string transform).
.resolve_events_path_from_preproc <- function(preproc_path) {
  if (!.is_preproc_path(preproc_path)) {
    return(NA_character_)
  }
  sub(.preproc_suffix_regex, "\\1events.tsv", preproc_path)
}

# .filter_by_event_one_file: reads one file's preproc.tsv (required) and its
# sibling events.tsv (optional -- a missing/unreadable/empty events.tsv
# degrades to "no matching windows for this file", not an error, since a
# recording legitimately having 0 logged events, or an adapter with no event
# integration, is an expected case elsewhere in this package -- see
# load_gaze_trajectory_data()'s own comment, inst/shiny-apps/app/
# analyze_helpers.R, for the same degradation rule applied to the same file
# pair).
.filter_by_event_one_file <- function(preproc_path, startEvent, endEvent, eventValue,
                                       includeBefore_ms, includeAfter_ms,
                                       includeBefore_n, includeAfter_n) {
  data <- windows_safe_read_tsv(preproc_path, show_col_types = FALSE, progress = FALSE)
  if (!"recordingTimestamp_ms" %in% names(data)) {
    stop(
      "filterByEvent: '", preproc_path, "' has no recordingTimestamp_ms column -- ",
      "is this actually an eyeQuality *_preproc.tsv output file?",
      call. = FALSE
    )
  }

  events_path <- .resolve_events_path_from_preproc(preproc_path)
  events <- if (!is.na(events_path) && file.exists(events_path)) {
    tryCatch(
      windows_safe_read_tsv(events_path, show_col_types = FALSE, progress = FALSE),
      error = function(e) NULL
    )
  } else {
    NULL
  }

  .filter_by_event_data(
    data, events, startEvent, endEvent, eventValue,
    includeBefore_ms, includeAfter_ms, includeBefore_n, includeAfter_n,
    source_file = preproc_path
  )
}

#' Filter gaze data down to the timepoints belonging to one or more logged events
#'
#' Extracts just the gaze samples that fall within a specific logged event (or
#' repeated presentations of one), optionally padded by a fixed amount of
#' time before/after -- across a single processed file, or every processed
#' file found in a BIDS-style study directory. Meant to replace ad hoc
#' per-analysis subsetting: the same "pair Start/End markers by eventValue
#' into one window per presentation" logic the Analyze app's Gaze Explorer
#' tab uses for its "Jump to stimulus" feature, exposed here as a reusable
#' package function, since many tasks present more than one stimulus per
#' recording and per-stimulus (not whole-file) precision/robustness is
#' frequently the metric that actually matters for those study designs.
#'
#' The returned data.frame preserves every column already present in the
#' source file(s) -- including any pipeline intermediate column produced by
#' `eyeQuality(includeIntermediates = TRUE)` -- unmodified, alongside a
#' handful of new identifying columns prepended in front. That means the
#' result can be handed directly to [calculateOutputMetrics()] (after
#' splitting by `source_file`/`stimulus`/`occurrence`, e.g. via
#' `dplyr::group_by()` + `dplyr::group_modify()`) to compute precision and
#' robustness metrics for just one stimulus presentation, rather than the
#' whole recording.
#'
#' @param path Either the path to a single `*_preproc.tsv` file (the same
#'   output `saveFiles()`/`eyeQuality()` write), or a directory to search for
#'   every processed file in a BIDS-style study, via
#'   [listBidsDerivativeFiles()] -- `path` should be the STUDY ROOT
#'   (containing `sub-<label>/ses-<label>/...` raw files with their own
#'   sibling `derivatives/` folders), matching that function's own contract,
#'   not the `derivatives/eyeQuality-v1/` folder itself.
#' @param startEvent Character scalar: the `event` column value marking the
#'   start of the window of interest -- or, when `endEvent` is left `NULL`,
#'   the single instantaneous marker itself (see `endEvent` below).
#' @param endEvent Character scalar, or `NULL` (default). When `NULL`,
#'   `startEvent` is treated as a point-in-time marker: each occurrence
#'   produces a zero-duration window at that exact timestamp, widened only by
#'   `includeBefore_ms`/`includeAfter_ms`/`includeBefore_n`/`includeAfter_n`
#'   below. When supplied, `startEvent`/`endEvent` occurrences are paired by
#'   their shared `eventValue` (oldest-open-start-closes-first, correct even
#'   when the same `eventValue` is presented more than once in the same
#'   recording) into one window per actual presentation -- must be a
#'   different label from `startEvent`.
#' @param eventValue Optional character vector further restricting which
#'   presentation(s)/occurrence(s) are included, matched against the paired
#'   (or point-event) `eventValue` -- e.g. just one specific stimulus name.
#'   `NULL` (default) includes every occurrence.
#' @param includeBefore_ms,includeAfter_ms Non-negative numeric, milliseconds
#'   of padding to include before the window's start / after its end.
#'   Default `0` (exactly the event window, no padding).
#' @param includeBefore_n,includeAfter_n Non-negative integer sample counts,
#'   converted to milliseconds via each file's own actual sampling rate
#'   (see [calculateFrequency_hz()]) and ADDED to
#'   `includeBefore_ms`/`includeAfter_ms` above (not used instead of it) --
#'   so a caller can combine "at least 50ms" with "and at least 3 samples" if
#'   both guarantees matter for their analysis. Default `0`.
#' @param recursiveSearch,subjectPattern_regex,sessionPattern_regex,derivativePattern_regex
#'   Forwarded to [listBidsDerivativeFiles()] when `path` is a directory;
#'   ignored for a single-file `path`. `derivativePattern_regex` defaults to
#'   matching only `*_preproc.tsv` files (not their `_qcsummary`/`_runtimes`
#'   siblings) -- override only if this study's preproc files are named
#'   differently.
#'
#' @return A data.frame combining every matched window's rows across every
#'   file, with `source_file`, `stimulus`, `occurrence` (1-based, per file
#'   and stimulus, in presentation order), `event_start_ms`, `event_end_ms`
#'   (the unpadded event window), and `window_start_ms`, `window_end_ms` (the
#'   actual, padded bounds rows were filtered against) prepended ahead of
#'   every column already present in the source file(s). A 0-row data.frame
#'   (not an error) when nothing matches -- a missing/unusable/empty sibling
#'   events.tsv for a given file degrades that file to "no matching windows"
#'   rather than failing the whole call, since a recording having no logged
#'   events (or none of the requested label) is an expected, not exceptional,
#'   case.
#' @export
filterByEvent <- function(path, startEvent, endEvent = NULL, eventValue = NULL,
                           includeBefore_ms = 0, includeAfter_ms = 0,
                           includeBefore_n = 0, includeAfter_n = 0,
                           recursiveSearch = FALSE,
                           subjectPattern_regex = "sub-[A-Z0-9]+",
                           sessionPattern_regex = "ses-[0-9]+",
                           derivativePattern_regex = .preproc_suffix_regex) {
  if (!is.character(startEvent) || length(startEvent) != 1 || is.na(startEvent)) {
    stop("filterByEvent: startEvent must be a single, non-NA character value.", call. = FALSE)
  }
  if (!is.null(endEvent)) {
    if (!is.character(endEvent) || length(endEvent) != 1 || is.na(endEvent)) {
      stop("filterByEvent: endEvent must be NULL or a single, non-NA character value.", call. = FALSE)
    }
    if (identical(endEvent, startEvent)) {
      stop(
        "filterByEvent: startEvent and endEvent must be different labels for a start/end pair -- ",
        "leave endEvent = NULL instead if you want a single instantaneous marker.",
        call. = FALSE
      )
    }
  }
  pad_args <- c(includeBefore_ms, includeAfter_ms, includeBefore_n, includeAfter_n)
  if (!is.numeric(pad_args) || any(is.na(pad_args)) || any(pad_args < 0)) {
    stop(
      "filterByEvent: includeBefore_ms/includeAfter_ms/includeBefore_n/includeAfter_n ",
      "must all be non-negative numbers.",
      call. = FALSE
    )
  }

  if (dir.exists(path)) {
    preproc_files <- listBidsDerivativeFiles(
      path,
      subjectPattern_regex = subjectPattern_regex,
      sessionPattern_regex = sessionPattern_regex,
      recursiveSearch = recursiveSearch,
      derivativePattern_regex = derivativePattern_regex
    )
  } else {
    if (!.is_preproc_path(path)) {
      stop(
        "filterByEvent: '", path, "' doesn't look like an eyeQuality *_preproc.tsv output file ",
        "(and isn't an existing directory either). Point `path` at a specific *_preproc.tsv file, ",
        "or at a BIDS study root to search across every processed file.",
        call. = FALSE
      )
    }
    if (!file.exists(path)) {
      stop("filterByEvent: file not found: '", path, "'.", call. = FALSE)
    }
    preproc_files <- path
  }

  if (length(preproc_files) == 0) {
    return(.rows_in_window(
      data.frame(recordingTimestamp_ms = numeric(0)), character(0), character(0),
      integer(0), numeric(0), numeric(0), numeric(0), numeric(0)
    ))
  }

  results <- lapply(preproc_files, function(f) {
    .filter_by_event_one_file(
      f, startEvent, endEvent, eventValue,
      includeBefore_ms, includeAfter_ms, includeBefore_n, includeAfter_n
    )
  })
  dplyr::bind_rows(results)
}
