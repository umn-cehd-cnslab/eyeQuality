# Tests for filterByEvent() and its internal pure helpers (R/filterByEvent.R).
# filterByEvent() filters a gaze data.frame down to just the timepoints
# belonging to one or more logged events, optionally padded before/after in
# ms and/or sample-count, across a single *_preproc.tsv file or every
# processed file in a BIDS-style study directory. Following this package's
# existing "pure internal helper + thin file-I/O wrapper" pattern (see
# calculateOutputMetrics.R's .select_fixation_rows()/
# .calculate_precision_metrics()), most of these tests exercise the internal
# helpers directly against plain in-memory data.frames -- no file I/O.

# ---------------------------------------------------------------------------
# .pair_event_windows()
# ---------------------------------------------------------------------------

test_that(".pair_event_windows pairs Start/End occurrences by shared eventValue, FIFO per value", {
  # Same eventValue ("A") presented twice with a different value's start/end
  # nested in between (Start A, Start B, End B, End A) -- this is the whole
  # reason a per-eventValue queue was used instead of a single
  # most-recently-seen start; a naive "close on next End" implementation
  # would wrongly pair the first Start A with the End B in between.
  events <- data.frame(
    event = c("Start", "Start", "End", "End"),
    eventValue = c("A", "B", "B", "A"),
    recordingTimestamp_ms = c(100, 200, 300, 400)
  )
  result <- .pair_event_windows(events, "Start", "End")

  expect_equal(nrow(result), 2)
  a_row <- result[result$stimulus == "A", ]
  b_row <- result[result$stimulus == "B", ]
  expect_equal(a_row$start_ms, 100)
  expect_equal(a_row$end_ms, 400)
  expect_equal(a_row$duration_ms, 300)
  expect_equal(b_row$start_ms, 200)
  expect_equal(b_row$end_ms, 300)
  expect_equal(b_row$duration_ms, 100)
})

test_that(".pair_event_windows correctly pairs the SAME eventValue presented twice in sequence (not overlapping)", {
  events <- data.frame(
    event = c("Start", "End", "Start", "End"),
    eventValue = c("A", "A", "A", "A"),
    recordingTimestamp_ms = c(100, 200, 500, 600)
  )
  result <- .pair_event_windows(events, "Start", "End")

  expect_equal(nrow(result), 2)
  expect_equal(result$start_ms, c(100, 500))
  expect_equal(result$end_ms, c(200, 600))
})

test_that(".pair_event_windows skips an orphan End with no open Start for its eventValue", {
  events <- data.frame(
    event = c("End", "Start", "End"),
    eventValue = c("A", "A", "A"),
    recordingTimestamp_ms = c(50, 100, 200)
  )
  result <- .pair_event_windows(events, "Start", "End")

  expect_equal(nrow(result), 1)
  expect_equal(result$start_ms, 100)
  expect_equal(result$end_ms, 200)
})

test_that(".pair_event_windows drops a truncated Start left open at the end of the recording", {
  events <- data.frame(
    event = c("Start", "End", "Start"),
    eventValue = c("A", "A", "A"),
    recordingTimestamp_ms = c(100, 200, 900)
  )
  result <- .pair_event_windows(events, "Start", "End")

  expect_equal(nrow(result), 1)
  expect_equal(result$start_ms, 100)
  expect_equal(result$end_ms, 200)
})

test_that(".pair_event_windows returns NULL when Start/End labels are present but 0 complete pairs form", {
  # only orphan Ends, no Start ever logged for either -- every End is
  # skipped (no open queue entry to close), so rows stays empty even though
  # matching events (by label) did exist.
  events <- data.frame(
    event = c("End", "End"),
    eventValue = c("A", "B"),
    recordingTimestamp_ms = c(100, 200)
  )
  expect_null(.pair_event_windows(events, "Start", "End"))
})

test_that(".pair_event_windows returns NULL for unusable or empty events input", {
  expect_null(.pair_event_windows(NULL, "Start", "End"))
  expect_null(.pair_event_windows(data.frame(x = 1), "Start", "End"))
  expect_null(.pair_event_windows(
    data.frame(event = character(0), eventValue = character(0), recordingTimestamp_ms = numeric(0)),
    "Start", "End"
  ))
  # events present, but none match startEvent/endEvent at all
  expect_null(.pair_event_windows(
    data.frame(event = "SomethingElse", eventValue = "A", recordingTimestamp_ms = 1),
    "Start", "End"
  ))
})

# ---------------------------------------------------------------------------
# .point_event_windows()
# ---------------------------------------------------------------------------

test_that(".point_event_windows produces one zero-duration window per occurrence, using eventValue as stimulus", {
  events <- data.frame(
    event = c("TrialStart", "Other", "TrialStart"),
    eventValue = c("cond1", "ignored", "cond2"),
    recordingTimestamp_ms = c(100, 150, 300)
  )
  result <- .point_event_windows(events, "TrialStart")

  expect_equal(nrow(result), 2)
  expect_equal(result$stimulus, c("cond1", "cond2"))
  expect_equal(result$start_ms, result$end_ms)
  expect_equal(result$duration_ms, c(0, 0))
})

test_that(".point_event_windows works with no eventValue column at all (stimulus stays NA)", {
  events <- data.frame(
    event = c("Blink", "Blink"),
    recordingTimestamp_ms = c(100, 200)
  )
  result <- .point_event_windows(events, "Blink")

  expect_equal(nrow(result), 2)
  expect_true(all(is.na(result$stimulus)))
})

test_that(".point_event_windows returns NULL when nothing matches startEvent", {
  events <- data.frame(event = "Other", eventValue = "x", recordingTimestamp_ms = 1)
  expect_null(.point_event_windows(events, "Blink"))
  expect_null(.point_event_windows(NULL, "Blink"))
})

# ---------------------------------------------------------------------------
# .event_windows_for_file()
# ---------------------------------------------------------------------------

test_that(".event_windows_for_file dispatches to point mode when endEvent is NULL, pair mode otherwise", {
  point_events <- data.frame(event = "Marker", eventValue = "A", recordingTimestamp_ms = 100)
  point_result <- .event_windows_for_file(point_events, "Marker", NULL, NULL)
  expect_equal(nrow(point_result), 1)
  expect_equal(point_result$start_ms, point_result$end_ms)

  pair_events <- data.frame(
    event = c("Start", "End"), eventValue = c("A", "A"),
    recordingTimestamp_ms = c(100, 200)
  )
  pair_result <- .event_windows_for_file(pair_events, "Start", "End", NULL)
  expect_equal(nrow(pair_result), 1)
  expect_equal(pair_result$start_ms, 100)
  expect_equal(pair_result$end_ms, 200)
})

test_that(".event_windows_for_file restricts to the requested eventValue(s)", {
  events <- data.frame(
    event = c("Start", "End", "Start", "End"),
    eventValue = c("A", "A", "B", "B"),
    recordingTimestamp_ms = c(100, 200, 300, 400)
  )
  result <- .event_windows_for_file(events, "Start", "End", eventValue = "B")
  expect_equal(nrow(result), 1)
  expect_equal(result$stimulus, "B")
})

test_that(".event_windows_for_file returns NULL when the eventValue restriction matches nothing", {
  events <- data.frame(
    event = c("Start", "End"), eventValue = c("A", "A"),
    recordingTimestamp_ms = c(100, 200)
  )
  expect_null(.event_windows_for_file(events, "Start", "End", eventValue = "NoSuchValue"))
})

# ---------------------------------------------------------------------------
# .sample_period_ms()
# ---------------------------------------------------------------------------

test_that(".sample_period_ms returns the correct ms-per-sample for a regularly-sampled series", {
  data <- data.frame(recordingTimestamp_ms = seq(0, 1000, by = 10)) # 100 Hz
  expect_equal(.sample_period_ms(data), 10)
})

test_that(".sample_period_ms is NA-safe for fewer than 2 rows or a missing timestamp column", {
  expect_true(is.na(.sample_period_ms(data.frame(recordingTimestamp_ms = numeric(0)))))
  expect_true(is.na(.sample_period_ms(data.frame(recordingTimestamp_ms = 5))))
  expect_true(is.na(.sample_period_ms(data.frame(x = 1:5))))
})

test_that(".sample_period_ms is NA-safe for a degenerate (zero-spacing) frequency", {
  # every timestamp identical -> mean diff of 0 -> infinite/degenerate Hz
  data <- data.frame(recordingTimestamp_ms = rep(5, 10))
  expect_true(is.na(.sample_period_ms(data)))
})

# ---------------------------------------------------------------------------
# .rows_in_window()
# ---------------------------------------------------------------------------

test_that(".rows_in_window selects only rows within [window_start_ms, window_end_ms] and prepends identifying columns", {
  data <- data.frame(
    recordingTimestamp_ms = c(0, 10, 20, 30, 40),
    gazeX.preprocessed_px = c(1, 2, 3, 4, 5)
  )
  result <- .rows_in_window(
    data,
    source_file = "f.tsv", stimulus = "A", occurrence = 1,
    event_start_ms = 10, event_end_ms = 30,
    window_start_ms = 10, window_end_ms = 30
  )

  expect_equal(nrow(result), 3)
  expect_equal(result$recordingTimestamp_ms, c(10, 20, 30))
  expect_equal(
    names(result)[1:7],
    c("source_file", "stimulus", "occurrence", "event_start_ms", "event_end_ms", "window_start_ms", "window_end_ms")
  )
  expect_true("gazeX.preprocessed_px" %in% names(result))
  expect_equal(unique(result$source_file), "f.tsv")
  expect_equal(unique(result$stimulus), "A")
})

test_that(".rows_in_window returns a 0-row data.frame (not NULL) with the full column set when nothing matches", {
  data <- data.frame(
    recordingTimestamp_ms = c(0, 10, 20),
    gazeX.preprocessed_px = c(1, 2, 3)
  )
  result <- .rows_in_window(
    data,
    source_file = "f.tsv", stimulus = "A", occurrence = 1,
    event_start_ms = 1000, event_end_ms = 2000,
    window_start_ms = 1000, window_end_ms = 2000
  )

  expect_false(is.null(result))
  expect_equal(nrow(result), 0)
  expect_true("gazeX.preprocessed_px" %in% names(result))
})

test_that(".rows_in_window excludes rows with NA recordingTimestamp_ms", {
  data <- data.frame(
    recordingTimestamp_ms = c(0, NA, 20),
    gazeX.preprocessed_px = c(1, 2, 3)
  )
  result <- .rows_in_window(
    data,
    source_file = "f.tsv", stimulus = "A", occurrence = 1,
    event_start_ms = 0, event_end_ms = 20,
    window_start_ms = 0, window_end_ms = 20
  )
  expect_equal(nrow(result), 2)
  expect_false(anyNA(result$recordingTimestamp_ms))
})

# ---------------------------------------------------------------------------
# .filter_by_event_data() -- the full pure per-file core
# ---------------------------------------------------------------------------

# Shared synthetic fixture: 1000ms recording at 100 Hz (10ms spacing), two
# presentations of stimulus "A" and one of "B", using custom (non-Tobii)
# event labels to prove the pairing logic isn't hardcoded to Tobii's
# VideoStimulusStart/End convention.
filter_by_event_fixture <- function() {
  ts <- seq(0, 990, by = 10)
  data <- data.frame(
    recordingTimestamp_ms = ts,
    gazeX.preprocessed_px = seq_along(ts),
    gazeY.preprocessed_px = rev(seq_along(ts))
  )
  events <- data.frame(
    event = c("StimOn", "StimOff", "StimOn", "StimOff", "StimOn", "StimOff"),
    eventValue = c("A", "A", "B", "B", "A", "A"),
    recordingTimestamp_ms = c(100, 200, 400, 500, 700, 800)
  )
  list(data = data, events = events)
}

test_that(".filter_by_event_data returns rows for every matched window, tagged with occurrence restarting per stimulus", {
  fx <- filter_by_event_fixture()
  result <- .filter_by_event_data(
    fx$data, fx$events,
    startEvent = "StimOn", endEvent = "StimOff",
    source_file = "synthetic_preproc.tsv"
  )

  expect_true(nrow(result) > 0)
  expect_setequal(unique(result$stimulus), c("A", "B"))

  # occurrence restarts per stimulus, not a single running counter across
  # different stimuli: "A" appears at presentation index 1 and 3 overall
  # (interleaved with "B" at index 2), but should be tagged occurrence 1/2
  # within its own stimulus, and "B" should be occurrence 1.
  a_occurrences <- sort(unique(result$occurrence[result$stimulus == "A"]))
  b_occurrences <- sort(unique(result$occurrence[result$stimulus == "B"]))
  expect_equal(a_occurrences, c(1, 2))
  expect_equal(b_occurrences, 1)
})

test_that(".filter_by_event_data preserves every column from the input data.frame, not just gaze X/Y", {
  fx <- filter_by_event_fixture()
  fx$data$IVT.classification.valid <- "fixation" # simulated includeIntermediates = TRUE column
  result <- .filter_by_event_data(
    fx$data, fx$events,
    startEvent = "StimOn", endEvent = "StimOff",
    source_file = "synthetic_preproc.tsv"
  )
  expect_true(all(names(fx$data) %in% names(result)))
  expect_true("IVT.classification.valid" %in% names(result))
})

test_that(".filter_by_event_data applies includeBefore_ms/includeAfter_ms padding to the window bounds", {
  fx <- filter_by_event_fixture()
  result <- .filter_by_event_data(
    fx$data, fx$events,
    startEvent = "StimOn", endEvent = "StimOff",
    includeBefore_ms = 20, includeAfter_ms = 20,
    source_file = "synthetic_preproc.tsv"
  )
  # first "A" window is [100, 200], padded by 20ms -> [80, 220]
  first_a <- result[result$stimulus == "A" & result$occurrence == 1, ]
  expect_equal(min(first_a$recordingTimestamp_ms), 80)
  expect_equal(max(first_a$recordingTimestamp_ms), 220)
  expect_equal(unique(first_a$window_start_ms), 80)
  expect_equal(unique(first_a$window_end_ms), 220)
})

test_that(".filter_by_event_data converts includeBefore_n/includeAfter_n sample counts via the file's own sampling rate", {
  fx <- filter_by_event_fixture()
  point_events <- data.frame(
    event = c("Blink", "Blink"), eventValue = c(NA, NA),
    recordingTimestamp_ms = c(300, 600)
  )
  result <- .filter_by_event_data(
    fx$data, point_events,
    startEvent = "Blink", endEvent = NULL,
    includeBefore_n = 2, includeAfter_n = 2,
    source_file = "synthetic_preproc.tsv"
  )
  # 10ms spacing, 2 samples before/after -> [280, 320] for the first blink
  first_blink <- result[result$occurrence == 1, ]
  expect_equal(min(first_blink$recordingTimestamp_ms), 280)
  expect_equal(max(first_blink$recordingTimestamp_ms), 320)
})

test_that(".filter_by_event_data ADDS includeBefore_ms and includeBefore_n rather than one overriding the other", {
  fx <- filter_by_event_fixture()
  point_events <- data.frame(
    event = "Blink", eventValue = NA_character_, recordingTimestamp_ms = 300
  )
  # 10ms spacing: includeBefore_n = 2 -> 20ms, plus includeBefore_ms = 15ms
  # explicit -> together the window should start at 300 - 15 - 20 = 265.
  result <- .filter_by_event_data(
    fx$data, point_events,
    startEvent = "Blink", endEvent = NULL,
    includeBefore_ms = 15, includeBefore_n = 2,
    source_file = "synthetic_preproc.tsv"
  )
  expect_equal(unique(result$window_start_ms), 300 - 15 - 20)
})

test_that(".filter_by_event_data degrades to a 0-row result (not an error) for NULL/unusable/empty events", {
  fx <- filter_by_event_fixture()

  none_result <- .filter_by_event_data(
    fx$data, NULL,
    startEvent = "StimOn", endEvent = "StimOff",
    source_file = "synthetic_preproc.tsv"
  )
  expect_equal(nrow(none_result), 0)
  expect_true("recordingTimestamp_ms" %in% names(none_result))

  empty_events <- data.frame(event = character(0), eventValue = character(0), recordingTimestamp_ms = numeric(0))
  empty_result <- .filter_by_event_data(
    fx$data, empty_events,
    startEvent = "StimOn", endEvent = "StimOff",
    source_file = "synthetic_preproc.tsv"
  )
  expect_equal(nrow(empty_result), 0)

  no_match_result <- .filter_by_event_data(
    fx$data, fx$events,
    startEvent = "NoSuchEvent", endEvent = NULL,
    source_file = "synthetic_preproc.tsv"
  )
  expect_equal(nrow(no_match_result), 0)
})

# ---------------------------------------------------------------------------
# .is_preproc_path() / .resolve_events_path_from_preproc() -- filename
# pattern matching. BOTH real naming forms saveFiles() actually produces are
# tested here (confirmed against saveFiles.R's own preprocdesc/eventdesc
# construction): "..._desc-preproc.tsv" (NULL batchName, hyphen before
# "preproc") and "..._desc-<batchName>_preproc.tsv" (batchName supplied,
# underscore before "preproc").
# ---------------------------------------------------------------------------

test_that(".is_preproc_path matches both real saveFiles() naming forms", {
  expect_true(.is_preproc_path("sub-01_ses-01_task-video_recording-eyetracking_physio_desc-preproc.tsv"))
  expect_true(.is_preproc_path("sub-01_ses-01_task-video_recording-eyetracking_physio_desc-myBatch_preproc.tsv"))
})

test_that(".is_preproc_path rejects sibling non-preproc derivative files and unrelated files", {
  expect_false(.is_preproc_path("sub-01_ses-01_desc-preproc_qcsummary.tsv"))
  expect_false(.is_preproc_path("sub-01_ses-01_desc-preproc_runtimes.tsv"))
  expect_false(.is_preproc_path("sub-01_ses-01_desc-events.tsv"))
  expect_false(.is_preproc_path("some_random_file.tsv"))
  # merely ending in the letters "preproc.tsv" without a real separator
  # before them should NOT match
  expect_false(.is_preproc_path("notactuallypreproc.tsv"))
})

test_that(".resolve_events_path_from_preproc derives the sibling events.tsv path for the NULL-batchName naming form", {
  preproc <- "/data/sub-01/ses-01/derivatives/eyeQuality-v1/sub-01_ses-01_task-video_recording-eyetracking_physio_desc-preproc.tsv"
  expected <- "/data/sub-01/ses-01/derivatives/eyeQuality-v1/sub-01_ses-01_task-video_recording-eyetracking_physio_desc-events.tsv"
  expect_equal(.resolve_events_path_from_preproc(preproc), expected)
})

test_that(".resolve_events_path_from_preproc derives the sibling events.tsv path for the batchName-supplied naming form", {
  # saveFiles()'s eventdesc, like preprocdesc, keeps batchName in the
  # filename when one is supplied ("_desc-<batchName>_events.tsv"), so the
  # derived events path must too -- only the trailing "preproc"/"events"
  # word itself is substituted.
  preproc <- "/data/sub-01/ses-01/derivatives/eyeQuality-v1/sub-01_ses-01_task-video_recording-eyetracking_physio_desc-myBatch_preproc.tsv"
  expected <- "/data/sub-01/ses-01/derivatives/eyeQuality-v1/sub-01_ses-01_task-video_recording-eyetracking_physio_desc-myBatch_events.tsv"
  expect_equal(.resolve_events_path_from_preproc(preproc), expected)
})

test_that(".resolve_events_path_from_preproc returns NA for a path that isn't actually a *_preproc.tsv file", {
  expect_true(is.na(.resolve_events_path_from_preproc("sub-01_ses-01_desc-qcsummary.tsv")))
})

# ---------------------------------------------------------------------------
# filterByEvent() -- exported end-to-end behavior, argument validation
# ---------------------------------------------------------------------------

test_that("filterByEvent errors clearly when endEvent is identical to startEvent", {
  expect_error(
    filterByEvent("some_preproc.tsv", startEvent = "Marker", endEvent = "Marker"),
    "different labels"
  )
})

test_that("filterByEvent errors clearly on negative or non-numeric padding arguments", {
  expect_error(
    filterByEvent("some_preproc.tsv", startEvent = "Marker", includeBefore_ms = -5),
    "non-negative"
  )
  expect_error(
    filterByEvent("some_preproc.tsv", startEvent = "Marker", includeAfter_n = "two"),
    "non-negative"
  )
  expect_error(
    filterByEvent("some_preproc.tsv", startEvent = "Marker", includeBefore_n = NA),
    "non-negative"
  )
})

test_that("filterByEvent errors clearly when startEvent isn't a single non-NA character value", {
  expect_error(filterByEvent("some_preproc.tsv", startEvent = c("A", "B")), "single, non-NA")
  expect_error(filterByEvent("some_preproc.tsv", startEvent = NA_character_), "single, non-NA")
  expect_error(filterByEvent("some_preproc.tsv", startEvent = 5), "single, non-NA")
})

test_that("filterByEvent errors clearly when endEvent is supplied but isn't a single non-NA character value", {
  expect_error(
    filterByEvent("some_preproc.tsv", startEvent = "Marker", endEvent = c("A", "B")),
    "NULL or a single, non-NA"
  )
  expect_error(
    filterByEvent("some_preproc.tsv", startEvent = "Marker", endEvent = NA_character_),
    "NULL or a single, non-NA"
  )
})

test_that("filterByEvent errors clearly when path is neither a real directory nor a *_preproc.tsv-shaped file", {
  expect_error(
    filterByEvent(tempfile("not_a_real_path_"), startEvent = "Marker"),
    "doesn't look like an eyeQuality"
  )
  expect_error(
    filterByEvent("some_random_file.csv", startEvent = "Marker"),
    "doesn't look like an eyeQuality"
  )
})

test_that("filterByEvent errors clearly when a valid-looking *_preproc.tsv path doesn't actually exist", {
  missing_path <- file.path(tempdir(), "sub-99_ses-01_task-x_recording-eyetracking_physio_desc-preproc.tsv")
  expect_false(file.exists(missing_path))
  expect_error(filterByEvent(missing_path, startEvent = "Marker"), "file not found")
})

# ---------------------------------------------------------------------------
# filterByEvent() -- exported end-to-end behavior against real files on disk
# ---------------------------------------------------------------------------

# Writes a single preproc.tsv + sibling events.tsv pair directly (not run
# through the full eyeQuality()/saveFiles() pipeline -- this is testing
# filterByEvent()'s own file-resolution/reading layer, not the pipeline that
# produces these files elsewhere).
write_filter_by_event_single_file <- function(dir, batchName = NULL) {
  fx <- filter_by_event_fixture()
  preproc_desc <- if (is.null(batchName)) "preproc" else paste0(batchName, "_preproc")
  events_desc <- if (is.null(batchName)) "events" else paste0(batchName, "_events")
  preproc_path <- file.path(dir, paste0("sub-01_ses-01_task-video_recording-eyetracking_physio_desc-", preproc_desc, ".tsv"))
  events_path <- file.path(dir, paste0("sub-01_ses-01_task-video_recording-eyetracking_physio_desc-", events_desc, ".tsv"))
  write.table(fx$data, preproc_path, sep = "\t", row.names = FALSE)
  write.table(fx$events, events_path, sep = "\t", row.names = FALSE)
  list(preproc_path = preproc_path, events_path = events_path, data = fx$data, events = fx$events)
}

# Same BIDS-style layout listBidsDerivativeFiles() expects: sub/ses raw file
# with a sibling derivatives/eyeQuality-v1/ folder holding the preproc/events
# pair.
write_filter_by_event_bids_study <- function(root) {
  sub_dir <- file.path(root, "sub-01", "ses-01")
  deriv_dir <- file.path(sub_dir, "derivatives", "eyeQuality-v1")
  dir.create(deriv_dir, recursive = TRUE)
  raw_file <- file.path(sub_dir, "sub-01_ses-01_task-video_recording-eyetracking_physio.tsv")
  writeLines("placeholder raw file", raw_file)

  fx <- filter_by_event_fixture()
  preproc_path <- file.path(deriv_dir, "sub-01_ses-01_task-video_recording-eyetracking_physio_desc-preproc.tsv")
  events_path <- file.path(deriv_dir, "sub-01_ses-01_task-video_recording-eyetracking_physio_desc-events.tsv")
  write.table(fx$data, preproc_path, sep = "\t", row.names = FALSE)
  write.table(fx$events, events_path, sep = "\t", row.names = FALSE)
  list(preproc_path = preproc_path, events_path = events_path)
}

test_that("filterByEvent reads a real preproc.tsv/events.tsv sibling pair in single-file mode", {
  skip_on_cran()
  dir <- tempfile("filterByEvent_single_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  fx <- write_filter_by_event_single_file(dir)

  result <- filterByEvent(
    fx$preproc_path,
    startEvent = "StimOn", endEvent = "StimOff",
    includeBefore_ms = 20, includeAfter_ms = 20
  )

  expect_true(nrow(result) > 0)
  expect_setequal(unique(result$stimulus), c("A", "B"))
  expect_true(all(names(fx$data) %in% names(result)))
  expect_equal(unique(result$source_file), fx$preproc_path)
})

test_that("filterByEvent's single-file mode also works with the batchName-supplied naming form", {
  skip_on_cran()
  dir <- tempfile("filterByEvent_batch_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  fx <- write_filter_by_event_single_file(dir, batchName = "myBatch")

  result <- filterByEvent(fx$preproc_path, startEvent = "StimOn", endEvent = "StimOff")
  expect_true(nrow(result) > 0)
})

test_that("filterByEvent finds and reads every processed file in a BIDS study directory", {
  skip_on_cran()
  root <- tempfile("filterByEvent_bids_")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  fx <- write_filter_by_event_bids_study(root)

  single_result <- filterByEvent(
    fx$preproc_path,
    startEvent = "StimOn", endEvent = "StimOff",
    includeBefore_ms = 20, includeAfter_ms = 20
  )
  batch_result <- filterByEvent(
    root,
    startEvent = "StimOn", endEvent = "StimOff",
    includeBefore_ms = 20, includeAfter_ms = 20
  )

  expect_equal(nrow(batch_result), nrow(single_result))
  expect_equal(normalizePath(unique(batch_result$source_file)), normalizePath(fx$preproc_path))
})

test_that("filterByEvent degrades a file with a missing sibling events.tsv to 'no matching windows', not an error", {
  skip_on_cran()
  root <- tempfile("filterByEvent_missing_events_")
  sub_dir <- file.path(root, "sub-01", "ses-01")
  deriv_dir <- file.path(sub_dir, "derivatives", "eyeQuality-v1")
  dir.create(deriv_dir, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  raw_file <- file.path(sub_dir, "sub-01_ses-01_task-video_recording-eyetracking_physio.tsv")
  writeLines("placeholder raw file", raw_file)

  fx <- filter_by_event_fixture()
  preproc_path <- file.path(deriv_dir, "sub-01_ses-01_task-video_recording-eyetracking_physio_desc-preproc.tsv")
  write.table(fx$data, preproc_path, sep = "\t", row.names = FALSE)
  # deliberately no sibling events.tsv written

  expect_error(
    result <- filterByEvent(preproc_path, startEvent = "StimOn", endEvent = "StimOff"),
    NA
  )
  expect_equal(nrow(result), 0)
})

test_that("filterByEvent degrades a file with a 0-row events.tsv to 'no matching windows', not an error", {
  skip_on_cran()
  dir <- tempfile("filterByEvent_empty_events_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  fx <- filter_by_event_fixture()
  preproc_path <- file.path(dir, "sub-01_ses-01_task-video_recording-eyetracking_physio_desc-preproc.tsv")
  events_path <- file.path(dir, "sub-01_ses-01_task-video_recording-eyetracking_physio_desc-events.tsv")
  write.table(fx$data, preproc_path, sep = "\t", row.names = FALSE)
  write.table(fx$events[0, , drop = FALSE], events_path, sep = "\t", row.names = FALSE)

  expect_error(
    result <- filterByEvent(preproc_path, startEvent = "StimOn", endEvent = "StimOff"),
    NA
  )
  expect_equal(nrow(result), 0)
})

test_that("filterByEvent errors clearly when a *_preproc.tsv-shaped file exists but has no recordingTimestamp_ms column", {
  skip_on_cran()
  dir <- tempfile("filterByEvent_bad_preproc_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  bad_preproc_path <- file.path(dir, "sub-01_ses-01_task-video_recording-eyetracking_physio_desc-preproc.tsv")
  write.table(data.frame(notTheRightColumn = 1:3), bad_preproc_path, sep = "\t", row.names = FALSE)

  expect_error(
    filterByEvent(bad_preproc_path, startEvent = "Marker"),
    "no recordingTimestamp_ms column"
  )
})

test_that("filterByEvent returns a 0-row result (not an error) for a real directory with zero matching processed files", {
  skip_on_cran()
  empty_root <- tempfile("filterByEvent_empty_dir_")
  dir.create(empty_root)
  on.exit(unlink(empty_root, recursive = TRUE), add = TRUE)

  result <- NULL
  expect_error(result <- filterByEvent(empty_root, startEvent = "Marker"), NA)
  expect_equal(nrow(result), 0)
})

test_that("filterByEvent restricts to a specific eventValue via the eventValue argument", {
  skip_on_cran()
  dir <- tempfile("filterByEvent_eventvalue_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  fx <- write_filter_by_event_single_file(dir)

  result <- filterByEvent(fx$preproc_path, startEvent = "StimOn", endEvent = "StimOff", eventValue = "B")
  expect_true(nrow(result) > 0)
  expect_equal(unique(result$stimulus), "B")
})
