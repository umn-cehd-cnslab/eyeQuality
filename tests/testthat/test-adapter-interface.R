# Regression coverage for the three P3-10 verbose-diagnostic helpers defined
# in R/adapter-interface.R (`.emit_diagnostic()`, `.diagnose_consecutive_runs()`,
# `.diagnose_all_na_columns()`). These are the shared building blocks every
# adapter method and pipeline stage (classifyBlinks(), eyeSelection(),
# removeOffscreenGaze(), interpolateGaze()) calls to emit opt-in diagnostics,
# so bugs here would silently ripple through every one of those call sites.
# Diagnostics are emitted via print() (not message()), matching this
# package's existing progress-line convention, so expect_output()/
# capture.output() is the right tool -- not expect_message().

# --- .emit_diagnostic() -----------------------------------------------------

test_that(".emit_diagnostic prints the text prefixed with '[verbose] ' when verbose = TRUE", {
  expect_output(.emit_diagnostic("hello world", verbose = TRUE), "\\[verbose\\] hello world")
})

test_that(".emit_diagnostic is a silent no-op when verbose = FALSE", {
  expect_silent(.emit_diagnostic("hello world", verbose = FALSE))
})

test_that(".emit_diagnostic defaults to verbose = FALSE (silent) when the argument is omitted", {
  expect_silent(.emit_diagnostic("hello world"))
})

test_that(".emit_diagnostic returns invisible(NULL)", {
  result <- withVisible(.emit_diagnostic("hello world", verbose = FALSE))
  expect_null(result$value)
  expect_false(result$visible)
})

# --- .diagnose_consecutive_runs() -------------------------------------------

test_that(".diagnose_consecutive_runs is a silent no-op on an empty flag vector", {
  expect_silent(.diagnose_consecutive_runs(logical(0), "empty", verbose = TRUE))
})

test_that(".diagnose_consecutive_runs is a silent no-op when every flagged value is NA (treated as not-flagged)", {
  expect_silent(.diagnose_consecutive_runs(c(NA, NA, NA), "all NA", verbose = TRUE))
})

test_that(".diagnose_consecutive_runs is a silent no-op when nothing is flagged", {
  expect_silent(.diagnose_consecutive_runs(c(FALSE, FALSE, FALSE), "nothing flagged", verbose = TRUE))
})

test_that(".diagnose_consecutive_runs is a silent no-op when verbose = FALSE, even with a long flagged run present", {
  flag <- c(rep(FALSE, 2), rep(TRUE, 10), rep(FALSE, 2))
  expect_silent(.diagnose_consecutive_runs(flag, "should not print", verbose = FALSE))
})

test_that(".diagnose_consecutive_runs reports a total count line but no run-range line for a run just below the default min_run_length (5)", {
  # a run of exactly 4 TRUEs: below the default min_run_length = 5
  flag <- c(rep(FALSE, 2), rep(TRUE, 4), rep(FALSE, 2))
  out <- capture.output(.diagnose_consecutive_runs(flag, "four run", verbose = TRUE))

  expect_true(any(grepl("4 sample\\(s\\) flagged for: four run", out)))
  expect_false(any(grepl("consecutive samples flagged", out)))
})

test_that(".diagnose_consecutive_runs reports both a total count line and a run-range line for a run at exactly the default min_run_length (5)", {
  flag <- c(rep(FALSE, 2), rep(TRUE, 5), rep(FALSE, 2))
  out <- capture.output(.diagnose_consecutive_runs(flag, "five run", verbose = TRUE))

  expect_true(any(grepl("5 sample\\(s\\) flagged for: five run", out)))
  expect_true(any(grepl("rows 3-7: 5 consecutive samples flagged for: five run", out)))
})

test_that(".diagnose_consecutive_runs respects a custom min_run_length, reporting a run-range line for a 4-run when min_run_length = 4", {
  flag <- c(rep(FALSE, 2), rep(TRUE, 4), rep(FALSE, 2))
  out <- capture.output(
    .diagnose_consecutive_runs(flag, "custom threshold", verbose = TRUE, min_run_length = 4)
  )

  expect_true(any(grepl("rows 3-6: 4 consecutive samples flagged for: custom threshold", out)))
})

test_that(".diagnose_consecutive_runs reports each qualifying run separately when there are multiple long runs", {
  flag <- c(rep(TRUE, 6), rep(FALSE, 3), rep(TRUE, 8))
  out <- capture.output(.diagnose_consecutive_runs(flag, "multi run", verbose = TRUE))

  expect_true(any(grepl("rows 1-6: 6 consecutive samples flagged for: multi run", out)))
  expect_true(any(grepl("rows 10-17: 8 consecutive samples flagged for: multi run", out)))
  # total count reflects both runs combined (14), not just the longer one
  expect_true(any(grepl("14 sample\\(s\\) flagged for: multi run", out)))
})

# --- .diagnose_all_na_columns() ---------------------------------------------

test_that(".diagnose_all_na_columns reports only the columns that are entirely NA, not partially-NA or fully-populated columns", {
  data <- data.frame(
    allNA = c(NA, NA, NA),
    partialNA = c(1, NA, 3),
    noNA = c(1, 2, 3)
  )
  out <- capture.output(.diagnose_all_na_columns(data, c("allNA", "partialNA", "noNA"), verbose = TRUE))

  expect_true(any(grepl("column 'allNA' present but 100% NA", out)))
  expect_false(any(grepl("partialNA", out)))
  expect_false(any(grepl("'noNA'", out)))
})

test_that(".diagnose_all_na_columns silently ignores column names not present in data instead of flagging them", {
  data <- data.frame(allNA = c(NA, NA))
  expect_silent(.diagnose_all_na_columns(data, c("doesNotExist"), verbose = TRUE))
})

test_that(".diagnose_all_na_columns is a silent no-op on a zero-row column (no values to be entirely NA)", {
  data <- data.frame(emptyCol = numeric(0))
  expect_silent(.diagnose_all_na_columns(data, c("emptyCol"), verbose = TRUE))
})

test_that(".diagnose_all_na_columns is a silent no-op when verbose = FALSE, even with an all-NA column present", {
  data <- data.frame(allNA = c(NA, NA, NA))
  expect_silent(.diagnose_all_na_columns(data, c("allNA"), verbose = FALSE))
})
