#' Launch the eyeQuality app
#'
#' Launches the combined eyeQuality Shiny app (P10-12: Setup & Run and
#' Analyze / QC Explorer are tabs of one app/process, not two separately
#' launched apps). The app always opens on its Setup & Run tab, for choosing
#' a data directory, previewing which files a batch run would find (matched
#' file count, sample filenames, and skipped-item count) via
#' `listBidsFiles()`, and launching an `eyeQualityBatch()` run against that
#' directory in the background, with a basic running/done/failed status
#' display. A run started here can be reviewed in the Analyze tabs without
#' leaving this app -- see the "Review results in Analyze tabs" button shown
#' once a run finishes. The Analyze / QC Explorer tabs load every
#' `qcsummary.tsv` output found under a chosen output directory (see
#' `saveFiles()`/`eyeQualityBatch()`'s output naming convention) into a
#' single sortable/filterable table.
#'
#' This function requires the union of both tab groups' own Suggests-only
#' dependencies (`shiny`, `shinyFiles`, `future`, `promises` for Setup & Run;
#' `DT`, `plotly` for Analyze / QC Explorer), none of which are installed
#' automatically with this package (see `Suggests` in `DESCRIPTION`), since
#' they're only needed for this optional interface, not the core
#' preprocessing pipeline. Install them separately to use this function.
#' `plotly` backs the "Gaze Explorer" tab's interactive trajectory/AOI view
#' specifically.
#'
#' @param initialDirectory optional path to pre-populate the Analyze tabs'
#'   output directory field with on startup (e.g. an existing output
#'   directory produced by a script-driven `eyeQualityBatch()` run, not this
#'   app's own Setup & Run tab). The user still needs to click "Load
#'   qcsummary files" themselves -- this only saves them re-navigating the
#'   directory picker, it does not auto-load. `NULL` (default) leaves the
#'   field empty, same as launching with no argument.
#' @param ... additional arguments passed through to `shiny::runApp()` (e.g.
#'   `launch.browser`, `port`).
#'
#' @return Nothing; called for the side effect of launching a Shiny app in
#'   the current R session. Does not return until the app is closed.
#' @export
#'
#' @examples
#' \dontrun{
#' eyeQualityApp()
#'
#' # pre-populated with an existing output directory, for use from the
#' # Analyze / QC Explorer tabs
#' eyeQualityApp(initialDirectory = "/path/to/study/data")
#' }
eyeQualityApp <- function(initialDirectory = NULL, ...) {
  if (!requireNamespace("shiny", quietly = TRUE) ||
    !requireNamespace("shinyFiles", quietly = TRUE) ||
    !requireNamespace("future", quietly = TRUE) ||
    !requireNamespace("promises", quietly = TRUE) ||
    !requireNamespace("DT", quietly = TRUE) ||
    !requireNamespace("plotly", quietly = TRUE)) {
    stop(
      "eyeQualityApp() requires the 'shiny', 'shinyFiles', 'future', 'promises', ",
      "'DT', and 'plotly' packages, which are not installed. Install them with ",
      'install.packages(c("shiny", "shinyFiles", "future", "promises", "DT", "plotly")).',
      call. = FALSE
    )
  }
  if (!is.null(initialDirectory) &&
    (!is.character(initialDirectory) || length(initialDirectory) != 1 || is.na(initialDirectory))) {
    stop("eyeQualityApp(): 'initialDirectory' must be NULL or a single path string.", call. = FALSE)
  }

  app_dir <- system.file("shiny-apps", "app", package = "eyeQuality")
  if (!nzchar(app_dir)) {
    stop(
      "Could not find the eyeQuality app directory. Try re-installing 'eyeQuality'.",
      call. = FALSE
    )
  }

  # app_initialTab: read by app.R's navbarPage(selected = ...) so this app
  # always opens on the Setup & Run tab.
  #
  # analyze_initialDirectory: the only way to hand a value into an app
  # launched via runApp(appDir) (rather than a shinyApp(ui, server) object
  # built directly here) is shiny::shinyOptions()/getShinyOption() -- a
  # process-wide, not session-scoped, key-value store Shiny itself provides
  # for exactly this kind of "parameterize an app directory launch" case.
  # app.R reads this back via getShinyOption("analyze_initialDirectory", NULL).
  # Pre-P10-12 (when Setup and Analyze were two separate processes) this was
  # also how a finished Setup run's output directory reached the Analyze
  # app; that in-process hand-off is now direct (see app.R's
  # input$review_in_analyze), so this option is only still needed for the
  # "open pointed at an existing directory from outside this app" case
  # described above.
  shiny::shinyOptions(
    app_initialTab = "Setup & Run",
    analyze_initialDirectory = initialDirectory
  )

  shiny::runApp(app_dir, ...)
}
