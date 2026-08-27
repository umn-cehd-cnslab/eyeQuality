#' Launch the eyeQuality app, opening on its Analyze / QC Explorer tabs
#'
#' Launches the combined eyeQuality Shiny app (P10-12: Setup & Run and
#' Analyze / QC Explorer are tabs of one app/process, not two separately
#' launched apps) for choosing an output directory and loading every
#' `qcsummary.tsv` output found under it (see `saveFiles()`/
#' `eyeQualityBatch()`'s output naming convention) into a single
#' sortable/filterable table.
#'
#' This is one of two entry points into the same app (`runSetupApp()` is the
#' other) -- both launch the identical `shiny::runApp()` process, differing
#' only in which tab starts selected and, here, an optional pre-populated
#' output directory. A batch run started from the Setup & Run tab hands its
#' own output directory to these tabs automatically (via the "Review results
#' in Analyze tabs" button shown once that run finishes) -- `initialDirectory`
#' below exists for the separate case of opening this app already pointed at
#' an existing output directory produced *outside* this app (e.g. by a
#' script-driven `eyeQualityBatch()` run).
#'
#' Since both tab groups now live in one process, this function requires the
#' union of both tab groups' own Suggests-only dependencies (`shiny`,
#' `shinyFiles`, `future`, `promises` for the Setup & Run tab; `DT`, `plotly`
#' for these tabs), none of which are installed automatically with this
#' package (see `Suggests` in `DESCRIPTION`), since they're only needed for
#' this optional interface, not the core preprocessing pipeline. Install them
#' separately to use this function. `plotly` backs the "Gaze Explorer" tab's
#' interactive trajectory/AOI view specifically.
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
#' runAnalyzeApp()
#'
#' # pre-populated with an existing output directory
#' runAnalyzeApp(initialDirectory = "/path/to/study/data")
#' }
runAnalyzeApp <- function(initialDirectory = NULL, ...) {
  if (!requireNamespace("shiny", quietly = TRUE) ||
    !requireNamespace("shinyFiles", quietly = TRUE) ||
    !requireNamespace("future", quietly = TRUE) ||
    !requireNamespace("promises", quietly = TRUE) ||
    !requireNamespace("DT", quietly = TRUE) ||
    !requireNamespace("plotly", quietly = TRUE)) {
    stop(
      "runAnalyzeApp() requires the 'shiny', 'shinyFiles', 'future', 'promises', ",
      "'DT', and 'plotly' packages, which are not installed. Install them with ",
      'install.packages(c("shiny", "shinyFiles", "future", "promises", "DT", "plotly")).',
      call. = FALSE
    )
  }
  if (!is.null(initialDirectory) &&
    (!is.character(initialDirectory) || length(initialDirectory) != 1 || is.na(initialDirectory))) {
    stop("runAnalyzeApp(): 'initialDirectory' must be NULL or a single path string.", call. = FALSE)
  }

  app_dir <- system.file("shiny-apps", "app", package = "eyeQuality")
  if (!nzchar(app_dir)) {
    stop(
      "Could not find the eyeQuality app directory. Try re-installing 'eyeQuality'.",
      call. = FALSE
    )
  }

  # app_initialTab: read by app.R's navbarPage(selected = ...) so this entry
  # point opens on the Analyze / QC Explorer tab -- see R/runSetupApp.R for
  # the sibling entry point that selects the other tab instead.
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
    app_initialTab = "Analyze / QC Explorer",
    analyze_initialDirectory = initialDirectory
  )

  shiny::runApp(app_dir, ...)
}
