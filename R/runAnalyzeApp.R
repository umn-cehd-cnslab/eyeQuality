#' Launch the Analyze / QC Explorer Shiny app
#'
#' Launches a Shiny app for choosing an output directory and loading every
#' `qcsummary.tsv` output found under it (see `saveFiles()`/
#' `eyeQualityBatch()`'s output naming convention) into a single
#' sortable/filterable table.
#'
#' This app requires the `shiny`, `shinyFiles`, `DT`, and `plotly` packages,
#' which are not installed automatically with this package (see `Suggests` in
#' `DESCRIPTION`) since they're only needed for this optional interface, not
#' the core preprocessing pipeline. Install them separately to use this
#' function. `plotly` backs the "Gaze Explorer" tab's interactive
#' trajectory/AOI view specifically -- every other tab only needs the first
#' three.
#'
#' @param initialDirectory optional path to pre-populate the output
#'   directory field with on startup (e.g. the directory a Setup app
#'   (`runSetupApp()`) batch run just finished writing to). The user still
#'   needs to click "Load qcsummary files" themselves -- this only saves them
#'   re-navigating the directory picker, it does not auto-load. `NULL`
#'   (default) leaves the field empty, same as launching with no argument.
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
#' # pre-populated with a Setup app run's output directory
#' runAnalyzeApp(initialDirectory = "/path/to/study/data")
#' }
runAnalyzeApp <- function(initialDirectory = NULL, ...) {
  if (!requireNamespace("shiny", quietly = TRUE) ||
    !requireNamespace("shinyFiles", quietly = TRUE) ||
    !requireNamespace("DT", quietly = TRUE) ||
    !requireNamespace("plotly", quietly = TRUE)) {
    stop(
      "runAnalyzeApp() requires the 'shiny', 'shinyFiles', 'DT', and 'plotly' ",
      "packages, which are not installed. Install them with ",
      'install.packages(c("shiny", "shinyFiles", "DT", "plotly")).',
      call. = FALSE
    )
  }
  if (!is.null(initialDirectory) &&
    (!is.character(initialDirectory) || length(initialDirectory) != 1 || is.na(initialDirectory))) {
    stop("runAnalyzeApp(): 'initialDirectory' must be NULL or a single path string.", call. = FALSE)
  }

  app_dir <- system.file("shiny-apps", "analyze", package = "eyeQuality")
  if (!nzchar(app_dir)) {
    stop(
      "Could not find the Analyze app directory. Try re-installing 'eyeQuality'.",
      call. = FALSE
    )
  }

  # P9-07: the only way to hand a value into an app launched via runApp(appDir)
  # (rather than a shinyApp(ui, server) object built directly here) is
  # shiny::shinyOptions()/getShinyOption() -- a process-wide, not
  # session-scoped, key-value store Shiny itself provides for exactly this
  # kind of "parameterize an app directory launch" case. app.R reads this
  # back via getShinyOption("analyze_initialDirectory", NULL).
  shiny::shinyOptions(analyze_initialDirectory = initialDirectory)

  shiny::runApp(app_dir, ...)
}
