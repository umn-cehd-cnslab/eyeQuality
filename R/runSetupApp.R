#' Launch the eyeQuality app, opening on its Setup & Run tab
#'
#' Launches the combined eyeQuality Shiny app (P10-12: Setup & Run and
#' Analyze / QC Explorer are tabs of one app/process, not two separately
#' launched apps) for choosing a data directory, previewing which files a
#' batch run would find (matched file count, sample filenames, and
#' skipped-item count) via `listBidsFiles()`, and launching an
#' `eyeQualityBatch()` run against that directory in the background, with a
#' basic running/done/failed status display. A run started here can be
#' reviewed in the Analyze tabs without leaving this app -- see the "Review
#' results in Analyze tabs" button shown once a run finishes.
#'
#' This is one of two entry points into the same app (`runAnalyzeApp()` is
#' the other) -- both launch the identical `shiny::runApp()` process, differing
#' only in which tab starts selected and, for `runAnalyzeApp()`, an optional
#' pre-populated output directory. Since both tab groups now live in one
#' process, this function requires the union of both tab groups' own
#' Suggests-only dependencies (`shiny`, `shinyFiles`, `future`, `promises` for
#' Setup & Run; `DT`, `plotly` for Analyze / QC Explorer), none of which are
#' installed automatically with this package (see `Suggests` in
#' `DESCRIPTION`), since they're only needed for this optional interface, not
#' the core preprocessing pipeline. Install them separately to use this
#' function.
#'
#' @param ... additional arguments passed through to `shiny::runApp()` (e.g.
#'   `launch.browser`, `port`).
#'
#' @return Nothing; called for the side effect of launching a Shiny app in
#'   the current R session. Does not return until the app is closed.
#' @export
#'
#' @examples
#' \dontrun{
#' runSetupApp()
#' }
runSetupApp <- function(...) {
  if (!requireNamespace("shiny", quietly = TRUE) ||
    !requireNamespace("shinyFiles", quietly = TRUE) ||
    !requireNamespace("future", quietly = TRUE) ||
    !requireNamespace("promises", quietly = TRUE) ||
    !requireNamespace("DT", quietly = TRUE) ||
    !requireNamespace("plotly", quietly = TRUE)) {
    stop(
      "runSetupApp() requires the 'shiny', 'shinyFiles', 'future', 'promises', ",
      "'DT', and 'plotly' packages, which are not installed. Install them with ",
      'install.packages(c("shiny", "shinyFiles", "future", "promises", "DT", "plotly")).',
      call. = FALSE
    )
  }

  app_dir <- system.file("shiny-apps", "app", package = "eyeQuality")
  if (!nzchar(app_dir)) {
    stop(
      "Could not find the eyeQuality app directory. Try re-installing 'eyeQuality'.",
      call. = FALSE
    )
  }

  # app_initialTab: read by app.R's navbarPage(selected = ...) so this entry
  # point opens on the Setup & Run tab -- see R/runAnalyzeApp.R for the
  # sibling entry point that selects the other tab instead. Same
  # shinyOptions()/getShinyOption() mechanism P9-07 already used for
  # analyze_initialDirectory (still supported, see runAnalyzeApp()).
  shiny::shinyOptions(app_initialTab = "Setup & Run")

  shiny::runApp(app_dir, ...)
}
