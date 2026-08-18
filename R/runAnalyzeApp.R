#' Launch the Analyze / QC Explorer Shiny app
#'
#' Launches a Shiny app for choosing an output directory and loading every
#' `qcsummary.tsv` output found under it (see `saveFiles()`/
#' `eyeQualityBatch()`'s output naming convention) into a single
#' sortable/filterable table.
#'
#' This app requires the `shiny`, `shinyFiles`, and `DT` packages, which are
#' not installed automatically with this package (see `Suggests` in
#' `DESCRIPTION`) since they're only needed for this optional interface, not
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
#' runAnalyzeApp()
#' }
runAnalyzeApp <- function(...) {
  if (!requireNamespace("shiny", quietly = TRUE) ||
    !requireNamespace("shinyFiles", quietly = TRUE) ||
    !requireNamespace("DT", quietly = TRUE)) {
    stop(
      "runAnalyzeApp() requires the 'shiny', 'shinyFiles', and 'DT' ",
      "packages, which are not installed. Install them with ",
      'install.packages(c("shiny", "shinyFiles", "DT")).',
      call. = FALSE
    )
  }

  app_dir <- system.file("shiny-apps", "analyze", package = "eyeQuality")
  if (!nzchar(app_dir)) {
    stop(
      "Could not find the Analyze app directory. Try re-installing 'eyeQuality'.",
      call. = FALSE
    )
  }

  shiny::runApp(app_dir, ...)
}
