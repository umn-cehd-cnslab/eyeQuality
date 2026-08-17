#' Launch the Setup & Run Shiny app
#'
#' Launches a Shiny app for choosing a data directory and previewing which
#' files a batch run would find (matched file count, sample filenames, and
#' skipped-item count) via `listBidsFiles()`, before committing to an actual
#' `eyeQualityBatch()` run.
#'
#' This app requires the `shiny` and `shinyFiles` packages, which are not
#' installed automatically with this package (see `Suggests` in
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
#' runSetupApp()
#' }
runSetupApp <- function(...) {
  if (!requireNamespace("shiny", quietly = TRUE) ||
    !requireNamespace("shinyFiles", quietly = TRUE)) {
    stop(
      "runSetupApp() requires the 'shiny' and 'shinyFiles' packages, ",
      "which are not installed. Install them with ",
      'install.packages(c("shiny", "shinyFiles")).',
      call. = FALSE
    )
  }

  app_dir <- system.file("shiny-apps", "setup", package = "eyeQuality")
  if (!nzchar(app_dir)) {
    stop(
      "Could not find the Setup app directory. Try re-installing 'eyeQuality'.",
      call. = FALSE
    )
  }

  shiny::runApp(app_dir, ...)
}
