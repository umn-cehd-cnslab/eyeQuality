# eyeQuality Setup app: pick a data directory and preview which files a
# batch run would process, before committing to an actual run.
#
# Launch via eyeQuality::runSetupApp() rather than sourcing this file
# directly -- that wrapper checks for the shiny/shinyFiles dependencies
# (Suggests, not installed automatically with the package) and resolves this
# app's installed location.

library(shiny)
library(shinyFiles)

source("helpers.R", local = TRUE)

ui <- fluidPage(
  titlePanel("eyeQuality: Setup & Run"),
  p(
    "Choose the top-level directory containing your eye-tracking data files, ",
    "then preview which files a batch run would find before starting one. ",
    "Launching a batch run from this app is not yet available -- for now, ",
    "use this preview to confirm your directory layout and matching options ",
    "are correct, then run ", code("eyeQualityBatch()"), " directly."
  ),
  sidebarLayout(
    sidebarPanel(
      shinyDirButton(
        "directory",
        "Choose data directory",
        "Select the top-level directory containing your eye-tracking data"
      ),
      br(), br(),
      verbatimTextOutput("selected_directory"),
      hr(),
      radioButtons(
        "layout",
        "Directory layout",
        choices = c(
          "BIDS-like (sub-XX/ses-XX/...)" = "bids",
          "Custom / glob pattern" = "glob"
        ),
        selected = "bids"
      ),
      conditionalPanel(
        condition = "input.layout == 'bids'",
        textInput(
          "subjectPattern_regex",
          "Subject directory pattern (regex, blank = every subfolder)",
          value = "sub-[A-Z0-9]+"
        ),
        textInput(
          "sessionPattern_regex",
          "Session directory pattern (regex, blank = no session subfolder)",
          value = "ses-[0-9]+"
        ),
        checkboxInput("recursiveSearch", "Search each subject/session directory recursively", value = FALSE)
      ),
      conditionalPanel(
        condition = "input.layout == 'glob'",
        textInput(
          "pathPattern",
          "Path pattern (glob, relative to directory)",
          value = "**/*.tsv",
          placeholder = "e.g. */*.tsv or **/*.tsv"
        ),
        textInput(
          "excludePattern_regex",
          "Exclude pattern (regex, optional)",
          value = "",
          placeholder = "e.g. derivatives"
        )
      ),
      textInput("modalityPattern_regex", "File name pattern (regex, optional -- default matches *.tsv)", value = ""),
      actionButton("preview", "Preview matched files", class = "btn-primary")
    ),
    mainPanel(
      h4("Dry-run preview"),
      uiOutput("preview_summary"),
      h5("Sample filenames"),
      verbatimTextOutput("preview_samples"),
      h5("Skipped items"),
      verbatimTextOutput("preview_skipped")
    )
  )
)

server <- function(input, output, session) {
  volumes <- c(Home = fs::path_home(), shinyFiles::getVolumes()())
  shinyFiles::shinyDirChoose(input, "directory", roots = volumes, session = session)

  selected_dir <- reactive({
    if (is.null(input$directory) || !is.list(input$directory)) {
      return(character(0))
    }
    shinyFiles::parseDirPath(volumes, input$directory)
  })

  output$selected_directory <- renderText({
    dir <- selected_dir()
    if (length(dir) == 0 || !nzchar(dir)) {
      "No directory selected yet."
    } else {
      dir
    }
  })

  preview_result <- eventReactive(input$preview, {
    dir <- selected_dir()
    validate(need(length(dir) > 0 && nzchar(dir), "Please choose a directory first."))
    validate(need(dir.exists(dir), "Selected directory does not exist."))
    if (identical(input$layout, "glob")) {
      validate(need(
        nzchar(input$pathPattern),
        "Path pattern is required for glob layout (e.g. '*/*.tsv' or '**/*.tsv')."
      ))
    }

    build_dry_run_preview(
      directory = dir,
      layout = input$layout,
      subjectPattern_regex = if (nzchar(input$subjectPattern_regex)) input$subjectPattern_regex else NULL,
      sessionPattern_regex = if (nzchar(input$sessionPattern_regex)) input$sessionPattern_regex else NULL,
      modalityPattern_regex = if (nzchar(input$modalityPattern_regex)) input$modalityPattern_regex else NULL,
      recursiveSearch = isTRUE(input$recursiveSearch),
      pathPattern = if (nzchar(input$pathPattern)) input$pathPattern else NULL,
      excludePattern_regex = if (nzchar(input$excludePattern_regex)) input$excludePattern_regex else NULL
    )
  })

  output$preview_summary <- renderUI({
    result <- preview_result()
    tagList(
      p(strong(sprintf("Matched files: %d", result$matched_count))),
      p(strong(sprintf("Skipped items: %d", result$skipped_count)))
    )
  })

  output$preview_samples <- renderPrint({
    result <- preview_result()
    if (result$matched_count == 0) {
      cat("No files matched.\n")
    } else {
      cat(paste(" -", result$sample_files), sep = "\n")
      if (result$matched_count > length(result$sample_files)) {
        cat(sprintf("\n... and %d more\n", result$matched_count - length(result$sample_files)))
      }
    }
  })

  output$preview_skipped <- renderPrint({
    result <- preview_result()
    if (result$skipped_count == 0) {
      cat("Nothing skipped.\n")
    } else {
      cat(paste(" -", result$skipped_items), sep = "\n")
    }
  })
}

shinyApp(ui = ui, server = server)
