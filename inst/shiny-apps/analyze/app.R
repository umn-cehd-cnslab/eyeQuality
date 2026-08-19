# eyeQuality Analyze / QC Explorer app: pick an output directory and load
# every qcsummary.tsv output found under it into a single sortable/filterable
# table.
#
# Launch via eyeQuality::runAnalyzeApp() rather than sourcing this file
# directly -- that wrapper checks for the shiny/shinyFiles/DT dependencies
# (Suggests, not installed automatically with the package) and resolves this
# app's installed location.

library(shiny)
library(shinyFiles)
library(DT)

source("helpers.R", local = TRUE)

ui <- fluidPage(
  titlePanel("eyeQuality: Analyze / QC Explorer"),
  p(
    "Choose the output directory a batch run wrote (or pointed outputDir at), ",
    "and load every qcsummary.tsv output found under it into one table."
  ),
  sidebarLayout(
    sidebarPanel(
      shinyDirButton(
        "directory",
        "Choose output directory",
        "Select the directory containing qcsummary.tsv outputs"
      ),
      br(), br(),
      verbatimTextOutput("selected_directory"),
      hr(),
      checkboxInput(
        "recursiveSearch",
        "Search subdirectories recursively",
        value = TRUE
      ),
      actionButton("load", "Load qcsummary files", class = "btn-primary")
    ),
    mainPanel(
      uiOutput("load_summary"),
      uiOutput("load_diagnostics"),
      uiOutput("read_error_ui"),
      tabsetPanel(
        tabPanel(
          "QC table",
          br(),
          p("Click a row to view that recording's plots in the \"Plots\" tab."),
          DTOutput("qc_table")
        ),
        tabPanel(
          "Plots",
          br(),
          uiOutput("plot_status_ui"),
          conditionalPanel(
            condition = "output.plot_ready == true",
            h4("Raw gaze, pupil, and distance channels"),
            plotOutput("plot_raw_gaze", height = "700px"),
            h4("Gaze density heatmap"),
            plotOutput("plot_gaze_heatmap", height = "450px"),
            h4("Smoothed gaze position timecourse"),
            plotOutput("plot_gaze_timecourse", height = "350px")
          )
        )
      )
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

  load_result <- eventReactive(input$load, {
    dir <- selected_dir()
    validate(need(length(dir) > 0 && nzchar(dir), "Please choose a directory first."))
    validate(need(dir.exists(dir), "Selected directory does not exist."))

    load_qcsummary_table(dir, recursive = isTRUE(input$recursiveSearch))
  })

  output$load_summary <- renderUI({
    result <- load_result()
    n_read <- if (is.null(result$table)) 0L else length(unique(result$table$source_file))
    tagList(
      h4("Loaded files"),
      p(strong(sprintf(
        "Matched qcsummary.tsv files: %d (%d loaded successfully, %d failed to read)",
        result$n_files, n_read, length(result$read_errors)
      )))
    )
  })

  # Same diagnostic spirit as the Setup app's P7-06 zero-match handling --
  # this app's UI has no console for a user to read list.files()'s silence
  # from, so a blank table needs an explicit reason attached instead.
  output$load_diagnostics <- renderUI({
    result <- load_result()
    if (is.null(result$diagnostic_message)) {
      return(NULL)
    }
    div(
      class = "alert alert-warning",
      strong("No qcsummary files found: "), result$diagnostic_message
    )
  })

  output$read_error_ui <- renderUI({
    result <- load_result()
    if (length(result$read_errors) == 0) {
      return(NULL)
    }
    div(
      class = "alert alert-danger",
      strong(sprintf("%d file(s) failed to load:", length(result$read_errors))),
      tags$ul(
        lapply(names(result$read_errors), function(f) {
          tags$li(sprintf("%s: %s", f, result$read_errors[[f]]))
        })
      )
    )
  })

  output$qc_table <- renderDT({
    result <- load_result()
    req(result$table)
    datatable(
      result$table,
      filter = "top",
      rownames = FALSE,
      selection = "single",
      options = list(
        pageLength = 25,
        scrollX = TRUE
      )
    )
  })

  # P10-03: row click -> that row's plots. DT's "*_rows_selected" input
  # reports the index into the data.frame passed to datatable() (result$table
  # above), not the currently-displayed/sorted/filtered row order, so this
  # index is stable regardless of any column sort or the "filter = 'top'"
  # search boxes a user has applied.
  selected_source_file <- reactive({
    sel <- input$qc_table_rows_selected
    if (is.null(sel) || length(sel) == 0) {
      return(NULL)
    }
    result <- load_result()
    req(result$table)
    result$table$source_file[sel[1]]
  })

  # Resolves the selected row's source_file (a qcsummary.tsv path) to its
  # sibling *_preproc.tsv data file, loads it, and calls
  # generateEyeTrackingPlots() on it -- see load_plot_data() in helpers.R.
  # Returns NULL (rather than a not-ok result) when nothing is selected yet,
  # so downstream outputs can req() this cleanly before a row is clicked.
  plot_result <- reactive({
    sf <- selected_source_file()
    if (is.null(sf)) {
      return(NULL)
    }
    load_plot_data(sf)
  })

  output$plot_ready <- reactive({
    result <- plot_result()
    isTRUE(!is.null(result) && result$ok)
  })
  outputOptions(output, "plot_ready", suspendWhenHidden = FALSE)

  output$plot_status_ui <- renderUI({
    result <- plot_result()
    if (is.null(result)) {
      return(div(class = "alert alert-info", "Select a row in the QC table tab to view its plots here."))
    }
    if (!result$ok) {
      return(div(class = "alert alert-danger", strong("Could not render plots: "), result$error))
    }
    div(class = "alert alert-success", sprintf("Showing plots for: %s", result$preproc_path))
  })

  output$plot_raw_gaze <- renderPlot({
    result <- plot_result()
    req(result, result$ok)
    result$plots[[1]]
  })

  output$plot_gaze_heatmap <- renderPlot({
    result <- plot_result()
    req(result, result$ok)
    result$plots[[2]]
  })

  output$plot_gaze_timecourse <- renderPlot({
    result <- plot_result()
    req(result, result$ok)
    result$plots[[3]]
  })
}

shinyApp(ui = ui, server = server)
