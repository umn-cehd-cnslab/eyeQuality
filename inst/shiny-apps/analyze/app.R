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

# P10-02: qc_threshold_config (helpers.R) has one row per (threshold_id,
# qc_metric) pair -- interpolated_LeftEye/RightEye deliberately share one
# threshold_id/UI control (see helpers.R's comment on qc_threshold_config).
# The UI only needs one input per unique threshold_id, so it's deduplicated
# once here rather than re-deduplicating inline wherever the UI is built.
qc_threshold_ui_config <- qc_threshold_config[!duplicated(qc_threshold_config$threshold_id), ]

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
      actionButton("load", "Load qcsummary files", class = "btn-primary"),
      hr(),
      h4("QC thresholds"),
      p(
        "Rows for these metrics are highlighted in the QC table when their ",
        "value crosses the threshold below. See P10-02 in helpers.R for why ",
        "only these metrics are thresholdable."
      ),
      # P10-02: one numericInput per unique threshold_id in
      # qc_threshold_config (helpers.R) -- currently 3 (valid_pct, robust_pct,
      # interp_pct). Looping over the shared config here (rather than
      # hardcoding 3 numericInput() calls with separately-typed labels)
      # keeps the UI and the flagging logic's metric/label/direction
      # definitions from being able to drift apart.
      lapply(seq_len(nrow(qc_threshold_ui_config)), function(i) {
        cfg <- qc_threshold_ui_config[i, ]
        numericInput(
          paste0("qc_threshold_", cfg$threshold_id),
          cfg$label,
          value = cfg$default_percent,
          min = 0,
          max = 100,
          step = 1
        )
      })
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
          uiOutput("qc_flag_summary"),
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

  # P10-02: current threshold values, as 0-1 fractions keyed by
  # threshold_id -- matching compute_qc_flags()'s expected shape. Falls back
  # to default_qc_thresholds() for any input that's momentarily NULL (not
  # rendered yet) or NA (user cleared the numericInput box), rather than
  # letting compute_qc_flags() see a hole and skip that threshold_id
  # entirely -- from a QC reviewer's perspective, clearing the box should
  # revert to the documented default, not silently disable flagging.
  qc_thresholds <- reactive({
    defaults <- default_qc_thresholds()
    ids <- names(defaults)
    stats::setNames(
      lapply(ids, function(id) {
        val <- input[[paste0("qc_threshold_", id)]]
        if (is.null(val) || is.na(val)) defaults[[id]] else val / 100
      }),
      ids
    )
  })

  # qc_table_flagged: load_result()'s table with a "qc_flag" column appended
  # -- TRUE for rows whose metric currently crosses its configured
  # threshold (compute_qc_flags(), helpers.R). Recomputed whenever either
  # the loaded data or the threshold inputs change, but this reactive itself
  # is never rendered directly -- see the initial renderDT() (built once per
  # load, using isolate()'d thresholds so a threshold tweak doesn't rebuild
  # the whole DT widget and reset the user's sort/filter/page state) and the
  # dataTableProxy observer just below (which pushes qc_table_flagged()'s
  # updated "qc_flag" values into the already-rendered widget via
  # DT::replaceData() instead).
  qc_table_flagged <- reactive({
    result <- load_result()
    req(result$table)
    tbl <- result$table
    tbl$qc_flag <- compute_qc_flags(tbl, qc_thresholds())
    tbl
  })

  # P10-05 hook: distinct recordings with >=1 currently-flagged qc_metric
  # row, for a future "export flagged file list" feature to consume
  # directly rather than recomputing threshold-crossing logic itself. Not
  # rendered to any output here -- P10-02's scope is the flagging mechanism,
  # not the export UI.
  flagged_recordings <- reactive({
    tbl <- qc_table_flagged()
    unique(tbl[tbl$qc_flag, c("recording", "batch_name", "source_file"), drop = FALSE])
  })

  output$qc_flag_summary <- renderUI({
    tbl <- qc_table_flagged()
    n_flagged_rows <- sum(tbl$qc_flag)
    if (n_flagged_rows == 0) {
      return(div(class = "alert alert-info", "No rows currently cross a configured QC threshold."))
    }
    div(
      class = "alert alert-warning",
      sprintf(
        "%d row(s) across %d recording(s) currently cross a configured QC threshold (highlighted below).",
        n_flagged_rows, nrow(flagged_recordings())
      )
    )
  })

  # Built once per data load (not per threshold tweak, see qc_table_flagged()
  # above), with formatStyle() baking a row-highlight rule that reads each
  # row's own "qc_flag" column value at every DataTables draw -- including
  # redraws triggered by replaceData() below, not just this initial render.
  # That's what lets the threshold observer further down update highlighting
  # without rebuilding this widget. "qc_flag" itself is hidden via
  # columnDefs (it's plumbing, not a metric a user would sort/filter on).
  output$qc_table <- renderDT({
    tbl <- isolate(qc_table_flagged())
    req(tbl)
    qc_flag_col <- which(names(tbl) == "qc_flag") - 1L
    datatable(
      tbl,
      filter = "top",
      rownames = FALSE,
      selection = "single",
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        columnDefs = list(list(visible = FALSE, targets = qc_flag_col))
      )
    ) %>%
      DT::formatStyle(
        columns = "qc_flag",
        target = "row",
        backgroundColor = DT::styleEqual(c(TRUE, FALSE), c("#fbe3e4", "white"))
      )
  })

  qc_table_proxy <- DT::dataTableProxy("qc_table")

  # Reactively re-flag without a full table reload: triggered specifically by
  # qc_thresholds() (not qc_table_flagged()) so this only fires when a
  # threshold input actually changes, never on the data-load event that
  # already triggers renderDT()'s own (isolate()'d) initial render above --
  # avoiding any race between this proxy update and the widget's own
  # creation on first load. Pushes qc_table_flagged()'s updated "qc_flag"
  # column into the already-rendered widget via replaceData(), which
  # triggers a DataTables redraw (re-running the formatStyle rule baked in
  # above against the new qc_flag values) while preserving the widget's
  # current sort/filter/page state -- unlike calling renderDT() again, which
  # would rebuild the widget from scratch.
  observeEvent(qc_thresholds(), {
    tbl <- qc_table_flagged()
    req(tbl)
    DT::replaceData(qc_table_proxy, tbl, resetPaging = FALSE, rownames = FALSE)
  }, ignoreInit = TRUE)

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
