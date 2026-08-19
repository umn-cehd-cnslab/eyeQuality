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
      }),
      hr(),
      # P10-07: save the thresholds above into the same batch_config.yaml the
      # Setup app (P9-04) reads/writes, rather than a second, fragmented
      # config format. "Load config" pulls in an existing file's qcThresholds
      # (if it has one) and repopulates the numericInputs above from it, and
      # also remembers its other (run-parameter) fields so a later "Save
      # config" updates that same file in place instead of dropping them --
      # see build_current_config()/loaded_config_extra below.
      h4("Save / load config"),
      p(
        class = "text-muted",
        "Load an existing batch_config.yaml (e.g. one exported by the Setup app) to repopulate the ",
        "thresholds above from its qcThresholds section, if it has one. Save writes the thresholds ",
        "above back into that same config -- or, with no config loaded yet, a fresh one using the ",
        "study info fields below."
      ),
      fileInput("load_config_file", "Load config", accept = c(".yaml", ".yml")),
      h5("Study info"),
      p(
        class = "text-muted",
        "Required by batch_config.yaml's schema even for a QC-thresholds-only save. Filled in ",
        "automatically by \"Load config\"; fill in by hand for a fresh config."
      ),
      textInput("cfg_batchName", "Batch name", value = ""),
      textInput("cfg_directoryBIDS", "Data directory (directoryBIDS)", value = ""),
      numericInput("cfg_displayDimensionX_mm", "Display width (mm)", value = NA, min = 1),
      numericInput("cfg_displayDimensionY_mm", "Display height (mm)", value = NA, min = 1),
      shinyFiles::shinySaveButton(
        "save_config",
        "Save config...",
        "Save current QC thresholds (and study info) as batch_config.yaml",
        filetype = list(yaml = c("yaml", "yml"))
      ),
      uiOutput("config_io_status")
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
  shinyFiles::shinyFileSave(input, "save_config", roots = volumes, session = session)

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

  # --- P10-07: save/load QC thresholds via the shared batch_config.yaml ---
  #
  # loaded_config_extra: the full config most recently loaded via "Load
  # config" this session (or NULL if none has been), used purely so
  # build_current_config() can carry forward every schema field this app's
  # own form doesn't expose (layout, patterns, eyeSelection_method, etc.) on
  # the next save -- same pattern as the Setup app's own loaded_config_extra
  # (P9-04). Its qcThresholds field, if any, is always the already-filtered
  # (filter_recognized_qc_thresholds(), helpers.R) set kept from the most
  # recent load, so a later resave can never resurrect a dropped/
  # unrecognized entry the app never actually applied.
  loaded_config_extra <- reactiveVal(NULL)
  # config_io_status: list(ok = TRUE/FALSE, message = ...) for the most
  # recent save or load attempt, or NULL before either has happened this
  # session -- same persistent-alert rendering convention as the Setup app.
  config_io_status <- reactiveVal(NULL)

  # build_current_config: loaded_config_extra() (or an empty base, for a
  # fresh config) with the study-info fields and the live threshold values
  # overlaid on top, batch_config.yaml-shaped and ready for
  # write_batch_config(). keep.null = TRUE so an explicitly blanked study-info
  # field (e.g. clearing "Data directory" after loading a config) actually
  # clears that field in the saved config rather than leaving the loaded
  # value in place -- same reasoning as write_batch_config()'s own
  # modifyList() call (R/batchConfig.R).
  build_current_config <- reactive({
    extra <- loaded_config_extra()
    extra <- if (is.null(extra)) list() else extra

    form_fields <- list(
      batchName = blank_to_null(input$cfg_batchName),
      directoryBIDS = blank_to_null(input$cfg_directoryBIDS),
      displayDimensionX_mm = blank_to_null(input$cfg_displayDimensionX_mm),
      displayDimensionY_mm = blank_to_null(input$cfg_displayDimensionY_mm),
      qcThresholds = qc_thresholds_to_percent(qc_thresholds())
    )

    utils::modifyList(extra, form_fields, keep.null = TRUE)
  })

  # shinySaveButton (rather than downloadButton/downloadHandler): matches the
  # Setup app's own P9-04 save control, and for the same reason --
  # write_batch_config() validates before writing anything (R/batchConfig.R),
  # so an incomplete "fresh" save (e.g. no batch name filled in, no config
  # loaded to carry a directoryBIDS forward) is just a caught error and a
  # clear status message here, never a partially-written file.
  observeEvent(input$save_config, {
    if (is.null(input$save_config) || !is.list(input$save_config)) {
      return()
    }
    save_path <- shinyFiles::parseSavePath(volumes, input$save_config)
    if (nrow(save_path) == 0) {
      return()
    }
    dest <- save_path$datapath[1]

    result <- tryCatch(
      {
        eyeQuality::write_batch_config(build_current_config(), dest)
        list(ok = TRUE, message = paste0("Saved config to ", dest))
      },
      error = function(e) {
        list(ok = FALSE, message = paste0("Could not save config: ", conditionMessage(e)))
      }
    )
    config_io_status(result)
  })

  # Load config: read(validate = FALSE) deliberately, unlike the Setup app's
  # own load handler -- this app only cares about qcThresholds (plus
  # carrying forward whatever else the file has for a later resave), not
  # whether the file's run parameters are themselves complete/valid, so a
  # config this app can't fully validate (e.g. one still missing a batch
  # name, saved standalone from this same app) must still load its
  # thresholds rather than being rejected outright. Any qcThresholds entry
  # that isn't a currently recognized threshold_id, or whose value isn't a
  # sane 0-100 percentage, is dropped by filter_recognized_qc_thresholds()
  # (helpers.R) rather than erroring -- covers both a hand-edited typo and a
  # config written by a future eyeQuality version with a metric this version
  # doesn't know about (P10-07's forward-compatibility requirement).
  #
  # Every threshold_id's numericInput is always set here (to the loaded
  # value if kept, otherwise back to its documented default) rather than
  # only updating the ones the file happened to specify -- so "load" fully
  # determines the resulting threshold state, the same way the Setup app's
  # load handler unconditionally sets every form field it owns.
  observeEvent(input$load_config_file, {
    req(input$load_config_file)

    result <- tryCatch(
      {
        config <- eyeQuality::read_batch_config(input$load_config_file$datapath, validate = FALSE)
        list(ok = TRUE, config = config)
      },
      error = function(e) {
        list(ok = FALSE, message = paste0("Could not load config: ", conditionMessage(e)))
      }
    )

    if (!isTRUE(result$ok)) {
      config_io_status(list(ok = FALSE, message = result$message))
      return()
    }

    config <- result$config
    filtered <- filter_recognized_qc_thresholds(config$qcThresholds)
    config$qcThresholds <- filtered$kept
    loaded_config_extra(config)

    updateTextInput(session, "cfg_batchName", value = null_to_blank(config$batchName))
    updateTextInput(session, "cfg_directoryBIDS", value = null_to_blank(config$directoryBIDS))
    updateNumericInput(
      session, "cfg_displayDimensionX_mm",
      value = if (is.null(config$displayDimensionX_mm)) NA else config$displayDimensionX_mm
    )
    updateNumericInput(
      session, "cfg_displayDimensionY_mm",
      value = if (is.null(config$displayDimensionY_mm)) NA else config$displayDimensionY_mm
    )

    defaults_percent <- lapply(default_qc_thresholds(), function(f) f * 100)
    for (id in names(defaults_percent)) {
      kept_value <- filtered$kept[[id]]
      percent_value <- if (!is.null(kept_value)) kept_value else defaults_percent[[id]]
      updateNumericInput(session, paste0("qc_threshold_", id), value = percent_value)
    }

    message <- paste0("Loaded config from ", input$load_config_file$name)
    if (length(filtered$dropped) > 0) {
      message <- paste0(
        message, " (ignored unrecognized/invalid qcThresholds entry(ies): ",
        paste(filtered$dropped, collapse = ", "), ")"
      )
    }
    config_io_status(list(ok = TRUE, message = message))
  })

  output$config_io_status <- renderUI({
    status <- config_io_status()
    if (is.null(status)) {
      return(NULL)
    }
    div(
      class = if (isTRUE(status$ok)) "alert alert-success" else "alert alert-danger",
      status$message
    )
  })
}

shinyApp(ui = ui, server = server)
