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
      # Live-monitoring mode: lets this app be pointed at the same output
      # directory a Setup app run (P9-05/P9-06, a separate process/browser
      # tab) is currently writing qcsummary.tsv files into, and watch results
      # accumulate without repeatedly re-clicking "Load qcsummary files"
      # above. Deliberately gated on having already clicked Load at least
      # once (see has_loaded_once() below) -- there's no directory to poll
      # yet otherwise, and checking this box before that point would have
      # nothing to do until a real Load click happens anyway.
      checkboxInput(
        "autoRefresh",
        "Auto-refresh (poll this directory for new/updated qcsummary files)",
        value = FALSE
      ),
      numericInput(
        "autoRefreshIntervalSec", "Auto-refresh interval (seconds)",
        value = 5, min = 1, step = 1
      ),
      uiOutput("auto_refresh_status"),
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
          # P10-05: export every recording currently flagged by any
          # configured threshold (qc_table_flagged()'s own qc_flag, the same
          # source of truth the highlighting above reads) as a CSV, for
          # handing off to a colleague or using as an exclusion/re-review
          # list. See build_flagged_export_table() (helpers.R) and the
          # downloadHandler below for why "flagged by any threshold" is the
          # default rather than a single metric.
          downloadButton("download_flagged_csv", "Export flagged for review (CSV)"),
          br(), br(),
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
        ),
        # P10-04: cross-file comparison, distinct from the two views above --
        # "QC table" is one row per metric per file (every file, every
        # metric, at once) and "Plots" is every channel for one file at a
        # time; this tab is one metric (or a handful) across every file at
        # once, for spotting outlier files without scanning the long table
        # row by row.
        tabPanel(
          "Compare files",
          br(),
          p(
            "Compare how loaded files stack up against each other on a chosen QC metric, ",
            "using the same thresholds configured in the sidebar."
          ),
          uiOutput("compare_status_ui"),
          # Narrows both the bar chart and the wide table below down to one
          # or more runs, since the same subject/session sometimes gets
          # reprocessed under a different batch_name (a re-run with
          # different parameters, or a genuinely repeated recording -- see
          # build_qc_comparison_plot()'s own comment on the bar-stacking bug
          # this caused before that fix). Multi-select, defaulting to every
          # currently loaded batch_name selected (see the observeEvent(manual_load_trigger(), ...)
          # below), so a user narrows down from "everything" rather than
          # starting from an empty comparison. The "(none)" choice stands in
          # for derive_batch_name()'s NA_character_ case (helpers.R) --
          # eyeQuality()'s single-file naming form has no batchName at all,
          # which is a real, expected case here, not a data error.
          selectizeInput(
            "compare_batch_name_filter", "Filter to batch_name (run)",
            choices = character(0), multiple = TRUE
          ),
          h4("Metric across files"),
          selectInput("compare_metric_plot", "QC metric", choices = character(0)),
          # height = "auto": paired with renderPlot(..., height = function() ...)
          # below -- a fixed pixel height here made the bar chart illegible
          # once the loaded file count grew past what fits in ~500px (found
          # in field testing with 100+ files), so the container instead
          # sizes to whatever the server-computed height function returns
          # for the currently selected metric's row count.
          plotOutput("compare_plot", height = "auto"),
          hr(),
          h4("Side-by-side comparison table"),
          p(
            class = "text-muted",
            "One row per file, one column per selected metric. Cells are highlighted the same way ",
            "as the QC table tab for any metric with a configured threshold."
          ),
          selectizeInput(
            "compare_metrics_table", "QC metrics to include",
            choices = character(0), multiple = TRUE
          ),
          DTOutput("compare_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  volumes <- c(Home = fs::path_home(), shinyFiles::getVolumes()())
  shinyFiles::shinyDirChoose(input, "directory", roots = volumes, session = session)
  shinyFiles::shinyFileSave(input, "save_config", roots = volumes, session = session)

  # directory_override: P9-07's runAnalyzeApp(initialDirectory = ...) --
  # typically a Setup app run's own output directory, so a user can go
  # straight from "batch run finished" to "review it here" without
  # re-navigating the directory picker. Same bridge pattern as the Setup
  # app's own directory_override (P9-04's "Load config"): shinyDirButton has
  # no supported way to programmatically set a selection (input$directory is
  # a roots-relative path-segment list built entirely client-side by its JS
  # picker, not a plain string an update*Input()-style call can set), so the
  # initial value is tracked here and used as a fallback wherever the user
  # hasn't (yet) made a real picker selection -- a real pick via the button
  # always takes precedence. Pre-populates the field only; the user still
  # clicks "Load qcsummary files" themselves (see runAnalyzeApp()'s own doc
  # comment for why this doesn't auto-load).
  directory_override <- reactiveVal(getShinyOption("analyze_initialDirectory", NULL))

  selected_dir <- reactive({
    if (!is.null(input$directory) && is.list(input$directory)) {
      return(shinyFiles::parseDirPath(volumes, input$directory))
    }
    override <- directory_override()
    if (!is.null(override) && nzchar(override)) {
      return(override)
    }
    character(0)
  })

  output$selected_directory <- renderText({
    dir <- selected_dir()
    if (length(dir) == 0 || !nzchar(dir)) {
      "No directory selected yet."
    } else {
      dir
    }
  })

  # current_load_result: the single source of truth every other reactive in
  # this app reads from -- same shape load_qcsummary_table() itself returns
  # (n_files/table/diagnostic_message/read_errors). Was a plain
  # eventReactive(input$load, ...) before live auto-refresh existed; that
  # only ever recomputes on its one named trigger event, which is exactly
  # why it couldn't also be driven by a timer without this restructuring --
  # a reactiveVal, by contrast, can be updated from two independent places
  # below: a manual "Load qcsummary files" click (observeEvent(input$load, ...)
  # just below) and a live auto-refresh timer tick (the invalidateLater()-based
  # observer further down, past qc_table_proxy). Both write into this same
  # reactiveVal so every downstream reactive/output that reads
  # current_load_result() (load_summary, qc_table_flagged(),
  # compare_metric_choices(), etc.) picks up either kind of update
  # identically, without needing to know which path produced it.
  current_load_result <- reactiveVal(NULL)

  # has_loaded_once: TRUE from the moment the FIRST manual "Load" click
  # successfully validates a directory, and stays TRUE for the rest of the
  # session (even across a later Load of a different directory) -- this is
  # what auto-refresh's own polling observer (below) gates on, so checking
  # the "Auto-refresh" box before any directory has ever been loaded simply
  # does nothing yet rather than trying to poll an unvalidated/nonexistent
  # selected_dir(). Deliberately its own reactiveVal rather than derived as
  # !is.null(current_load_result()) -- the auto-refresh polling observer
  # itself also WRITES current_load_result() on every tick, and gating that
  # same observer's re-trigger condition on a value it also sets up would
  # create a self-invalidating loop (the observer's own tick causing it to
  # immediately want to re-run again, independent of invalidateLater()'s
  # schedule) -- see that observer's own comment for the full explanation.
  has_loaded_once <- reactiveVal(FALSE)

  # last_refresh_time: POSIXct timestamp of the most recent successful load
  # of any kind (manual click or auto-refresh tick), or NULL before the
  # first one -- surfaced via output$auto_refresh_status below so it's
  # always obvious to a user watching a live run whether auto-refresh is on
  # and when it actually last pulled fresh data (a real UX complaint from
  # field testing: it wasn't always clear what state the app was in).
  last_refresh_time <- reactiveVal(NULL)

  # manual_load_trigger: a plain counter incremented once per genuine manual
  # "Load qcsummary files" click, used purely as output$qc_table's OWN
  # rebuild trigger (see that renderDT() below) -- current_load_result()
  # itself can't serve that role once auto-refresh exists, because
  # current_load_result() now also changes on every auto-refresh tick, and
  # rebuilding the whole DT widget on every tick would reset a user's
  # sort/filter/page state every few seconds (exactly the UX regression
  # auto-refresh is supposed to avoid -- see the P10-02 threshold-tweak
  # precedent this design follows: a full rebuild only on a deliberate
  # "start fresh" action, replaceData() for everything else). A counter
  # (rather than e.g. Sys.time()) is enough -- nothing downstream reads its
  # actual value, only that it changed.
  manual_load_trigger <- reactiveVal(0L)

  # The manual "Load qcsummary files" path: validates selected_dir() exactly
  # as the former eventReactive did, then does a full, deliberate "start
  # fresh" load -- this is the one place a NEW directory selection actually
  # takes effect (auto-refresh below always re-polls whatever directory was
  # most recently loaded here, never a directory the user has merely clicked
  # around in the picker without loading).
  observeEvent(input$load, {
    dir <- selected_dir()
    validate(need(length(dir) > 0 && nzchar(dir), "Please choose a directory first."))
    validate(need(dir.exists(dir), "Selected directory does not exist."))

    current_load_result(load_qcsummary_table(dir, recursive = isTRUE(input$recursiveSearch)))
    has_loaded_once(TRUE)
    last_refresh_time(Sys.time())
    manual_load_trigger(isolate(manual_load_trigger()) + 1L)
  })

  # load_result: backward-compatible alias, preserving the exact semantics
  # the former eventReactive(input$load, ...) had for anything reading this
  # app's server-local reactives by name -- most notably the
  # shiny::testServer()-based regression suite, which calls load_result()
  # directly in several places and specifically asserts (in the P9-07
  # seeded-directory tests) that it throws before the first successful
  # manual load, the same way an eventReactive that's never fired does.
  # req(has_loaded_once()) reproduces that "throws pre-load, returns the
  # real value post-load" behavior on top of the reactiveVal-based design
  # above. Nothing in app.R itself reads load_result() -- every internal
  # call site elsewhere in this file already reads current_load_result()
  # directly (see that reactiveVal's own comment above for why the rename
  # was needed once auto-refresh existed); this alias exists purely so
  # existing external callers don't have to change.
  load_result <- reactive({
    req(has_loaded_once())
    current_load_result()
  })

  output$load_summary <- renderUI({
    result <- current_load_result()
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
    result <- current_load_result()
    if (is.null(result$diagnostic_message)) {
      return(NULL)
    }
    div(
      class = "alert alert-warning",
      strong("No qcsummary files found: "), result$diagnostic_message
    )
  })

  output$read_error_ui <- renderUI({
    result <- current_load_result()
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

  # qc_table_flagged: current_load_result()'s table with a "qc_flag" column
  # appended -- TRUE for rows whose metric currently crosses its configured
  # threshold (compute_qc_flags(), helpers.R). Recomputed whenever either
  # the loaded data or the threshold inputs change, but this reactive itself
  # is never rendered directly -- see the initial renderDT() (built once per
  # load, using isolate()'d thresholds so a threshold tweak doesn't rebuild
  # the whole DT widget and reset the user's sort/filter/page state) and the
  # dataTableProxy observer just below (which pushes qc_table_flagged()'s
  # updated "qc_flag" values into the already-rendered widget via
  # DT::replaceData() instead -- the same replaceData() mechanism the
  # auto-refresh polling observer further down also reuses for a live tick's
  # freshly loaded data).
  qc_table_flagged <- reactive({
    result <- current_load_result()
    req(result$table)
    tbl <- result$table
    tbl$qc_flag <- compute_qc_flags(tbl, qc_thresholds())
    tbl
  })

  # Distinct recordings with >=1 currently-flagged qc_metric row -- used
  # below purely for the qc_flag_summary count ("N recordings"). P10-05's
  # actual CSV export (download_flagged_csv below) doesn't wrap this
  # reactive: it needs the per-metric detail (which metric(s), what value)
  # this narrower recording-only shape doesn't carry, so it calls
  # build_flagged_export_table() directly against qc_table_flagged() instead
  # -- see that function's own comment (helpers.R) for why.
  flagged_recordings <- reactive({
    tbl <- qc_table_flagged()
    unique(tbl[tbl$qc_flag, c("recording", "batch_name", "source_file"), drop = FALSE])
  })

  # P10-05: export the currently flagged recordings as a CSV.
  #
  # downloadButton/downloadHandler (rather than shinySaveButton, unlike this
  # app's own P10-07 config save and the Setup app's P9-04 save) -- this
  # export's whole point is to hand a file to a colleague or use as a local
  # exclusion/re-review list, not to write back into the server-local output
  # directory this app read qcsummary.tsv files from, so a browser download
  # matches the actual use case better than a server-side save-as picker.
  # It's also safe against downloadHandler's usual failure mode (an
  # unhandled content() error producing a generic error page): with no data
  # loaded or nothing flagged, build_flagged_export_table() returns NULL and
  # content() below just writes a header-only CSV instead of erroring, so
  # there's no invalid-input case left for content() to throw on. Note that
  # qc_table_flagged() itself does req(result$table), which throws a
  # shiny.silent.error/validation condition (rather than returning NULL) if
  # no directory has ever been loaded yet -- fine inside a render context
  # (Shiny swallows it), but downloadHandler's content() runs outside that
  # flush cycle, so an uncaught version of that condition here would surface
  # as a broken download. The tryCatch below normalizes that case to the
  # same NULL that "loaded, nothing flagged" already produces.
  #
  # "Flagged by any configured threshold" (qc_table_flagged()'s qc_flag
  # column, the same one the QC table's row highlighting and
  # flagged_recordings() above already read) rather than a single metric --
  # the most useful default for a triage/exclusion list is "does this
  # recording need a second look for any reason," not one threshold at a
  # time; a reviewer who only cares about one metric can already isolate
  # that in the QC table's own filter row before deciding whether to trust
  # this export.
  output$download_flagged_csv <- downloadHandler(
    filename = function() {
      paste0("flagged_for_review_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      flagged_tbl <- tryCatch(
        qc_table_flagged(),
        shiny.silent.error = function(e) NULL,
        validation = function(e) NULL
      )
      tbl <- build_flagged_export_table(flagged_tbl)
      if (is.null(tbl)) {
        tbl <- data.frame(
          recording = character(0),
          batch_name = character(0),
          source_file = character(0),
          n_flagged_metrics = integer(0),
          flagged_metrics = character(0),
          flagged_values = character(0)
        )
      }
      utils::write.csv(tbl, file, row.names = FALSE)
    }
  )

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

  # Rebuilt once per data load (not per threshold tweak, see qc_table_flagged()
  # above), with formatStyle() baking a row-highlight rule that reads each
  # row's own "qc_flag" column value at every DataTables draw -- including
  # redraws triggered by replaceData() below, not just this initial render.
  # That's what lets the threshold observer further down update highlighting
  # without rebuilding this widget. "qc_flag" itself is hidden via
  # columnDefs (it's plumbing, not a metric a user would sort/filter on).
  #
  # manual_load_trigger() (NOT current_load_result() or qc_table_flagged())
  # is what gives this render block its reactive dependency, so it rebuilds
  # the whole DT widget on a genuine manual "Load qcsummary files" click --
  # exactly like req(load_result()$table) used to when load_result() was a
  # plain eventReactive that only ever changed on that same click -- but,
  # unlike current_load_result() itself, manual_load_trigger() does NOT also
  # change on every auto-refresh timer tick, so a live-monitoring session
  # doesn't get its whole table (and the user's sort/filter/page state)
  # rebuilt from scratch every few seconds; the auto-refresh polling
  # observer (further down, past qc_table_proxy) instead pushes tick data in
  # place via DT::replaceData(), the same mechanism the threshold observer
  # just below already uses for a threshold tweak. Wrapping the WHOLE body
  # in isolate() (as an earlier version did) strips every dependency,
  # including this one -- the render then only ever executes once, at app
  # startup before any directory is even chosen, produces nothing
  # (current_load_result() is still NULL), and never runs again no matter
  # what gets loaded afterward -- see the regression test guarding exactly
  # this bug. current_load_result() and qc_table_flagged() are therefore
  # both read via isolate() below: this render's only real trigger is
  # manual_load_trigger(), with the isolate()'d reads simply fetching
  # whatever's current at the moment the trigger fires.
  # server = FALSE: this table's realistic scale (tens to low hundreds of
  # files x ~30 metric rows each) doesn't need server-side pagination, and
  # DT::renderDT()'s default of server = TRUE serves rows from whichever
  # data.frame was captured the last time this render actually executed --
  # under the isolate()-everything bug above, that was a single frozen empty
  # snapshot from before any directory was even chosen, which is exactly why
  # the table stayed permanently blank rather than something that would have
  # resolved itself later. Explicit client-side rendering keeps this
  # transparent and directly testable (the rendered data is embedded in the
  # widget itself, not fetched via a separate AJAX round-trip).
  output$qc_table <- renderDT({
    manual_load_trigger()
    result <- isolate(current_load_result())
    req(result$table)
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
  }, server = FALSE)

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

  # auto_refresh_interval_ms: the polling interval below, in milliseconds,
  # for invalidateLater(). Falls back to a 5-second default for the same
  # reasons qc_thresholds() falls back to default_qc_thresholds() above --
  # a momentarily NULL (not rendered yet) or NA/below-1 (user clears or
  # mistypes the numericInput box) value shouldn't produce a runaway
  # near-0ms polling loop or an invalidateLater() call with a nonsensical
  # argument.
  auto_refresh_interval_ms <- reactive({
    secs <- input$autoRefreshIntervalSec
    if (is.null(secs) || is.na(secs) || secs < 1) secs <- 5
    secs * 1000
  })

  # Live auto-refresh: re-polls the SAME directory selected_dir() currently
  # points at on a fixed timer, so a user who has pointed this app at a
  # Setup app run's (P9-05/P9-06, a separate process/browser tab) output
  # directory can watch qcsummary.tsv results accumulate without repeatedly
  # re-clicking "Load qcsummary files". This is the standard shiny
  # conditional-polling pattern (see ?shiny::invalidateLater): invalidateLater()
  # is only ever reached, and therefore only ever reschedules itself, while
  # BOTH input$autoRefresh is checked AND has_loaded_once() is TRUE -- the
  # instant either goes false (box unchecked, or a session that's never
  # loaded anything), the early return() below is hit instead and the
  # invalidation chain simply stops, rather than continuing to tick in the
  # background with nothing to do.
  #
  # selected_dir()/input$recursiveSearch are read via isolate(): this
  # observer's own re-execution is meant to be driven ONLY by the timer and
  # the autoRefresh checkbox (and has_loaded_once(), which only a manual
  # Load click ever sets) -- not by a user idly clicking around the
  # directory picker before deciding whether to load it. The directory
  # actually being polled only ever changes via a real "Load" click (which
  # already does a full current_load_result() reset and manual_load_trigger()
  # bump), never by this observer noticing a new, not-yet-loaded picker
  # selection.
  #
  # qc_table_flagged() is also read via isolate() here, for a different
  # reason: without it, this observer would additionally depend on
  # qc_thresholds() (a pure threshold tweak would then trigger a full
  # directory re-scan, which is both wasteful and not what a threshold tweak
  # should do -- that's the qc_thresholds() observer's own job, just above)
  # and would re-depend on current_load_result() a second time (this
  # observer's own current_load_result(fresh) call two lines above would
  # then re-invalidate itself outside of invalidateLater()'s own schedule --
  # a self-triggering loop bypassing the timer entirely). isolate() here
  # keeps this observer's only real triggers the three named above.
  observe({
    if (!isTRUE(input$autoRefresh) || !isTRUE(has_loaded_once())) {
      return(invisible(NULL))
    }
    invalidateLater(auto_refresh_interval_ms())

    dir <- isolate(selected_dir())
    recursive <- isolate(isTRUE(input$recursiveSearch))
    if (length(dir) == 0 || !nzchar(dir) || !dir.exists(dir)) {
      return(invisible(NULL))
    }

    current_load_result(load_qcsummary_table(dir, recursive = recursive))
    last_refresh_time(Sys.time())

    # Push straight into the already-rendered DT widget via replaceData()
    # (never output$qc_table's own renderDT(), which only rebuilds on
    # manual_load_trigger() -- see that render's own comment) so a user
    # mid-sort/filter/scroll/page on the QC table while watching a live run
    # doesn't get yanked back to the top every tick. tryCatch() rather than
    # req() around qc_table_flagged(): req()'s silent-stop behavior is
    # designed for render/observer bodies that read it directly (as
    # elsewhere in this app), but here it's guarding a value only used
    # conditionally two lines down, so a plain NULL-on-"nothing to flag yet"
    # (e.g. a directory whose files just got deleted mid-run) reads more
    # clearly than relying on req()'s early-return semantics a second time
    # in the same observer.
    tbl <- isolate(tryCatch(
      qc_table_flagged(),
      shiny.silent.error = function(e) NULL,
      validation = function(e) NULL
    ))
    if (!is.null(tbl)) {
      DT::replaceData(qc_table_proxy, tbl, resetPaging = FALSE, rownames = FALSE)
    }
  })

  # auto_refresh_status: makes live-monitoring state and recency explicit --
  # field testing flagged that it wasn't always clear whether the app was
  # actively polling or just sitting on a stale one-shot snapshot. Three
  # distinct states rather than a single on/off badge: off, on-but-not-yet-
  # eligible (checked before any directory has ever been loaded), and
  # actively polling with a concrete last-updated time.
  output$auto_refresh_status <- renderUI({
    if (!isTRUE(input$autoRefresh)) {
      return(div(class = "text-muted", "Auto-refresh is off."))
    }
    if (!isTRUE(has_loaded_once())) {
      return(div(
        class = "alert alert-info",
        "Auto-refresh will begin once you load a directory below."
      ))
    }
    last <- last_refresh_time()
    last_str <- if (is.null(last)) "never" else format(last, "%H:%M:%S")
    div(
      class = "alert alert-success",
      sprintf(
        "Auto-refreshing every %ds -- last updated %s.",
        round(auto_refresh_interval_ms() / 1000), last_str
      )
    )
  })

  # P10-03: row click -> that row's plots. DT's "*_rows_selected" input
  # reports the index into the data.frame passed to datatable() (result$table
  # above), not the currently-displayed/sorted/filtered row order, so this
  # index is stable regardless of any column sort or the "filter = 'top'"
  # search boxes a user has applied -- as long as the underlying data.frame
  # itself doesn't change out from under that index. Auto-refresh breaks
  # that assumption: a newly discovered qcsummary.tsv sorts (per
  # discover_qcsummary_files()'s alphabetical order) wherever its filename
  # lands, potentially ahead of a row a user had already selected, shifting
  # every row after it down by one. Re-deriving source_file by re-indexing
  # sel[1] into whatever current_load_result()$table happens to be at read
  # time (the original, pre-auto-refresh implementation) would then
  # silently resolve to a DIFFERENT file's plots after a tick -- exactly the
  # "wrong file, no crash, no warning" failure this needs to avoid.
  #
  # Instead, the source_file a row click resolves to is captured ONCE, at
  # the moment DT actually reports a selection change, using
  # current_load_result()'s table AS IT STOOD AT THAT MOMENT -- correct by
  # construction, no re-indexing involved -- and stashed in
  # selected_source_file_val, a plain reactiveVal. A later auto-refresh tick
  # updating current_load_result() does NOT re-trigger this resolution (DT
  # doesn't resend qc_table_rows_selected just because the underlying data
  # changed), so the stashed value simply persists across ticks, which is
  # exactly the desired "selection survives a live refresh" behavior. The
  # separate observer just below instead watches for the one case this
  # can't paper over: the selected file being removed from a freshly
  # reloaded table entirely (e.g. deleted/moved mid-run) -- degrading to "no
  # selection" there rather than continuing to show a plot for a file that
  # no longer has a corresponding row at all.
  selected_source_file_val <- reactiveVal(NULL)

  observeEvent(input$qc_table_rows_selected, {
    sel <- input$qc_table_rows_selected
    if (is.null(sel) || length(sel) == 0) {
      selected_source_file_val(NULL)
      return(invisible(NULL))
    }
    result <- current_load_result()
    if (is.null(result$table) || sel[1] < 1 || sel[1] > nrow(result$table)) {
      selected_source_file_val(NULL)
      return(invisible(NULL))
    }
    selected_source_file_val(result$table$source_file[sel[1]])
  }, ignoreNULL = FALSE)

  # Clears a stale selection if an auto-refresh tick's freshly reloaded table
  # no longer contains the previously selected file at all -- e.g. its
  # qcsummary.tsv was deleted, moved, or renamed since it was selected.
  # Deliberately does NOT try to re-resolve to "the same recording under a
  # new index" or similar -- clearing the selection (so plot_result() below
  # falls back to its own "nothing selected" state) is the safe, non-jarring
  # degradation this needs, not a best-effort guess at which row is now the
  # "right" one.
  observeEvent(current_load_result(), {
    sf <- isolate(selected_source_file_val())
    if (is.null(sf)) {
      return(invisible(NULL))
    }
    result <- current_load_result()
    if (is.null(result$table) || !(sf %in% result$table$source_file)) {
      selected_source_file_val(NULL)
    }
  })

  selected_source_file <- reactive({
    selected_source_file_val()
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

  # --- P10-04: cross-file QC metric comparison view ---
  #
  # compare_metric_choices: every distinct qc_metric value in the currently
  # loaded table, sorted -- the full set a user could plot/compare, not just
  # the 3 thresholdable ones (qc_threshold_config), since a reviewer may
  # still want to eyeball a descriptive metric (e.g. blink rate) across files
  # even though this app takes no pass/fail position on it (see
  # qc_threshold_config's own comment on why only 3 metrics are
  # thresholdable). Empty (not NULL) before any data is loaded, so the
  # selectInput/selectizeInput below always get a well-typed `choices`
  # argument.
  compare_metric_choices <- reactive({
    tbl <- current_load_result()$table
    if (is.null(tbl) || !"qc_metric" %in% names(tbl)) {
      return(character(0))
    }
    sort(unique(tbl$qc_metric))
  })

  # Repopulates both the single-metric plot selector and the multi-metric
  # table selector, defaulting to the configured, thresholdable metrics
  # (qc_threshold_config) when present -- those are the ones this app
  # already has an opinion on, so they're the most useful starting point --
  # and falling back to whatever's first alphabetically otherwise.
  #
  # Triggered by manual_load_trigger() rather than current_load_result()
  # itself: a genuine manual Load click (a possibly brand-new directory with
  # a different file set) should reset these selections, but an auto-refresh
  # tick against the SAME directory should not -- the metric set itself
  # never changes file to file (every qcsummary.tsv carries the same fixed
  # ~32 qc_metric rows), so there's nothing to gain by re-populating these
  # selectors on every tick, only a user's in-progress metric choice to lose
  # every few seconds. ignoreInit = TRUE: without it this would also fire
  # once at session startup (manual_load_trigger() starts at a real value,
  # 0L, not an unevaluated eventReactive sentinel like the old load_result()
  # this replaced), which would be a harmless no-op here but is suppressed
  # anyway for clarity -- this observer's whole point is "in response to a
  # load", not "also once before any load has happened".
  observeEvent(manual_load_trigger(), {
    choices <- compare_metric_choices()
    if (length(choices) == 0) {
      updateSelectInput(session, "compare_metric_plot", choices = character(0))
      updateSelectizeInput(session, "compare_metrics_table", choices = character(0), selected = character(0))
      return()
    }

    configured <- unique(qc_threshold_config$qc_metric[qc_threshold_config$qc_metric %in% choices])

    default_plot_metric <- if (length(configured) > 0) configured[1] else choices[1]
    updateSelectInput(session, "compare_metric_plot", choices = choices, selected = default_plot_metric)

    default_table_metrics <- if (length(configured) > 0) configured else choices[1]
    updateSelectizeInput(
      session, "compare_metrics_table",
      choices = choices, selected = default_table_metrics
    )
  }, ignoreInit = TRUE)

  # Repopulates the batch_name (run) filter, same pattern and same
  # manual_load_trigger()-only reasoning as the metric-choice observer just
  # above -- defaulting to every currently loaded batch_name selected
  # (compare_batch_name_choices(), helpers.R, sorted with a trailing
  # "(none)" for derive_batch_name()'s NA_character_ case) so a fresh
  # manual load starts from "show everything". An auto-refresh tick that
  # picks up a brand-new batch_name (e.g. a Setup run started under a second
  # batchName partway through) deliberately does NOT auto-select it into an
  # already-narrowed filter -- the same "don't silently change what a user
  # is looking at mid-session" reasoning as the metric selectors above; a
  # user who wants to see a newly appeared run can select it here manually.
  observeEvent(manual_load_trigger(), {
    choices <- compare_batch_name_choices(current_load_result()$table)
    updateSelectizeInput(
      session, "compare_batch_name_filter",
      choices = choices, selected = choices
    )
  }, ignoreInit = TRUE)

  output$compare_status_ui <- renderUI({
    if (length(compare_metric_choices()) == 0) {
      return(div(class = "alert alert-info", "Load qcsummary files first to compare metrics across files."))
    }
    NULL
  })

  # build_qc_comparison_plot() (helpers.R) is handed qc_table_flagged() --
  # not current_load_result()$table directly -- so this plot's bar coloring always
  # matches the QC table tab's own row highlighting for the same metric
  # (both trace back to the same compute_qc_flags() call), rather than this
  # view recomputing pass/fail independently and risking drift from P10-02's
  # flagging. Narrowed to the batch_name(s) currently selected in the filter
  # above via filter_by_batch_name() (helpers.R) before being handed to
  # build_qc_comparison_plot() -- that function stays agnostic to which
  # batch_name(s) are in view, the same way it's agnostic to which directory
  # was loaded.
  output$compare_plot <- renderPlot(
    {
      metric <- input$compare_metric_plot
      req(metric, nzchar(metric))
      tbl <- qc_table_flagged()
      req(tbl)
      tbl <- filter_by_batch_name(tbl, input$compare_batch_name_filter)
      plot <- build_qc_comparison_plot(tbl, metric, qc_thresholds())
      req(plot)
      plot
    },
    # Scales with the number of bars the currently selected metric will
    # produce, rather than a fixed pixel height -- a static height squeezed
    # 100+ files' bars/labels down to illegibility in field testing. 28px
    # per bar (roomy enough for the legible font sizes set in
    # build_qc_comparison_plot()) plus fixed space for the title/axis, with
    # a 500px floor so a small file count doesn't render a cramped sliver.
    # Filtered by the batch_name selection the same way the plot itself is,
    # above, so the container height matches the actual bar count being
    # drawn rather than the full, unfiltered file count.
    height = function() {
      metric <- input$compare_metric_plot
      tbl <- qc_table_flagged()
      if (is.null(tbl) || is.null(metric) || !nzchar(metric)) {
        return(500)
      }
      tbl <- filter_by_batch_name(tbl, input$compare_batch_name_filter)
      n <- sum(tbl$qc_metric == metric, na.rm = TRUE)
      max(500, n * 28 + 120)
    }
  )

  # Wide comparison table: one row per file, one column per selected metric.
  # Built from current_load_result()$table (not qc_table_flagged()) since qc_flag is
  # a per-row (per metric-per-file), not per-column, concept -- once
  # pivoted wide there's no single qc_flag value per file to carry over.
  # Threshold crossing is instead shown per output *column* below via
  # DT::formatStyle(), reusing qc_threshold_config's own direction/threshold
  # definitions directly (the same source of truth compute_qc_flags() reads)
  # rather than a second flagging mechanism. Narrowed to the batch_name(s)
  # currently selected in the filter above via filter_by_batch_name()
  # (helpers.R) before pivoting -- same reasoning as output$compare_plot
  # above, build_qc_comparison_table() itself stays agnostic to which
  # batch_name(s) are in view.
  output$compare_table <- renderDT({
    metrics <- input$compare_metrics_table
    validate(need(length(metrics) > 0, "Select at least one QC metric above."))
    filtered <- filter_by_batch_name(current_load_result()$table, input$compare_batch_name_filter)
    tbl <- build_qc_comparison_table(filtered, metrics)
    validate(need(!is.null(tbl), "No data available for the selected metric(s)."))

    thresholds <- qc_thresholds()
    dt <- DT::datatable(
      tbl,
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 25, scrollX = TRUE)
    )

    # One formatStyle() call per configured metric that made it into this
    # table's columns -- styleInterval()'s two-color order depends on
    # direction, matching compute_qc_flags()'s own min/max semantics: "min"
    # (higher is better) flags below the threshold, so the low-value color
    # comes first; "max" (lower is better) flags above it, so the high-value
    # color comes second.
    for (i in seq_len(nrow(qc_threshold_config))) {
      cfg <- qc_threshold_config[i, ]
      if (!(cfg$qc_metric %in% names(tbl))) {
        next
      }
      threshold_value <- thresholds[[cfg$threshold_id]]
      if (is.null(threshold_value) || is.na(threshold_value)) {
        next
      }
      colors <- if (identical(cfg$direction, "min")) {
        c("#fbe3e4", "white")
      } else {
        c("white", "#fbe3e4")
      }
      dt <- dt %>% DT::formatStyle(cfg$qc_metric, backgroundColor = DT::styleInterval(threshold_value, colors))
    }

    metric_cols <- setdiff(names(tbl), c("recording", "batch_name", "source_file"))
    dt %>% DT::formatPercentage(metric_cols, 1)
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
