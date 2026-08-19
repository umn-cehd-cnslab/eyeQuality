# eyeQuality Setup app: pick a data directory and preview which files a
# batch run would process, before committing to an actual run.
#
# Launch via eyeQuality::runSetupApp() rather than sourcing this file
# directly -- that wrapper checks for the shiny/shinyFiles dependencies
# (Suggests, not installed automatically with the package) and resolves this
# app's installed location.

library(shiny)
library(shinyFiles)
library(future)
library(promises)

source("helpers.R", local = TRUE)
source("background_run.R", local = TRUE)

ui <- fluidPage(
  titlePanel("eyeQuality: Setup & Run"),
  p(
    "Choose the top-level directory containing your eye-tracking data files, ",
    "preview which files a batch run would find, then start the run and ",
    "watch its basic progress here."
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
      hr(),
      h5("Processing parameters"),
      # displayDimensionX_mm/Y_mm: batch_config.yaml (R/batchConfig.R)
      # requires these (no schema-wide default), so a real value is needed
      # here for "Save config" to ever produce a valid file -- defaulted to
      # eyeQualityBatch()'s own built-in defaults (594x344mm), not left
      # blank, so an unedited form is already save-able and already matches
      # what "Start batch run" would use if these fields didn't exist at
      # all.
      numericInput("displayDimensionX_mm", "Display width (mm)", value = 594, min = 1),
      numericInput("displayDimensionY_mm", "Display height (mm)", value = 344, min = 1),
      selectInput(
        "eyeSelection_method",
        "Eye selection method",
        choices = c("Maximize", "Strict", "Left", "Right"),
        selected = "Maximize"
      ),
      numericInput(
        "validityThreshold",
        "Validity threshold (0-1, blank = adapter default)",
        value = NA,
        min = 0,
        max = 1,
        step = 0.05
      ),
      textInput("outputDir", "Output directory (blank = default location alongside each input file)", value = ""),
      textInput("batchName", "Batch name (used to label output files)", value = "run1"),
      actionButton("preview", "Preview matched files", class = "btn-primary"),
      hr(),
      h5("Save / load config"),
      p(class = "text-muted", "Save the settings above to a batch_config.yaml, or load one back in."),
      shinyFiles::shinySaveButton(
        "save_config",
        "Save config...",
        "Save current settings as batch_config.yaml",
        filetype = list(yaml = c("yaml", "yml"))
      ),
      br(), br(),
      fileInput("load_config_file", "Load config", accept = c(".yaml", ".yml")),
      uiOutput("config_io_status")
    ),
    mainPanel(
      h4("Dry-run preview"),
      uiOutput("preview_summary"),
      uiOutput("preview_diagnostics"),
      h5("Sample filenames"),
      verbatimTextOutput("preview_samples"),
      h5("Skipped items"),
      verbatimTextOutput("preview_skipped"),
      hr(),
      h4("Run"),
      uiOutput("launch_ui"),
      uiOutput("run_status")
    )
  )
)

server <- function(input, output, session) {
  volumes <- c(Home = fs::path_home(), shinyFiles::getVolumes()())
  shinyFiles::shinyDirChoose(input, "directory", roots = volumes, session = session)
  shinyFiles::shinyFileSave(input, "save_config", roots = volumes, session = session)

  # DEFAULT_GUI_NUMBER_CORES: the single source of truth for the numberCores
  # value the "Start batch run" launch call below actually uses -- also read
  # by build_current_config() (P9-04, further down) so a config saved
  # without ever loading one from a file still records the real value this
  # run would use, rather than a second hardcoded literal that could drift
  # from the launch call's own.
  DEFAULT_GUI_NUMBER_CORES <- 2L

  # directory_override: P9-04's "Load config" sets a directory string
  # directly from a loaded batch_config.yaml's `directoryBIDS`. shinyFiles
  # has no supported way to programmatically set a shinyDirButton's
  # selection (input$directory is a roots-relative path-segment list built
  # entirely client-side by its JS picker, not a plain string an
  # update*Input()-style call can set) -- so a loaded directory is tracked
  # here instead and used as a fallback wherever the user hasn't (yet, or
  # again) made a real picker selection. A subsequent real pick via the
  # button always takes precedence (see selected_dir() below), so this is
  # purely a "reflect the loaded value until the user overrides it" bridge,
  # not a fake picker selection.
  directory_override <- reactiveVal(NULL)

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

  # P7-06: when a preview matches zero files, listBidsFiles() attaches a
  # diagnostics attribute explaining why (e.g. "found N subfolders but none
  # matched subjectPattern_regex") -- surface it directly, since this app's
  # UI has no console for a user to read print()/message() output from.
  output$preview_diagnostics <- renderUI({
    result <- preview_result()
    if (is.null(result$diagnostic_message)) {
      return(NULL)
    }
    div(
      class = "alert alert-warning",
      strong("Why zero files matched: "), result$diagnostic_message
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

  # --- P9-05: background batch run + polling ---------------------------
  #
  # run_info: identifies the in-flight/most recent run (directory, batch
  # name, expected file count) so poll_batch_progress() can be called
  # without depending on holding a live future/promise reference -- see
  # background_run.R's ensure_future_plan() note on session/tab-close
  # lifecycle for why that independence is intentional.
  run_info <- reactiveValues(directory = NULL, batchName = NULL, n_expected = NULL)
  # start_time: set when the run launches, used only for
  # estimate_remaining_seconds()'s ETA -- not part of the underlying state
  # machine poll_batch_progress() drives (status/n_done/n_failed), so it's
  # tracked separately rather than folded into that polling logic.
  # failed_detail: per-file failure detail (data.frame or NULL), populated
  # once poll_batch_progress()'s n_failed becomes known and positive -- see
  # get_failed_file_details() in background_run.R.
  progress_state <- reactiveValues(
    status = "not started", n_done = 0L, n_failed = NA_integer_, message = NULL,
    start_time = NULL, failed_detail = NULL
  )
  # Held for the life of the session purely so the promise chain below has a
  # persistent reference to keep it from being garbage-collected before it
  # resolves; not otherwise read.
  batch_promise <- reactiveVal(NULL)

  output$launch_ui <- renderUI({
    if (identical(progress_state$status, "running")) {
      return(p(em("Batch run in progress -- see status below.")))
    }

    result <- tryCatch(preview_result(), error = function(e) NULL)
    if (is.null(result) || result$matched_count == 0) {
      return(p(em("Run a preview above with at least one matched file before starting a run.")))
    }

    tagList(
      actionButton("launch", "Start batch run", class = "btn-success")
    )
  })

  observeEvent(input$launch, {
    result <- preview_result()
    dir <- selected_dir()
    batch_name <- input$batchName
    validate(need(nzchar(batch_name), "Batch name is required."))

    run_info$directory <- dir
    run_info$batchName <- batch_name
    run_info$n_expected <- result$matched_count

    progress_state$status <- "running"
    progress_state$n_done <- 0L
    progress_state$n_failed <- NA_integer_
    progress_state$message <- NULL
    progress_state$start_time <- Sys.time()
    progress_state$failed_detail <- NULL

    # P9-04: forward the same layout/pattern arguments the dry-run preview
    # above was already built and validated against (via build_dry_run_preview(),
    # same layout-conditional list(...) shape -- see its P7-06 comment on why
    # this can't be assembled incrementally without dropping explicit NULLs),
    # plus the processing-parameter inputs added alongside "Save config"
    # (display dimensions, eye selection method, validity threshold, output
    # directory). Before this, "Start batch run" silently ignored all of
    # these and ran eyeQualityBatch() against its own listBidsFiles()/
    # eyeQuality() defaults regardless of what the preview above had actually
    # matched against -- a real run could process a different (or empty) set
    # of files than the dry-run preview just showed, most visibly for glob
    # layout (eyeQualityBatch() defaults to bids-layout matching). Discovered
    # while wiring the new form fields through for reproducibility; fixed
    # here rather than left in place, since it directly undermines the
    # "preview accurately previews what a run will do" and "save now, rerun
    # identically later" guarantees this task is otherwise building toward.
    layout_args <- if (identical(input$layout, "bids")) {
      list(
        layout = input$layout,
        subjectPattern_regex = if (nzchar(input$subjectPattern_regex)) input$subjectPattern_regex else NULL,
        sessionPattern_regex = if (nzchar(input$sessionPattern_regex)) input$sessionPattern_regex else NULL,
        recursiveSearch = isTRUE(input$recursiveSearch)
      )
    } else {
      list(
        layout = input$layout,
        pathPattern = if (nzchar(input$pathPattern)) input$pathPattern else NULL,
        excludePattern_regex = if (nzchar(input$excludePattern_regex)) input$excludePattern_regex else NULL
      )
    }

    launch_args <- c(
      list(
        directoryBIDS = dir,
        batchName = batch_name,
        numberCores = DEFAULT_GUI_NUMBER_CORES, # see background_run.R for why this default, not eyeQualityBatch()'s auto-detect
        outputDir = blank_to_null(input$outputDir),
        displayDimensionX_mm = input$displayDimensionX_mm,
        displayDimensionY_mm = input$displayDimensionY_mm,
        eyeSelection_method = input$eyeSelection_method,
        validityThreshold = blank_to_null(input$validityThreshold),
        modalityPattern_regex = if (nzchar(input$modalityPattern_regex)) input$modalityPattern_regex else NULL
      ),
      layout_args
    )

    prom <- do.call(start_background_batch, launch_args)

    prom <- prom %...>% (function(result) {
      final <- poll_batch_progress(run_info$directory, run_info$batchName, run_info$n_expected)
      progress_state$n_done <- final$n_done
      progress_state$n_failed <- final$n_failed
      progress_state$status <- if (identical(result$status, "ok")) "done" else "failed"
      progress_state$message <- result$message
      if (!is.na(final$n_failed) && final$n_failed > 0) {
        progress_state$failed_detail <- get_failed_file_details(run_info$directory, run_info$batchName)
      }
    })
    # Catches both a rejected future (an error escaping eyeQualityBatch()'s
    # own tryCatch entirely) and any error raised inside the %...>% handler
    # above, since catch() is attached to the chained promise, not the
    # original.
    prom <- catch(prom, function(e) {
      progress_state$status <- "failed"
      progress_state$message <- conditionMessage(e)
    })
    batch_promise(prom)
  })

  # Poll while a run is in progress; each tick reschedules itself via
  # invalidateLater() only if still running, so polling stops on its own
  # once progress_state$status flips to "done"/"failed" (set from the
  # promise callback above, independent of this timer).
  observe({
    if (!identical(progress_state$status, "running")) {
      return()
    }

    progress <- poll_batch_progress(run_info$directory, run_info$batchName, run_info$n_expected)
    progress_state$n_done <- progress$n_done
    progress_state$n_failed <- progress$n_failed

    invalidateLater(2000, session)
  })

  # progress_bar_ui: a plain HTML5 <progress> element plus a percentage
  # label, shared by both the "running" and "done" states below. A bare
  # <progress> tag (rather than e.g. shinyWidgets::progressBar()) is
  # deliberate -- this app has no shinyWidgets dependency already, and
  # driving progress from poll_batch_progress()'s external polling rather
  # than a blocking loop rules out shiny::withProgress()/incProgress()
  # regardless, so a plain reactively-updated element is the simplest option
  # that actually fits how progress is discovered here.
  progress_bar_ui <- function(n_done, n_expected) {
    pct <- if (isTRUE(n_expected > 0)) round(100 * n_done / n_expected) else 0
    tagList(
      tags$progress(value = n_done, max = max(n_expected, 1), style = "width: 100%;"),
      p(sprintf("%d%% complete", pct))
    )
  }

  output$run_status <- renderUI({
    switch(progress_state$status,
      "not started" = p(em("No batch run started yet.")),
      "running" = {
        n_remaining <- max(run_info$n_expected - progress_state$n_done, 0)
        eta_secs <- estimate_remaining_seconds(
          progress_state$start_time, progress_state$n_done, run_info$n_expected
        )
        eta_label <- format_duration_seconds(eta_secs)
        tagList(
          p(strong("Batch run in progress...")),
          progress_bar_ui(progress_state$n_done, run_info$n_expected),
          p(sprintf(
            "Processed: %d   Remaining: %d", progress_state$n_done, n_remaining
          )),
          if (!is.na(progress_state$n_failed)) p(sprintf("Failed so far: %d", progress_state$n_failed)),
          if (!is.na(eta_label)) p(sprintf("Estimated time remaining: %s", eta_label))
        )
      },
      "done" = {
        n_failed <- if (is.na(progress_state$n_failed)) 0L else progress_state$n_failed
        tagList(
          p(strong("Batch run complete.")),
          progress_bar_ui(progress_state$n_done, run_info$n_expected),
          p(sprintf(
            "%d of %d files processed (%d failed).",
            progress_state$n_done, run_info$n_expected, n_failed
          )),
          if (n_failed > 0) {
            tagList(
              h5("Failed files"),
              if (is.null(progress_state$failed_detail)) {
                p(em("Failure count is known, but per-file detail could not be read from the batch summary."))
              } else {
                tableOutput("failed_files_table")
              }
            )
          }
        )
      },
      "failed" = tagList(
        p(strong("Batch run did not complete successfully.")),
        p(progress_state$message)
      )
    )
  })

  output$failed_files_table <- renderTable({
    req(progress_state$failed_detail)
    progress_state$failed_detail
  })

  # --- P9-04: save/load named batch_config.yaml configs -----------------
  #
  # loaded_config_extra: the full config most recently loaded via "Load
  # config" this session (or NULL if none has been), used purely so
  # build_batch_config_from_form() can carry forward the schema fields this
  # form has no input for (adapterType, numberCores) on the next save -- see
  # that function's own comment in helpers.R for why those two specifically.
  loaded_config_extra <- reactiveVal(NULL)
  # config_io_status: list(ok = TRUE/FALSE, message = ...) for the most
  # recent save or load attempt, or NULL before either has happened this
  # session. Rendered below as a persistent alert (not a transient
  # notification) so a validation failure - e.g. a hand-edited config
  # missing a required field - stays visible and readable rather than
  # flashing by, per this task's "surface validate_batch_config()'s error
  # message" requirement.
  config_io_status <- reactiveVal(NULL)

  # build_current_config: the form's current values, batch_config.yaml-shaped
  # (see build_batch_config_from_form()), used by both the save handler below
  # and available for anything else that wants "what would Save write right
  # now."
  build_current_config <- reactive({
    dir <- selected_dir()
    build_batch_config_from_form(
      directoryBIDS = if (length(dir) == 0) NULL else dir,
      batchName = input$batchName,
      layout = input$layout,
      subjectPattern_regex = input$subjectPattern_regex,
      sessionPattern_regex = input$sessionPattern_regex,
      recursiveSearch = input$recursiveSearch,
      pathPattern = input$pathPattern,
      excludePattern_regex = input$excludePattern_regex,
      modalityPattern_regex = input$modalityPattern_regex,
      displayDimensionX_mm = input$displayDimensionX_mm,
      displayDimensionY_mm = input$displayDimensionY_mm,
      outputDir = input$outputDir,
      validityThreshold = input$validityThreshold,
      eyeSelection_method = input$eyeSelection_method,
      extra = loaded_config_extra(),
      defaultNumberCores = DEFAULT_GUI_NUMBER_CORES
    )
  })

  # shinySaveButton (rather than downloadButton/downloadHandler): this app
  # already treats the data directory as server-local (shinyDirButton, not a
  # browser upload), so a server-local save-as picker matches its existing
  # model better than a browser download would. It also makes graceful
  # invalid-config handling straightforward: write_batch_config() validates
  # before writing anything (see R/batchConfig.R), so a bad config here is
  # just a caught error and a notification/status message -- never a
  # partially-written file, and never the generic unhandled-error page a
  # downloadHandler's content() throwing would otherwise produce.
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

  # Load config: reads the uploaded file via read_batch_config(validate =
  # TRUE), so a config that doesn't validate cleanly (missing a required
  # field, an out-of-range value, a hand-edited typo) is caught here as a
  # plain error and surfaced via config_io_status - never partially applied
  # to the form. Only on a clean read do any update*Input() calls happen.
  observeEvent(input$load_config_file, {
    req(input$load_config_file)

    result <- tryCatch(
      {
        config <- eyeQuality::read_batch_config(input$load_config_file$datapath, validate = TRUE)
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
    loaded_config_extra(config)

    if (!is.null(config$directoryBIDS) && nzchar(config$directoryBIDS)) {
      directory_override(config$directoryBIDS)
    }

    updateRadioButtons(session, "layout", selected = config$layout)
    updateTextInput(session, "subjectPattern_regex", value = null_to_blank(config$subjectPattern_regex))
    updateTextInput(session, "sessionPattern_regex", value = null_to_blank(config$sessionPattern_regex))
    updateCheckboxInput(session, "recursiveSearch", value = isTRUE(config$recursiveSearch))
    updateTextInput(session, "pathPattern", value = null_to_blank(config$pathPattern))
    updateTextInput(session, "excludePattern_regex", value = null_to_blank(config$excludePattern_regex))
    updateTextInput(session, "modalityPattern_regex", value = null_to_blank(config$modalityPattern_regex))
    updateTextInput(session, "batchName", value = null_to_blank(config$batchName))
    updateNumericInput(session, "displayDimensionX_mm", value = config$displayDimensionX_mm)
    updateNumericInput(session, "displayDimensionY_mm", value = config$displayDimensionY_mm)
    updateTextInput(session, "outputDir", value = null_to_blank(config$outputDir))
    updateNumericInput(
      session, "validityThreshold",
      value = if (is.null(config$validityThreshold)) NA else config$validityThreshold
    )
    updateSelectInput(session, "eyeSelection_method", selected = config$eyeSelection_method)

    config_io_status(list(ok = TRUE, message = paste0("Loaded config from ", input$load_config_file$name)))
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
