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
      actionButton("preview", "Preview matched files", class = "btn-primary")
    ),
    mainPanel(
      h4("Dry-run preview"),
      uiOutput("preview_summary"),
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
      textInput("batchName", "Batch name (used to label output files)", value = "run1"),
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

    prom <- start_background_batch(
      directoryBIDS = dir,
      batchName = batch_name,
      numberCores = 2L # see background_run.R for why this default, not eyeQualityBatch()'s auto-detect
    )

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
}

shinyApp(ui = ui, server = server)
