# eyeQuality: merged Setup & Run / Analyze QC Explorer app (P10-12).
#
# Before this task, "Setup & Run" (Phase 9) and "Analyze / QC Explorer"
# (Phase 10) were two independently-launched Shiny processes
# (inst/shiny-apps/setup/, inst/shiny-apps/analyze/), each its own
# shiny::runApp() call -- a running Shiny session can't launch a second,
# separate Shiny process for itself, so P9-07's "post-run summary" could only
# ever hand the user a copy-pasteable eyeQuality::runAnalyzeApp() command to
# run in a different R console/session (that entry point has since been
# folded into eyeQuality::eyeQualityApp(), see below). This file merges both
# into one shinyApp()/process with two top-level tabs, so a finished Setup
# run can hand its output directory to the Analyze tabs directly, in-process,
# and a user never leaves this one running app to go from "processed my
# data" to "reviewing it."
#
# Launch via eyeQuality::eyeQualityApp() rather than sourcing this file
# directly. That single entry point always opens on the Setup & Run tab, and
# optionally accepts an `initialDirectory` argument to pre-point the Analyze
# tabs' own directory field at an existing output directory produced outside
# this app, e.g. by a script-driven eyeQualityBatch() run. See
# R/eyeQualityApp.R for how that's wired via shiny::shinyOptions().
#
# UI/server content below is inherited from the two standalone apps
# essentially unchanged (including P10-11's shared file selector/per-file
# notes/QC flags reorganization, built against the standalone Analyze app --
# nothing about that design assumed a separate process, so it needed no
# rework here beyond the plain ID renames below) -- see each section's own
# comments for what's new versus carried over. The real changes this task
# made:
#   - shared page, so a handful of input/output IDs that were safe to reuse
#     across two separate pages (input$directory/input$recursiveSearch/
#     output$selected_directory -- the Analyze tabs' own "which output
#     directory to load qcsummary files from" controls, distinct from the
#     Setup tab's "which directory to scan for input files") had to be
#     renamed to input$analyze_directory/input$analyze_recursiveSearch/
#     output$analyze_selected_directory to avoid colliding with the Setup
#     tab's own identically-named controls now sharing one page.
#   - one reconciled save/load-config flow (Setup tab's sidebar only) instead
#     of two independent ones -- see that section's own comment below.
#   - the Setup tab's post-run panel now hands its output directory straight
#     to the Analyze tabs (loading it and switching tabs), instead of
#     rendering a copy-pasteable command -- see output$post_run_link/
#     input$review_in_analyze below.
#
# This app requires the union of both apps' own Suggests-only dependencies:
# shiny, shinyFiles, future, promises (Setup tab), DT, plotly (Analyze
# tabs) -- since both tab groups now live in one process, both sets are
# needed regardless of which tab a user starts on. See R/eyeQualityApp.R for
# the actual requireNamespace() checks.

library(shiny)
library(shinyFiles)
library(future)
library(promises)
library(DT)
library(plotly)

source("setup_helpers.R", local = TRUE)
source("background_run.R", local = TRUE)
source("analyze_helpers.R", local = TRUE)

# P10-02 (unchanged from the standalone Analyze app): qc_threshold_config
# (analyze_helpers.R) has one row per (threshold_id, qc_metric) pair --
# interpolated_LeftEye/RightEye deliberately share one threshold_id/UI
# control (see analyze_helpers.R's comment on qc_threshold_config). The UI
# only needs one input per unique threshold_id, so it's deduplicated once
# here rather than re-deduplicating inline wherever the UI is built.
qc_threshold_ui_config <- qc_threshold_config[!duplicated(qc_threshold_config$threshold_id), ]

ui <- navbarPage(
  title = "eyeQuality",
  id = "top_nav",
  # getShinyOption("app_initialTab", ...): set by eyeQualityApp()
  # (R/eyeQualityApp.R) before shiny::runApp() is called, the same
  # shinyOptions()/getShinyOption() mechanism P9-07 already used for
  # analyze_initialDirectory below. eyeQualityApp() always sets this to
  # "1. Setup & Run"; the getShinyOption() default below only matters when
  # sourcing/launching this app.R directly with no shinyOption set at all,
  # which still opens somewhere sensible.
  #
  # "1. "/"2. " prefixes on both tab labels below (and everywhere else that
  # references them by exact string -- R/eyeQualityApp.R's own
  # app_initialTab default, and the updateNavbarPage() hand-off further down)
  # are a plain-text numeral marker, not a new feature -- field feedback was
  # that the intended left-to-right Setup-then-Analyze workflow order wasn't
  # visually obvious from two same-weight tab labels. navbarPage()'s own
  # `selected =`/updateNavbarPage()'s own `selected =` both match tabs by
  # this exact label string, so every one of those call sites has to agree,
  # not just the two tabPanel() titles themselves.
  selected = getShinyOption("app_initialTab", "1. Setup & Run"),

  tabPanel(
    "1. Setup & Run",
    h3("Setup & Run"),
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
        # P9-08: outputStructure/copyRawFile (P7-07) are only meaningful when
        # outputDir is set -- eyeQualityBatch()'s own outputStructure = "bids"
        # branch only changes anything when outputDir is non-NULL (see
        # create_new_filename()'s comment in R/saveFiles.R), so both checkboxes
        # are simply hidden rather than shown-but-disabled while outputDir is
        # blank. Hiding (not disabling) is enough here: a hidden checkbox's
        # underlying input value is retained, not reset, so toggling outputDir
        # blank and back doesn't lose whatever the user had checked -- and
        # since the value has no effect at all while outputDir is blank
        # anyway, there's nothing to protect against by additionally
        # disabling it server-side.
        conditionalPanel(
          condition = "input.outputDir != ''",
          checkboxInput(
            "useBidsOutput",
            "Use BIDS-structured output (derivatives/eyeQuality-v1/sub-XX/ses-XX/...)",
            value = FALSE
          ),
          conditionalPanel(
            condition = "input.useBidsOutput == true",
            checkboxInput(
              "copyRawFile",
              "Also copy raw file into output structure",
              value = FALSE
            )
          )
        ),
        textInput("batchName", "Batch name (used to label output files)", value = "run1"),
        actionButton("preview", "Preview matched files", class = "btn-primary"),
        hr(),
        # P10-12: the ONE save/load-config control for the whole app -- see
        # this tab's server-side "Save / load config" section below for why
        # this now also carries the Analyze tabs' own QC thresholds (P10-07),
        # reconciling what used to be two independent save/load flows (this
        # one, and a second, QC-thresholds-only one on the Analyze tabs) into
        # one. The Analyze tabs' own sidebar just points back here now.
        h5("Save / load config"),
        p(
          class = "text-muted",
          "Saves the run parameters above together with the Analyze tabs' current QC thresholds ",
          "as one batch_config.yaml, or loads one back in (both parts)."
        ),
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
        uiOutput("run_status"),
        uiOutput("post_run_link")
      )
    )
  ),

  tabPanel(
    "2. Analyze / QC Explorer",
    h3("Analyze / QC Explorer"),
    p(
      "Choose the output directory a batch run wrote (or pointed outputDir at), ",
      "and load every qcsummary.tsv output found under it into one table -- or use ",
      "the Setup & Run tab's \"Review results in Analyze tabs\" button once a run ",
      "finishes there to jump straight here, already loaded."
    ),
    sidebarLayout(
      sidebarPanel(
        # analyze_directory/analyze_selected_directory/analyze_recursiveSearch:
        # renamed from the standalone Analyze app's own directory/
        # selected_directory/recursiveSearch (P10-12) -- those names are
        # already used by the Setup tab's own, conceptually different,
        # "which directory to scan for input files" controls now sharing this
        # same page; a single Shiny page can't have two elements with the
        # same input/output ID. No other behavior change.
        shinyDirButton(
          "analyze_directory",
          "Choose output directory",
          "Select the directory containing qcsummary.tsv outputs"
        ),
        br(), br(),
        verbatimTextOutput("analyze_selected_directory"),
        hr(),
        checkboxInput(
          "analyze_recursiveSearch",
          "Search subdirectories recursively",
          value = TRUE
        ),
        actionButton("load", "Load qcsummary files", class = "btn-primary"),
        hr(),
        # Live-monitoring mode: lets this tab be pointed at the same output
        # directory a Setup tab run is currently writing qcsummary.tsv files
        # into and watch results accumulate without repeatedly re-clicking
        # "Load qcsummary files" above. Deliberately gated on having already
        # loaded at least once (see has_loaded_once() below) -- there's no
        # directory to poll yet otherwise, and checking this box before that
        # point would have nothing to do until a real load happens anyway
        # (either a manual "Load" click, or the Setup tab's own "Review
        # results" hand-off, P10-12).
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
          "value crosses the threshold below. See P10-02 in analyze_helpers.R for why ",
          "only these metrics are thresholdable."
        ),
        # P10-02: one numericInput per unique threshold_id in
        # qc_threshold_config (analyze_helpers.R) -- currently 3 (valid_pct,
        # robust_pct, interp_pct). Looping over the shared config here (rather
        # than hardcoding 3 numericInput() calls with separately-typed
        # labels) keeps the UI and the flagging logic's metric/label/direction
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
        # P10-12: save/load for the thresholds above now lives on the Setup &
        # Run tab's own sidebar -- it saves/loads them together with a run's
        # own parameters in the same batch_config.yaml (P10-07's schema)
        # instead of a second, separate save/load control here. Before the
        # merge, this tab needed its own "Study info" fields
        # (batchName/directoryBIDS/displayDimension*_mm) purely because the
        # Analyze app could be launched with no Setup form to source them
        # from; now that both live on one page, the Setup tab's own
        # equivalent fields already cover that, so those duplicate fields are
        # gone too.
        div(
          class = "text-muted",
          p(
            strong("Save / load config:"),
            " use the Setup & Run tab's sidebar -- it saves/loads these QC thresholds ",
            "together with a run's own parameters in one batch_config.yaml."
          )
        )
      ),
      mainPanel(
        uiOutput("load_summary"),
        uiOutput("load_diagnostics"),
        uiOutput("read_error_ui"),
        tabsetPanel(
          # "QC flags": renamed from "QC table" -- the every-file/every-metric
          # long table is still here (P10-01/P10-02), but it's no longer the
          # tab's default, fully-expanded view: field feedback was that seeing
          # every one of ~32 metrics for every loaded file at once is too much
          # to make a meaningful judgment call from. The tab now leads with two
          # more scannable things instead -- a file selector (shared with the
          # Plots/Gaze Explorer tabs, see file_selector_ids below) and a
          # compact, all-files table of just the 3 "major" metrics
          # (major_qc_metrics(), analyze_helpers.R) -- and tucks the full long
          # table behind a collapsed-by-default toggle for whoever wants to
          # drill into every metric for every file at once. The full table's
          # own server-side wiring (output$qc_table/qc_table_proxy, the
          # threshold re-flagging observer, auto-refresh's replaceData() push,
          # and P10-03's row-click -> selected_source_file_val()) is
          # UNCHANGED -- only its visibility is now gated on a checkbox, which
          # is a client-side conditionalPanel() condition with no server
          # involvement at all.
          tabPanel(
            "QC flags",
            br(),
            # width = "100%": without an explicit width, long BIDS-style
            # recording labels (e.g. "sub-01_ses-1_recording-eyetracking_physio")
            # were getting cut off in the field -- selectize.js's dropdown
            # popup matches this control's own rendered width, so widening the
            # control also widens the dropdown list, not just the closed box.
            # Typing to filter already works via selectizeInput's own default
            # behavior (no restrictive `options` here disable it) -- this was
            # only ever a visibility problem, not a missing-feature one.
            selectizeInput(
              "qcflags_file_selector", "Select file",
              choices = character(0),
              options = list(placeholder = "Select a file..."),
              width = "100%"
            ),
            h4("Major QC metrics (all files)"),
            p(
              class = "text-muted",
              "Valid %, robust %, and interpolated % -- the same 3 metrics thresholdable in the sidebar. ",
              "Click a metric's threshold in the sidebar to change the highlighting below."
            ),
            DTOutput("qc_major_table"),
            hr(),
            h4("Review notes"),
            p(
              class = "text-muted",
              "A note for the file currently selected above, explaining why its quality looks the way ",
              "it does -- saved to a single notes.tsv file alongside the loaded directory (shared across ",
              "anyone reviewing this same directory) and included in the CSV export below."
            ),
            textAreaInput(
              "qcflags_note_text", NULL,
              value = "", rows = 3, width = "100%",
              placeholder = "e.g. Excessive blinking during calibration, re-run recommended."
            ),
            actionButton("qcflags_save_note", "Save note", class = "btn-primary"),
            uiOutput("qcflags_note_status"),
            hr(),
            uiOutput("qc_flag_summary"),
            # P10-05: export every recording currently flagged by any
            # configured threshold (qc_table_flagged()'s own qc_flag, the same
            # source of truth the highlighting above reads) as a CSV, for
            # handing off to a colleague or using as an exclusion/re-review
            # list. See build_flagged_export_table() (analyze_helpers.R) and
            # the downloadHandler below for why "flagged by any threshold" is
            # the default rather than a single metric.
            downloadButton("download_flagged_csv", "Export flagged for review (CSV)"),
            br(), br(),
            checkboxInput("qcflags_show_detail", "Show detailed metrics table (every file, every metric)", value = FALSE),
            conditionalPanel(
              condition = "input.qcflags_show_detail == true",
              p("Click a row to view that recording's plots in the \"Plots\" tab."),
              DTOutput("qc_table")
            )
          ),
          tabPanel(
            "Plots",
            br(),
            fluidRow(
              # 8/4 rather than 6/6: a half-width column was cutting off long
              # BIDS-style recording labels in the field; the badges next to
              # it don't need as much room as the selector (and its dropdown
              # popup, which matches the control's own rendered width) does.
              column(
                8,
                selectizeInput(
                  "plots_file_selector", "Select file",
                  choices = character(0),
                  options = list(placeholder = "Select a file..."),
                  width = "100%"
                )
              ),
              column(4, uiOutput("plots_major_metrics_ui"))
            ),
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
          # "Gaze Explorer": distinct from both "QC table" (metrics, not raw
          # gaze samples) and "Plots" (fixed, pre-built ggplot outputs for the
          # WHOLE recording) -- this tab is an interactive, single-file view
          # over a user-chosen TIME RANGE of that recording's raw gaze
          # trajectory, plus a live AOI (area-of-interest) percentage for
          # whatever range is currently selected. Positioned right after
          # "Plots" (both are single-file views, unlike "Compare files" below,
          # which is cross-file) and reuses that tab's own selected_source_file()
          # gating pattern (see plot_status_ui/plot_ready above, and this
          # tab's own gaze_status_ui/gaze_ready below) so both tabs always
          # agree on which recording is currently in view.
          tabPanel(
            "Gaze Explorer",
            br(),
            fluidRow(
              # 8/4 rather than 6/6: same reasoning as the "Plots" tab's own
              # selector column above.
              column(
                8,
                selectizeInput(
                  "gaze_file_selector", "Select file",
                  choices = character(0),
                  options = list(placeholder = "Select a file..."),
                  width = "100%"
                )
              ),
              column(4, uiOutput("gaze_major_metrics_ui"))
            ),
            uiOutput("gaze_status_ui"),
            conditionalPanel(
              condition = "output.gaze_ready == true",
              p(
                class = "text-muted",
                "Drag the time-range slider below to isolate a trial or portion of the recording -- ",
                "the trajectory plot and AOI percentage both update live as you drag. If this ",
                "recording has logged event markers, jump to one first as a convenient starting ",
                "point, then widen or narrow the range freely from there -- the slider is never ",
                "locked to exact event boundaries."
              ),
              uiOutput("gaze_event_marker_ui"),
              sliderInput(
                "gaze_time_range", "Time range (ms)",
                min = 0, max = 1, value = c(0, 1), step = 1, width = "100%"
              ),
              hr(),
              fluidRow(
                column(
                  3,
                  h4("AOI"),
                  actionButton("gaze_define_aoi", "Define AOI..."),
                  actionButton("gaze_clear_aoi", "Clear AOI"),
                  br(), br(),
                  uiOutput("gaze_aoi_status"),
                  hr(),
                  # "Onion-skin" playback (repo-owner feature request): plays
                  # the current time-range window back sample-by-sample, with
                  # a fading trail leading up to the current position -- see
                  # analyze_helpers.R's "Gaze Explorer onion-skin playback"
                  # section header comment for the full design, and the
                  # output$gaze_trajectory_plot/gaze_anim_* reactives below
                  # for the server-side wiring. gaze_play_toggle's label is
                  # flipped between "Play"/"Pause" from the server
                  # (updateActionButton()) rather than set here, since it
                  # reflects gaze_anim_playing() state, not a fixed label.
                  h4("Playback"),
                  actionButton("gaze_play_toggle", "Play"),
                  br(), br(),
                  sliderInput(
                    "gaze_trail_length", "Trail length (samples)",
                    min = 10, max = 300, value = 90, step = 5, width = "100%"
                  )
                ),
                column(9, plotlyOutput("gaze_trajectory_plot", height = "600px"))
              )
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
              "Compare how loaded files stack up against each other on chosen QC metric(s), ",
              "using the same thresholds configured in the sidebar. Pick ONE metric for a bar ",
              "chart across files, or TWO metrics for a scatterplot (one metric per axis, one ",
              "point per file) -- useful for seeing whether files that are bad on one metric also ",
              "tend to be bad on another."
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
            # for derive_batch_name()'s NA_character_ case (analyze_helpers.R) --
            # eyeQuality()'s single-file naming form has no batchName at all,
            # which is a real, expected case here, not a data error.
            selectizeInput(
              "compare_batch_name_filter", "Filter to batch_name (run)",
              choices = character(0), multiple = TRUE
            ),
            # Mirror of the batch_name filter directly above, for studies
            # where the same sub/ses BIDS recording has multiple task-<label>
            # recordings (e.g. task-x and task-y) and a user wants to compare
            # one specific task, or every task together. Composes with the
            # batch_name filter -- both are applied in sequence wherever
            # either is applied below -- since they narrow on independent
            # axes (which processing run vs. which original task recording).
            # The "(none)" choice stands in for derive_task_label()'s
            # NA_character_ case (analyze_helpers.R) -- a non-BIDS filename,
            # or a BIDS name that simply omits "task-", is a real, expected
            # case here, not a data error.
            selectizeInput(
              "compare_task_name_filter", "Filter to task_name",
              choices = character(0), multiple = TRUE
            ),
            h4("Metric(s) across files"),
            # multiple = TRUE, maxItems = 2: 1 selected metric renders the
            # original bar chart (build_qc_comparison_plot()), 2 renders a
            # scatterplot (build_qc_comparison_scatter()) instead -- see the
            # server's output$compare_plot for the actual branch. Capped at 2
            # since a scatterplot only has 2 axes; a 3rd selection is simply
            # not offered by maxItems rather than silently ignored.
            selectizeInput(
              "compare_metric_plot", "QC metric(s) -- 1 = bar chart, 2 = scatterplot",
              choices = character(0), multiple = TRUE,
              options = list(maxItems = 2)
            ),
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
)

server <- function(input, output, session) {
  volumes <- c(Home = fs::path_home(), shinyFiles::getVolumes()())
  shinyFiles::shinyDirChoose(input, "directory", roots = volumes, session = session)
  shinyFiles::shinyDirChoose(input, "analyze_directory", roots = volumes, session = session)
  shinyFiles::shinyFileSave(input, "save_config", roots = volumes, session = session)

  # ==========================================================================
  # Setup & Run tab
  # ==========================================================================

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
    # trimws(): a stray leading/trailing space here (typed into
    # directory_override()'s source -- a pasted path, or a directoryBIDS
    # value loaded from a hand-edited batch_config.yaml -- shinyFiles'
    # parseDirPath() itself is a less likely but not excluded source)
    # propagates into every file path listBidsFiles() discovers under this
    # directory, and from there into every real output directory
    # eyeQualityBatch() tries to create per file. fs::dir_create() rejects a
    # leading-space path outright with a confusing "[EINVAL] Failed to make
    # directory ' C:'"-style error that gives no hint the actual problem is
    # whitespace -- confirmed by direct reproduction, not theoretical.
    # Trimmed once here, at the single reactive every downstream consumer
    # of the selected directory reads from, rather than chasing every
    # possible place a space could otherwise sneak in.
    if (!is.null(input$directory) && is.list(input$directory)) {
      return(trimws(shinyFiles::parseDirPath(volumes, input$directory)))
    }
    override <- directory_override()
    if (!is.null(override) && nzchar(override)) {
      return(trimws(override))
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
  run_info <- reactiveValues(directory = NULL, batchName = NULL, n_expected = NULL, outputDir = NULL)
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
    run_info$outputDir <- blank_to_null(input$outputDir)

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
        # P9-08: outputStructure/copyRawFile (P7-07) -- forwarded to
        # eyeQualityBatch() via start_background_batch()'s `...` passthrough.
        # Both checkboxes are only visible/meaningful once outputDir is set
        # (see the UI's conditionalPanel above), but read unconditionally
        # here regardless of visibility -- a hidden checkboxInput still has a
        # real value, and eyeQualityBatch()/create_new_filename() themselves
        # already ignore outputStructure = "bids"/copyRawFile whenever
        # outputDir is NULL (P7-07), so there's no need to duplicate that
        # gating here.
        outputStructure = if (isTRUE(input$useBidsOutput)) "bids" else "flat",
        copyRawFile = isTRUE(input$copyRawFile),
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
      final <- poll_batch_progress(
        run_info$directory, run_info$batchName, run_info$n_expected,
        outputDir = run_info$outputDir
      )
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

    progress <- poll_batch_progress(
      run_info$directory, run_info$batchName, run_info$n_expected,
      outputDir = run_info$outputDir
    )
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
          if (!is.na(eta_label)) p(sprintf("Estimated time remaining: %s", eta_label)),
          # Before this, the only visible link into the Analyze tabs
          # (output$post_run_link, below) only appears once a run is fully
          # "done" -- there was no way to discover, while still watching a
          # run in progress here, that qcsummary.tsv outputs can already be
          # browsed as they land. This note doesn't do any hand-off itself
          # (unlike output$post_run_link's own "Review results" button,
          # which also loads the directory automatically) -- the Analyze
          # tabs' directory field isn't necessarily known yet this early in a
          # run without an explicit outputDir set, so the user is pointed at
          # the manual controls (its own directory picker, "Load qcsummary
          # files", and "Auto-refresh") rather than a button that assumes one.
          div(
            class = "alert alert-info",
            "You can switch to the \"2. Analyze / QC Explorer\" tab right now to watch results ",
            "land as this run completes them -- choose this run's directory there, click ",
            "\"Load qcsummary files\" once, then check \"Auto-refresh\" to keep it updating live."
          )
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
    df <- progress_state$failed_detail
    # get_failed_file_details() names this column "error" (it holds the
    # actual per-file error message) -- relabeled to "notes" for display
    # only, since "Failed files" already says these are errors and the
    # repeated "error" column heading read as redundant/alarming next to
    # it. Local to this table; the underlying error/message contract is
    # unchanged everywhere else that reads get_failed_file_details().
    names(df)[names(df) == "error"] <- "notes"
    df
  })

  # P9-07/P10-12: hand the finished run's output directory to the Analyze
  # tabs. Shown only once progress_state$status reaches "done" -- i.e.
  # eyeQualityBatch() itself ran to completion without a coarse, run-level
  # error escaping it (see start_background_batch()'s own comment on that
  # distinction from per-file failures) -- so there's real output on disk
  # worth reviewing, even if some individual files failed and show up in the
  # failed-files table above. Not shown for the "failed" (coarse) state,
  # since that's typically an upfront validation error with nothing written.
  #
  # Before P10-12 this rendered a copy-pasteable eyeQuality::runAnalyzeApp()
  # (since renamed to eyeQuality::eyeQualityApp()) call for a SEPARATE R
  # console/session (see resolve_analyze_directory()/
  # build_analyze_launch_command() in setup_helpers.R for why that used to be
  # the only option). Now that both tab groups share one process, clicking
  # the button below does the real thing directly: loads this run's output
  # directory into the Analyze tabs (input$review_in_analyze below) and
  # switches to that tab -- no copy-pasting, no second session.
  output$post_run_link <- renderUI({
    if (!identical(progress_state$status, "done")) {
      return(NULL)
    }

    analyze_dir <- resolve_analyze_directory(input$outputDir, run_info$directory)

    div(
      class = "alert alert-success",
      h5("Review results in the Analyze tabs"),
      p("Outputs from this run are under:"),
      tags$pre(analyze_dir),
      actionButton("review_in_analyze", "Review results in Analyze tabs", class = "btn-success")
    )
  })

  # input$review_in_analyze: the actual in-process hand-off (P10-12). Reuses
  # resolve_analyze_directory() (setup_helpers.R, unchanged) to decide which
  # directory to point at -- same logic the panel above already used to
  # DISPLAY that directory -- then calls do_analyze_load() (defined in the
  # Analyze tabs' own server section below; see that section's header
  # comment for why calling it from here, before its own definition
  # appears in this file, is still correct) exactly the way a manual "Load
  # qcsummary files" click on the Analyze tabs would, and finally switches
  # the visible tab so the user actually sees the result land.
  observeEvent(input$review_in_analyze, {
    analyze_dir <- resolve_analyze_directory(input$outputDir, run_info$directory)
    req(analyze_dir, nzchar(analyze_dir))

    do_analyze_load(analyze_dir, recursive = TRUE)
    updateNavbarPage(session, "top_nav", selected = "2. Analyze / QC Explorer")
  })

  # --- P9-04/P10-07/P10-12: save/load one reconciled batch_config.yaml -----
  #
  # Before P10-12, the Setup app's own save/load flow (P9-04, run
  # parameters) and the Analyze app's own save/load flow (P10-07, QC
  # thresholds + a small "study info" field set duplicating a few of the
  # Setup form's own fields) were two independent UI flows against the same
  # underlying batch_config.yaml schema -- unavoidable while they were two
  # separate processes, since the Analyze app had no access to a Setup
  # form's live values to fall back on. Merged into one process, that's no
  # longer true: this ONE save/load control (Setup tab sidebar only, see the
  # Analyze tabs' own sidebar note pointing back here) now saves/loads both
  # halves together, and the Analyze tabs' old "Study info" duplicate fields
  # are gone entirely -- the Setup tab's own directoryBIDS/batchName/
  # displayDimension*_mm already cover that need.
  #
  # loaded_config_extra: the full config most recently loaded via "Load
  # config" this session (or NULL if none has been), used purely so
  # build_current_config() can carry forward every schema field neither
  # form directly controls (e.g. adapterType) -- see that function's own
  # comment for why.
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
  # (see build_batch_config_from_form()), PLUS the Analyze tabs' current QC
  # threshold values (qc_thresholds(), defined in that section below --
  # readable here regardless of definition order, see this file's own header
  # comment) overlaid as `qcThresholds` -- this is the actual reconciliation:
  # every save now always reflects the live threshold state, not just
  # whatever qcThresholds a previously loaded config happened to carry
  # forward untouched.
  build_current_config <- reactive({
    dir <- selected_dir()
    config <- build_batch_config_from_form(
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
      outputStructure = if (isTRUE(input$useBidsOutput)) "bids" else "flat",
      copyRawFile = isTRUE(input$copyRawFile),
      extra = loaded_config_extra(),
      defaultNumberCores = DEFAULT_GUI_NUMBER_CORES
    )
    config$qcThresholds <- qc_thresholds_to_percent(qc_thresholds())
    config
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
  # to the form. Only on a clean read do any update*Input() calls happen --
  # this now includes the Analyze tabs' own qc_threshold_* numericInputs,
  # not just the Setup form's own fields (P10-12 reconciliation).
  #
  # Deliberate scope decision, worth flagging explicitly rather than
  # silently picking a side: before the merge, the Analyze app's own load
  # handler used validate = FALSE specifically so a QC-thresholds-only
  # config (missing directoryBIDS/batchName/displayDimension*_mm entirely)
  # could still load its thresholds. This ONE reconciled handler instead
  # uses the Setup app's stricter validate = TRUE for everything, including
  # thresholds -- an incomplete config is now rejected outright, same as it
  # already was for Setup's own fields, rather than partially applied. This
  # is safe for anything the "Save config" button above now produces (it
  # always writes complete, valid run parameters alongside qcThresholds,
  # since the Setup form's own fields always have real, non-blank default
  # values), but is a real behavior change for a hand-authored,
  # deliberately-incomplete "thresholds only" YAML file, which would have
  # loaded its thresholds under the old standalone Analyze app and no longer
  # does here.
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
    # P9-08: outputStructure/copyRawFile (P7-07) -- default_batch_config()'s
    # modifyList() fill-in (R/batchConfig.R) already guarantees both fields
    # are present on `config` here even when the loaded file predates this
    # task (outputStructure = "flat", copyRawFile = FALSE), so no extra
    # is.null() fallback is needed at this call site.
    updateCheckboxInput(session, "useBidsOutput", value = identical(config$outputStructure, "bids"))
    updateCheckboxInput(session, "copyRawFile", value = isTRUE(config$copyRawFile))

    # P10-12: also populate the Analyze tabs' own QC threshold inputs, the
    # same filter-then-fallback-to-default logic the standalone Analyze
    # app's own load handler used (filter_recognized_qc_thresholds(),
    # default_qc_thresholds(), both analyze_helpers.R) -- every threshold_id's
    # numericInput is always set (to the loaded value if kept, otherwise back
    # to its documented default), not just the ones the file happened to
    # specify, so "load" fully determines the resulting threshold state.
    filtered <- filter_recognized_qc_thresholds(config$qcThresholds)
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

  # ==========================================================================
  # Analyze / QC Explorer tabs
  # ==========================================================================
  #
  # Inherited from the standalone Analyze app essentially unchanged (P10-12
  # scope: this task inherits P10-11's already-built shared-selector/notes/
  # QC-flags UI rather than rebuilding it -- see this file's own top-of-file
  # comment). The only structural changes in this section are: the
  # analyze_directory/analyze_selected_directory/analyze_recursiveSearch ID
  # renames (this page's Setup tab already owns directory/selected_directory/
  # recursiveSearch), removal of this tab's own save/load-config flow and
  # "study info" fields (now the Setup tab's single reconciled flow above),
  # and factoring the manual-load body out into do_analyze_load() so the
  # Setup tab's own "Review results in Analyze tabs" button (above) can
  # trigger the exact same load, not just prefill a directory field.

  # analyze_directory_override: seeds from analyze_initialDirectory
  # (getShinyOption(), set by eyeQualityApp()'s `initialDirectory` argument
  # before shiny::runApp() -- R/eyeQualityApp.R) for the case where this app
  # is launched already pointed at an existing output directory (e.g. one
  # produced by a script-driven eyeQualityBatch() run, not this app's own
  # Setup tab). do_analyze_load() (below) also writes into this reactiveVal
  # on every load, seeded or not, so this field's displayed value always
  # reflects whichever directory was most recently loaded, matching
  # selected_dir()'s own "a real pick always wins" fallback pattern.
  #
  # Renamed from the standalone Analyze app's own `directory_override` (same
  # fallback-bridge idea as the Setup tab's own `directory_override` above,
  # for the same shinyDirButton-has-no-programmatic-setter reason) purely to
  # avoid colliding with that same-named Setup tab reactiveVal now that both
  # live in one server() scope.
  analyze_directory_override <- reactiveVal(getShinyOption("analyze_initialDirectory", NULL))

  analyze_selected_dir <- reactive({
    # trimws(): see the Setup tab's identical selected_dir() above for the
    # full write-up -- a stray leading/trailing space here propagates into
    # every downstream file path this tab reads, producing a confusing
    # "[EINVAL] Failed to make directory ' C:'"-style error with no hint the
    # actual problem is whitespace.
    if (!is.null(input$analyze_directory) && is.list(input$analyze_directory)) {
      return(trimws(shinyFiles::parseDirPath(volumes, input$analyze_directory)))
    }
    override <- analyze_directory_override()
    if (!is.null(override) && nzchar(override)) {
      return(trimws(override))
    }
    character(0)
  })

  output$analyze_selected_directory <- renderText({
    dir <- analyze_selected_dir()
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
  # a reactiveVal, by contrast, can be updated from three independent places
  # now: a manual "Load qcsummary files" click, a live auto-refresh timer
  # tick, and (P10-12) the Setup tab's own "Review results" hand-off. All
  # three write into this same reactiveVal so every downstream reactive/
  # output that reads current_load_result() (load_summary, qc_table_flagged(),
  # compare_metric_choices(), etc.) picks up any of them identically,
  # without needing to know which path produced it.
  current_load_result <- reactiveVal(NULL)

  # has_loaded_once: TRUE from the moment the FIRST successful load
  # validates a directory (manual click, or the Setup tab's hand-off), and
  # stays TRUE for the rest of the session (even across a later load of a
  # different directory) -- this is what auto-refresh's own polling observer
  # (below) gates on, so checking the "Auto-refresh" box before any
  # directory has ever been loaded simply does nothing yet rather than
  # trying to poll an unvalidated/nonexistent selected directory.
  # Deliberately its own reactiveVal rather than derived as
  # !is.null(current_load_result()) -- the auto-refresh polling observer
  # itself also WRITES current_load_result() on every tick, and gating that
  # same observer's re-trigger condition on a value it also sets up would
  # create a self-invalidating loop (the observer's own tick causing it to
  # immediately want to re-run again, independent of invalidateLater()'s
  # schedule) -- see that observer's own comment for the full explanation.
  has_loaded_once <- reactiveVal(FALSE)

  # last_refresh_time: POSIXct timestamp of the most recent successful load
  # of any kind, or NULL before the first one -- surfaced via
  # output$auto_refresh_status below so it's always obvious to a user
  # watching a live run whether auto-refresh is on and when it actually last
  # pulled fresh data.
  last_refresh_time <- reactiveVal(NULL)

  # manual_load_trigger: a plain counter incremented once per genuine load
  # (manual "Load qcsummary files" click, or the Setup tab's hand-off -- see
  # do_analyze_load() below), used purely as output$qc_table's OWN rebuild
  # trigger (see that renderDT() below) -- current_load_result() itself
  # can't serve that role once auto-refresh exists, because
  # current_load_result() now also changes on every auto-refresh tick, and
  # rebuilding the whole DT widget on every tick would reset a user's
  # sort/filter/page state every few seconds (exactly the UX regression
  # auto-refresh is supposed to avoid -- see the P10-02 threshold-tweak
  # precedent this design follows: a full rebuild only on a deliberate
  # "start fresh" action, replaceData() for everything else). A counter
  # (rather than e.g. Sys.time()) is enough -- nothing downstream reads its
  # actual value, only that it changed.
  manual_load_trigger <- reactiveVal(0L)

  # notes_store: this loaded directory's shared review notes (see
  # analyze_helpers.R's header comment on notes_filename) -- read from disk
  # once per genuine load (below) and kept live here thereafter; "Save note"
  # (further down) both updates this reactiveVal and immediately persists it
  # back to disk via save_notes_table(). Starts as empty_notes_table() (a
  # real, 0-row table, not NULL) so every reader below can assume a
  # well-shaped data.frame is always present, even before any directory has
  # ever been loaded.
  notes_store <- reactiveVal(empty_notes_table())

  # do_analyze_load: the one real "start fresh, load this directory" body,
  # factored out (P10-12) so it can be called from two places that both need
  # the exact same behavior -- a manual "Load qcsummary files" click
  # (observeEvent(input$load, ...) just below) and the Setup tab's own
  # "Review results in Analyze tabs" button (input$review_in_analyze,
  # defined earlier in this file's Setup section -- readable from there
  # despite appearing textually before this definition, since both are
  # plain top-level statements inside the same server() call and neither
  # fires until well after the whole server() body, including this
  # assignment, has already executed once; see this file's own header
  # comment). Directory validation (non-empty, exists) is each CALLER's own
  # responsibility -- the manual path uses validate()/need() so a bad
  # picker selection shows an inline message, while the hand-off path uses a
  # plain req() guard instead, since there's no natural place for a
  # validate() message to render from a button click on a different tab.
  do_analyze_load <- function(dir, recursive) {
    analyze_directory_override(dir)
    current_load_result(load_qcsummary_table(dir, recursive = recursive))
    has_loaded_once(TRUE)
    last_refresh_time(Sys.time())
    manual_load_trigger(isolate(manual_load_trigger()) + 1L)
    notes_store(load_notes_table(dir))
  }

  # The manual "Load qcsummary files" path: validates analyze_selected_dir()
  # exactly as the former eventReactive did, then delegates to
  # do_analyze_load() -- this is one of two places a NEW directory selection
  # actually takes effect (the other being the Setup tab's hand-off button;
  # auto-refresh below always re-polls whatever directory was most recently
  # loaded via either of those two, never a directory the user has merely
  # clicked around in the picker without loading).
  observeEvent(input$load, {
    dir <- analyze_selected_dir()
    validate(need(length(dir) > 0 && nzchar(dir), "Please choose a directory first."))
    validate(need(dir.exists(dir), "Selected directory does not exist."))

    do_analyze_load(dir, recursive = isTRUE(input$analyze_recursiveSearch))
  })

  # load_result: backward-compatible alias, preserving the exact semantics
  # the former eventReactive(input$load, ...) had for anything reading this
  # app's server-local reactives by name -- most notably the
  # shiny::testServer()-based regression suite, which calls load_result()
  # directly in several places and specifically asserts (in the P9-07
  # seeded-directory tests) that it throws before the first successful
  # load, the same way an eventReactive that's never fired does.
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

  # Same diagnostic spirit as the Setup tab's P7-06 zero-match handling --
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
  # threshold (compute_qc_flags(), analyze_helpers.R). Recomputed whenever
  # either the loaded data or the threshold inputs change, but this reactive
  # itself is never rendered directly -- see the initial renderDT() (built
  # once per load, using isolate()'d thresholds so a threshold tweak doesn't
  # rebuild the whole DT widget and reset the user's sort/filter/page state)
  # and the dataTableProxy observer just below (which pushes
  # qc_table_flagged()'s updated "qc_flag" values into the already-rendered
  # widget via DT::replaceData() instead -- the same replaceData() mechanism
  # the auto-refresh polling observer further down also reuses for a live
  # tick's freshly loaded data).
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
  # -- see that function's own comment (analyze_helpers.R) for why.
  flagged_recordings <- reactive({
    tbl <- qc_table_flagged()
    unique(tbl[tbl$qc_flag, c("recording", "batch_name", "source_file"), drop = FALSE])
  })

  # P10-05: export the currently flagged recordings as a CSV.
  #
  # downloadButton/downloadHandler (rather than shinySaveButton, unlike the
  # Setup tab's own P9-04/P10-07 config save) -- this export's whole point is
  # to hand a file to a colleague or use as a local exclusion/re-review list,
  # not to write back into the server-local output directory this app read
  # qcsummary.tsv files from, so a browser download matches the actual use
  # case better than a server-side save-as picker. It's also safe against
  # downloadHandler's usual failure mode (an unhandled content() error
  # producing a generic error page): with no data loaded or nothing flagged,
  # build_flagged_export_table() returns NULL and content() below just
  # writes a header-only CSV instead of erroring, so there's no invalid-input
  # case left for content() to throw on. Note that qc_table_flagged() itself
  # does req(result$table), which throws a shiny.silent.error/validation
  # condition (rather than returning NULL) if no directory has ever been
  # loaded yet -- fine inside a render context (Shiny swallows it), but
  # downloadHandler's content() runs outside that flush cycle, so an
  # uncaught version of that condition here would surface as a broken
  # download. The tryCatch below normalizes that case to the same NULL that
  # "loaded, nothing flagged" already produces.
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
      # notes = notes_store(): adds a "note" column to the export (blank
      # where a flagged file has no saved note) -- see
      # build_flagged_export_table()'s own `notes` parameter doc. Read via
      # isolate() for the same reason flagged_tbl is wrapped in tryCatch()
      # just above: this content() function runs outside Shiny's normal
      # render/flush cycle, and this download shouldn't itself become a new
      # reactive dependency of anything.
      tbl <- build_flagged_export_table(flagged_tbl, notes = isolate(notes_store()))
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
  # the whole DT widget on a genuine load (manual click, or the Setup tab's
  # hand-off) -- exactly like req(load_result()$table) used to when
  # load_result() was a plain eventReactive that only ever changed on that
  # same click -- but, unlike current_load_result() itself,
  # manual_load_trigger() does NOT also change on every auto-refresh timer
  # tick, so a live-monitoring session doesn't get its whole table (and the
  # user's sort/filter/page state) rebuilt from scratch every few seconds;
  # the auto-refresh polling observer (further down, past qc_table_proxy)
  # instead pushes tick data in place via DT::replaceData(), the same
  # mechanism the threshold observer just below already uses for a
  # threshold tweak. Wrapping the WHOLE body in isolate() (as an earlier
  # version did) strips every dependency, including this one -- the render
  # then only ever executes once, at app startup before any directory is
  # even chosen, produces nothing (current_load_result() is still NULL), and
  # never runs again no matter what gets loaded afterward -- see the
  # regression test guarding exactly this bug. current_load_result() and
  # qc_table_flagged() are therefore both read via isolate() below: this
  # render's only real trigger is manual_load_trigger(), with the isolate()'d
  # reads simply fetching whatever's current at the moment the trigger
  # fires.
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

  # --- "QC flags" tab: compact major-metrics table ---
  #
  # One row per file, columns = major_qc_metrics() only (valid %, robust %,
  # interpolated % per eye) -- the scannable, all-files-at-once default view
  # this tab now leads with, in front of the full ~32-metric long table
  # (still available, just collapsed by default -- see the UI's
  # checkboxInput("qcflags_show_detail", ...)). Reuses
  # build_qc_comparison_table() (P10-04, unmodified) for the actual pivot,
  # and the exact same per-column formatStyle()/styleInterval() threshold-
  # highlighting loop the "Compare files" tab's own wide table already uses
  # (output$compare_table below) -- not the row-level highlighting the full
  # long table (output$qc_table) uses, since qc_flag is a per-metric-ROW
  # concept that has no single value once pivoted to one column per metric.
  #
  # Reactively rebuilt on any current_load_result()/qc_thresholds() change
  # (not isolate()'d/proxy-optimized the way the big table is) -- this
  # table's realistic scale (one row per file, 4 metric columns) makes a
  # full rebuild on a threshold tweak or an auto-refresh tick cheap, the same
  # tradeoff output$compare_table already accepts for the same reason.
  output$qc_major_table <- renderDT({
    tbl <- current_load_result()$table
    req(tbl)
    major_tbl <- build_qc_comparison_table(tbl, major_qc_metrics())
    req(major_tbl)

    thresholds <- qc_thresholds()
    dt <- DT::datatable(
      major_tbl,
      rownames = FALSE,
      filter = "top",
      selection = "none",
      options = list(pageLength = 25, scrollX = TRUE)
    )
    for (i in seq_len(nrow(qc_threshold_config))) {
      cfg <- qc_threshold_config[i, ]
      if (!(cfg$qc_metric %in% names(major_tbl))) {
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
    metric_cols <- setdiff(names(major_tbl), c("recording", "batch_name", "source_file"))
    dt %>% DT::formatPercentage(metric_cols, 1)
  })

  # --- Shared per-tab file selector (QC flags / Plots / Gaze Explorer) ---
  #
  # Three separate selectizeInput widgets (file_selector_ids below), one
  # positioned at the top of each of those three tabs -- a single Shiny
  # input can't render itself in three places on one page, but all three are
  # kept in sync against the SAME underlying selected_source_file_val()
  # (P10-03's reactiveVal, originally populated only by the QC table's own
  # row-click) below, so picking a file in any one instance, or clicking a
  # QC table row, is reflected in the other two the next time that tab is
  # viewed.
  file_selector_ids <- c("qcflags_file_selector", "plots_file_selector", "gaze_file_selector")

  # Repopulates every selector's CHOICES (not its current selection) on a
  # genuine load -- same manual_load_trigger()-only reasoning as the
  # "Compare files" tab's own metric/batch_name choice observers above: a
  # brand-new directory's file list should reset these dropdowns' available
  # choices, but an auto-refresh tick against the SAME directory picking up
  # one more file should not silently reset whichever file a user currently
  # has selected. (An auto-refresh tick that adds a brand-new file will not
  # appear in these dropdowns until the next genuine load -- the same
  # accepted limitation already documented for the "Compare files" tab's own
  # batch_name filter above.)
  observeEvent(manual_load_trigger(), {
    choices <- build_file_selector_choices(current_load_result()$table)
    current_selection <- isolate(selected_source_file_val())
    for (id in file_selector_ids) {
      updateSelectizeInput(session, id, choices = choices, selected = current_selection)
    }
  }, ignoreInit = TRUE)

  # Each selector's own change becomes the new shared selection. Three
  # explicit observers (not a loop over file_selector_ids) deliberately --
  # observeEvent()'s event expression captures its enclosing environment's
  # `id` lazily, and a `for` loop's loop variable is a single shared binding
  # mutated in place across iterations, not a fresh one per iteration; all
  # three observers registered inside such a loop would end up reading
  # whatever `id` happened to equal AFTER the loop finished (the classic
  # "closure over a loop variable" bug), not their own intended id. Writing
  # them out separately sidesteps that entirely for a fixed, small (3) set.
  observeEvent(input$qcflags_file_selector, {
    selected_source_file_val(input$qcflags_file_selector)
  }, ignoreInit = TRUE)
  observeEvent(input$plots_file_selector, {
    selected_source_file_val(input$plots_file_selector)
  }, ignoreInit = TRUE)
  observeEvent(input$gaze_file_selector, {
    selected_source_file_val(input$gaze_file_selector)
  }, ignoreInit = TRUE)

  # ...and the shared selection, however it changed (either of the 3
  # selectors above, or the QC table's own P10-03 row-click observer), echoes
  # back out to every selector widget so all 3 (plus the QC table's own
  # highlighted row, unaffected here) agree on the same file. Guarded
  # per-widget (only updated if it doesn't already show the right value) to
  # avoid a redundant round-trip to a widget that's already correct --
  # notably the ONE widget that just triggered this via the observers
  # directly above, which already reflects the new value client-side and
  # doesn't need to be told again.
  observeEvent(selected_source_file_val(), {
    sf <- selected_source_file_val()
    for (id in file_selector_ids) {
      if (!identical(isolate(input[[id]]), sf)) {
        updateSelectizeInput(session, id, selected = if (is.null(sf)) character(0) else sf)
      }
    }
  }, ignoreNULL = FALSE)

  # --- "QC flags" tab: review notes for the selected file ---
  #
  # Populates the notes textAreaInput with the SELECTED file's own
  # previously-saved note (if any) whenever the selection changes -- without
  # this, a note typed for one file would appear to carry over to the next
  # file selected, which would misleadingly look like an existing note for a
  # file that actually has none.
  observeEvent(selected_source_file_val(), {
    sf <- selected_source_file_val()
    tbl <- current_load_result()$table
    if (is.null(sf) || is.null(tbl)) {
      updateTextAreaInput(session, "qcflags_note_text", value = "")
      return(invisible(NULL))
    }
    match_row <- unique(tbl[tbl$source_file == sf, c("recording", "batch_name"), drop = FALSE])
    if (nrow(match_row) == 0) {
      updateTextAreaInput(session, "qcflags_note_text", value = "")
      return(invisible(NULL))
    }
    note <- note_for_recording(notes_store(), match_row$recording[1], match_row$batch_name[1])
    updateTextAreaInput(session, "qcflags_note_text", value = note)
  }, ignoreNULL = FALSE)

  # note_save_status: list(ok = TRUE/FALSE, message = ...) for the most
  # recent "Save note" click, or NULL before one has happened this session --
  # same persistent-alert rendering convention as this app's own P10-07/
  # P10-12 config_io_status.
  note_save_status <- reactiveVal(NULL)

  observeEvent(input$qcflags_save_note, {
    sf <- selected_source_file_val()
    dir <- analyze_selected_dir()
    tbl <- current_load_result()$table
    if (is.null(sf) || is.null(tbl) || length(dir) == 0 || !nzchar(dir)) {
      note_save_status(list(ok = FALSE, message = "Select a file first."))
      return(invisible(NULL))
    }
    match_row <- unique(tbl[tbl$source_file == sf, c("recording", "batch_name"), drop = FALSE])
    if (nrow(match_row) == 0) {
      note_save_status(list(ok = FALSE, message = "Selected file is no longer in the loaded table."))
      return(invisible(NULL))
    }

    updated <- upsert_note(notes_store(), match_row$recording[1], match_row$batch_name[1], input$qcflags_note_text)
    notes_store(updated)

    result <- tryCatch(
      {
        save_notes_table(updated, dir)
        list(ok = TRUE)
      },
      error = function(e) list(ok = FALSE, message = conditionMessage(e))
    )
    note_save_status(if (isTRUE(result$ok)) {
      list(ok = TRUE, message = "Note saved.")
    } else {
      list(ok = FALSE, message = paste0("Note kept for this session, but could not save to disk: ", result$message))
    })
  })

  output$qcflags_note_status <- renderUI({
    status <- note_save_status()
    if (is.null(status)) {
      return(NULL)
    }
    div(
      class = if (isTRUE(status$ok)) "alert alert-success" else "alert alert-danger",
      status$message
    )
  })

  # --- Compact "major metrics" readout on the Plots / Gaze Explorer tabs ---
  #
  # A quick "is this file okay?" glance next to those tabs' own file
  # selectors, without switching to the QC flags tab -- build_major_metrics_summary()
  # (analyze_helpers.R) does the actual data prep; both outputs below just
  # render its small label/value/flag data.frame as a row of colored badges.
  render_major_metrics_badges <- function(source_file) {
    summary <- build_major_metrics_summary(qc_table_flagged(), source_file)
    if (is.null(summary)) {
      return(NULL)
    }
    tagList(lapply(seq_len(nrow(summary)), function(i) {
      row <- summary[i, ]
      span(
        class = paste("label", if (isTRUE(row$qc_flag)) "label-danger" else "label-success"),
        style = "margin-right: 6px; font-size: 100%; display: inline-block; padding: 4px 8px;",
        sprintf("%s: %.1f%%", row$label, row$percent * 100)
      )
    }))
  }

  output$plots_major_metrics_ui <- renderUI({
    render_major_metrics_badges(selected_source_file())
  })
  output$gaze_major_metrics_ui <- renderUI({
    render_major_metrics_badges(selected_source_file())
  })

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

  # Live auto-refresh: re-polls the SAME directory analyze_selected_dir()
  # currently points at on a fixed timer, so a user who has pointed this tab
  # at a Setup tab run's output directory can watch qcsummary.tsv results
  # accumulate without repeatedly re-clicking "Load qcsummary files". This is
  # the standard shiny conditional-polling pattern (see ?shiny::invalidateLater):
  # invalidateLater() is only ever reached, and therefore only ever
  # reschedules itself, while BOTH input$autoRefresh is checked AND
  # has_loaded_once() is TRUE -- the instant either goes false (box
  # unchecked, or a session that's never loaded anything), the early
  # return() below is hit instead and the invalidation chain simply stops,
  # rather than continuing to tick in the background with nothing to do.
  #
  # analyze_selected_dir()/input$analyze_recursiveSearch are read via
  # isolate(): this observer's own re-execution is meant to be driven ONLY
  # by the timer and the autoRefresh checkbox (and has_loaded_once(), which
  # only a genuine load ever sets) -- not by a user idly clicking around the
  # directory picker before deciding whether to load it. The directory
  # actually being polled only ever changes via a genuine load (which
  # already does a full current_load_result() reset and
  # manual_load_trigger() bump via do_analyze_load()), never by this
  # observer noticing a new, not-yet-loaded picker selection.
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

    dir <- isolate(analyze_selected_dir())
    recursive <- isolate(isTRUE(input$analyze_recursiveSearch))
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
  # generateEyeTrackingPlots() on it -- see load_plot_data() in
  # analyze_helpers.R. Returns NULL (rather than a not-ok result) when
  # nothing is selected yet, so downstream outputs can req() this cleanly
  # before a row is clicked.
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

  # --- "Gaze Explorer" tab: interactive time-range + AOI exploration ---
  #
  # Reuses selected_source_file() (defined above, P10-03) as this tab's own
  # file-selection source of truth, exactly like plot_result() above does for
  # the "Plots" tab -- both tabs always agree on which recording is currently
  # in view rather than tracking two independent notions of "the selected
  # file".

  # gaze_traj_result: load_gaze_trajectory_data()'s (analyze_helpers.R)
  # return value for whatever file is currently selected, or NULL when
  # nothing is selected yet -- same "NULL means nothing selected, non-NULL
  # always has an $ok" contract plot_result() above uses, so
  # gaze_status_ui/gaze_ready below can be built the same way
  # plot_status_ui/plot_ready already are.
  gaze_traj_result <- reactive({
    sf <- selected_source_file()
    if (is.null(sf)) {
      return(NULL)
    }
    load_gaze_trajectory_data(sf)
  })

  output$gaze_ready <- reactive({
    result <- gaze_traj_result()
    isTRUE(!is.null(result) && result$ok)
  })
  outputOptions(output, "gaze_ready", suspendWhenHidden = FALSE)

  output$gaze_status_ui <- renderUI({
    result <- gaze_traj_result()
    if (is.null(result)) {
      return(div(class = "alert alert-info", "Select a row in the QC table tab to explore its gaze trajectory here."))
    }
    if (!result$ok) {
      return(div(class = "alert alert-danger", strong("Could not load trajectory data: "), result$error))
    }
    div(class = "alert alert-success", sprintf("Exploring: %s", result$preproc_path))
  })

  # gaze_event_windows: derive_event_marker_windows()'s (analyze_helpers.R)
  # output for the currently loaded file's events, or NULL if that file has
  # no usable event markers -- a real, expected case (head-mounted adapters
  # with no stimulus-marker integration, or any recording that simply had
  # no logged events -- see load_gaze_trajectory_data()'s own comment on
  # this graceful degradation).
  gaze_event_windows <- reactive({
    result <- gaze_traj_result()
    if (is.null(result) || !isTRUE(result$ok)) {
      return(NULL)
    }
    derive_event_marker_windows(result$events)
  })

  # aoi_polygon: the currently defined AOI (list(x = ..., y = ...), the
  # shape compute_aoi_percent()/build_gaze_trajectory_plot() expect), or
  # NULL before one has been defined. A plain reactiveVal, set only by the
  # modal's own "Set AOI" submit handler and the "Clear AOI" button below --
  # deliberately NOT derived directly from the modal's numericInputs, since
  # those exist and hold values (0, by default) the entire time the modal is
  # open but not yet submitted; deriving straight from them would make an
  # AOI editing session redraw the trajectory plot and recompute the AOI
  # percentage on every single keystroke, against a still-incomplete
  # polygon, instead of only once the user has finished specifying all 4
  # corners and clicked "Set AOI".
  aoi_polygon <- reactiveVal(NULL)

  # --- Onion-skin playback state (repo-owner feature request) ---
  #
  # gaze_anim_playing: TRUE while the animated view is actively advancing,
  # FALSE in every other state (initial load, paused, or reset). This is the
  # single source of truth output$gaze_trajectory_plot below branches on --
  # FALSE always means "show the plain, unchanged static
  # build_gaze_trajectory_plot() view, exactly what this tab showed before
  # this feature existed" (the chosen, documented behavior for Pause: it
  # reverts to the static view rather than freezing the last animated
  # frame, so "not playing" only ever has one visual meaning in this tab).
  #
  # gaze_anim_index: a 1-based row position into
  # prepare_gaze_trajectory_data()'s cleaned/sorted view of the CURRENT
  # filtered_trajectory() window (analyze_helpers.R) -- the "current
  # playback position" every animated frame is drawn relative to. Reset to 1
  # (start of window) whenever the window/file/AOI changes, per the
  # observeEvent() immediately below.
  gaze_anim_playing <- reactiveVal(FALSE)
  gaze_anim_index <- reactiveVal(1L)

  # gaze_anim_tick_interval_ms / gaze_anim_step_samples: fixed playback
  # tuning constants, not exposed as controls (only "Trail length" is, per
  # this feature's spec) -- 150ms is comfortably inside the 100-250ms range
  # that reads as smooth, watchable playback without visibly stuttering; 5
  # samples/tick was chosen so a several-thousand-sample window animates
  # through in a reviewable amount of time rather than crawling one sample
  # at a time.
  gaze_anim_tick_interval_ms <- 150
  gaze_anim_step_samples <- 5L

  # gaze_prepared_df: prepare_gaze_trajectory_data()'s (analyze_helpers.R)
  # cleaned/sorted view of the current filtered_trajectory() window -- the
  # same row ordering gaze_anim_index() indexes into, and the bound used
  # below to know when playback has reached the end of the window (for the
  # loop-back-to-start behavior chosen for "end of range", see the tick loop
  # below). Recomputed only when filtered_trajectory() itself changes
  # (slider drag or file switch), not on every animation tick.
  gaze_prepared_df <- reactive({
    prepare_gaze_trajectory_data(filtered_trajectory())
  })

  # gaze_bg_plot: the always-visible, very-low-opacity background trace for
  # the CURRENT window -- built once per slider/file change via this
  # reactive's own normal memoization (it depends only on
  # filtered_trajectory(), never on gaze_anim_index()), then reused
  # unmodified by every animation tick's frame below. See
  # build_gaze_trajectory_background_trace()'s own comment
  # (analyze_helpers.R) for why this split exists: rebuilding this trace
  # from every usable row in the window on every ~150ms tick is exactly the
  # per-tick cost this split avoids -- only the small trail trace (at most
  # gaze_trail_length rows) is rebuilt per tick, in output$gaze_trajectory_plot
  # below.
  gaze_bg_plot <- reactive({
    build_gaze_trajectory_background_trace(filtered_trajectory())
  })

  # Stops/resets playback the instant the window this animation is playing
  # over becomes stale -- a different file, a moved time-range slider, or a
  # newly defined/cleared AOI all invalidate "the current playback position
  # means something", so continuing to play would either error against an
  # out-of-range index or silently animate a now-inconsistent window/AOI
  # combination. Mirrors the observeEvent(selected_source_file(), ...) reset
  # pattern immediately below (and is intentionally a SEPARATE observer from
  # it, not merged in, since this one also needs to fire on slider moves and
  # AOI changes that observer doesn't reset for).
  observeEvent(list(selected_source_file(), input$gaze_time_range, aoi_polygon()), {
    gaze_anim_playing(FALSE)
    gaze_anim_index(1L)
  }, ignoreInit = TRUE)

  # Play/Pause toggle: flips gaze_anim_playing() and, when transitioning INTO
  # "playing", starts from the beginning of the window unless resuming from
  # a genuine mid-window pause (gaze_anim_index() not yet at/past the end) --
  # so pressing Play again right after Pause continues from where playback
  # left off, while pressing Play after a completed/looped run (or on first
  # use) starts over from sample 1. isolate()d reads throughout: this
  # handler is meant to fire only on the button click, not re-run whenever
  # gaze_anim_index()/gaze_prepared_df() themselves change.
  observeEvent(input$gaze_play_toggle, {
    now_playing <- !isTRUE(isolate(gaze_anim_playing()))
    if (now_playing) {
      prepared <- isolate(gaze_prepared_df())
      if (is.null(prepared) || nrow(prepared) == 0) {
        return(invisible(NULL))
      }
      if (isolate(gaze_anim_index()) >= nrow(prepared)) {
        gaze_anim_index(1L)
      }
    }
    gaze_anim_playing(now_playing)
  })

  observeEvent(gaze_anim_playing(), {
    updateActionButton(
      session, "gaze_play_toggle",
      label = if (isTRUE(gaze_anim_playing())) "Pause" else "Play"
    )
  })

  # The tick loop: the standard shiny conditional-polling pattern used
  # elsewhere in this app (see e.g. the batch-run progress-bar poll and the
  # "Live auto-refresh" observer, both above) -- invalidateLater() is only
  # ever reached, and therefore only ever reschedules itself, while
  # gaze_anim_playing() is TRUE; the instant it flips FALSE (Pause, or the
  # reset observer above firing), the early return() is hit instead and this
  # chain simply stops. gaze_prepared_df()/gaze_anim_index() are read via
  # isolate() so this observer's only real reactive dependency is
  # gaze_anim_playing() itself, not a self-triggering loop off of the very
  # index value it writes each tick.
  #
  # End-of-range behavior (a deliberate choice, not left to error): when the
  # next step would run past the end of the current window, playback loops
  # back to sample 1 and keeps playing, rather than stopping -- chosen so a
  # user reviewing a segment can watch it repeat without re-clicking Play
  # each time it finishes.
  observe({
    if (!isTRUE(gaze_anim_playing())) {
      return(invisible(NULL))
    }
    n <- isolate({
      prepared <- gaze_prepared_df()
      if (is.null(prepared)) 0L else nrow(prepared)
    })
    if (n == 0) {
      gaze_anim_playing(FALSE)
      return(invisible(NULL))
    }

    next_idx <- isolate(gaze_anim_index()) + gaze_anim_step_samples
    if (next_idx > n) {
      next_idx <- 1L
    }
    gaze_anim_index(next_idx)

    invalidateLater(gaze_anim_tick_interval_ms, session)
  })

  # Resets every piece of this tab's per-file state -- the time-range
  # slider's bounds and any previously defined AOI -- the instant a
  # DIFFERENT file becomes selected (default ignoreNULL = TRUE: this simply
  # doesn't fire while nothing is selected yet, or on a deselect back to
  # NULL, since the whole tab is hidden via conditionalPanel(output.gaze_ready)
  # in either of those states anyway -- nothing to reset for). Without this,
  # switching from one recording to another would silently carry over the
  # previous recording's slider bounds (meaningless against a new file's own
  # timestamp range) and, worse, its AOI polygon -- drawn in one recording's
  # gaze-coordinate space, silently reapplied as if it still meant something
  # against a different recording's data.
  observeEvent(selected_source_file(), {
    aoi_polygon(NULL)

    result <- gaze_traj_result()
    ts <- if (!is.null(result) && isTRUE(result$ok)) {
      result$data$recordingTimestamp_ms[!is.na(result$data$recordingTimestamp_ms)]
    } else {
      numeric(0)
    }

    if (length(ts) == 0) {
      updateSliderInput(session, "gaze_time_range", min = 0, max = 1, value = c(0, 1))
    } else {
      updateSliderInput(session, "gaze_time_range", min = min(ts), max = max(ts), value = c(min(ts), max(ts)))
    }
  })

  # gaze_event_marker_ui: an event-marker "jump to" selector, only rendered
  # when the current file actually has usable event markers -- a file with
  # none (gaze_event_windows() returns NULL) gets a plain explanatory
  # message instead of an empty/disabled dropdown, so it's immediately
  # obvious this particular file has no markers to snap to (P10-06-style
  # adapter-aware degradation, surfaced directly in the UI here rather than
  # only in a QC column).
  output$gaze_event_marker_ui <- renderUI({
    windows <- gaze_event_windows()
    if (is.null(windows) || nrow(windows) == 0) {
      return(div(
        class = "text-muted",
        "No event markers found for this recording -- use the free time-range slider below."
      ))
    }
    choices <- stats::setNames(
      windows$event,
      sprintf(
        "%s (%d occurrence%s, %.0f–%.0f ms)",
        windows$event, windows$n_occurrences,
        ifelse(windows$n_occurrences == 1, "", "s"),
        windows$start_ms, windows$end_ms
      )
    )
    fluidRow(
      column(8, selectInput("gaze_event_marker", "Jump to event marker", choices = choices)),
      column(4, br(), actionButton("gaze_snap_to_marker", "Snap slider to this marker"))
    )
  })

  # Snapping sets the slider's CURRENT value to the selected marker's
  # [start_ms, end_ms] window (that event label's first-to-last occurrence,
  # per derive_event_marker_windows()) without touching the slider's min/max
  # BOUNDS -- the user can immediately drag either handle wider or narrower
  # from there, never constrained back to only this exact window. This is
  # what satisfies this tab's "snap as a convenient default, but never lock
  # the range to exact event boundaries" requirement: sliderInput itself
  # imposes no such constraint once its value is set this way, it's a
  # perfectly ordinary drag-both-ways range control from this point on.
  observeEvent(input$gaze_snap_to_marker, {
    windows <- gaze_event_windows()
    req(windows, input$gaze_event_marker)
    row <- windows[windows$event == input$gaze_event_marker, , drop = FALSE]
    req(nrow(row) == 1)
    updateSliderInput(session, "gaze_time_range", value = c(row$start_ms[1], row$end_ms[1]))
  })

  # filtered_trajectory: the loaded preproc data.frame narrowed to the
  # slider's current [start, end] -- the single reactive both the trajectory
  # plot and the AOI percentage readout below are built from, so they can
  # never show two different time ranges out of sync with each other. Note:
  # right after a file switch, this may transiently evaluate against the
  # PREVIOUS file's slider value for one reactive flush (updateSliderInput()
  # above is a client round-trip, not synchronous) -- self-corrects as soon
  # as the browser echoes the new bounds back, same as any other
  # updateSliderInput()-driven reset in a Shiny app.
  filtered_trajectory <- reactive({
    result <- gaze_traj_result()
    req(result, result$ok)
    rng <- input$gaze_time_range
    req(rng)
    data <- result$data
    ts <- data$recordingTimestamp_ms
    data[!is.na(ts) & ts >= rng[1] & ts <= rng[2], , drop = FALSE]
  })

  # output$gaze_trajectory_plot: branches between the plain, unchanged
  # static view (gaze_anim_playing() FALSE -- the only value it ever has
  # outside of an active Play session, per the reset observer and Pause
  # behavior documented above) and the animated onion-skin view. The
  # animated branch reuses gaze_bg_plot()'s CACHED background trace and only
  # rebuilds the small trail trace here, per-tick -- see this section's
  # earlier header comment on why that split matters for a several-thousand-
  # sample recording.
  output$gaze_trajectory_plot <- renderPlotly({
    df <- filtered_trajectory()
    validate(need(nrow(df) > 0, "No gaze samples in the current time range."))

    if (!isTRUE(gaze_anim_playing())) {
      plot <- build_gaze_trajectory_plot(df, aoi_polygon())
      validate(need(!is.null(plot), "No usable (non-missing) gaze coordinates in the current time range."))
      return(plot)
    }

    bg <- gaze_bg_plot()
    validate(need(!is.null(bg), "No usable (non-missing) gaze coordinates in the current time range."))

    trail_length <- input$gaze_trail_length
    if (is.null(trail_length) || is.na(trail_length) || trail_length < 1) {
      trail_length <- 90
    }

    plot <- build_gaze_trajectory_trail_trace(bg, df, gaze_anim_index(), trail_length)
    plot <- add_aoi_overlay_trace(plot, aoi_polygon())
    plotly::layout(
      plot,
      xaxis = list(title = "gazeX.preprocessed_px"),
      yaxis = list(title = "gazeY.preprocessed_px"),
      showlegend = TRUE
    )
  })

  # gaze_aoi_result: compute_aoi_percent()'s (analyze_helpers.R) output for
  # the CURRENT time-range selection -- recomputed on every slider move (via
  # filtered_trajectory()) and every AOI define/clear, so the readout below
  # always describes the exact same rows the trajectory plot is showing.
  gaze_aoi_result <- reactive({
    compute_aoi_percent(filtered_trajectory(), aoi_polygon())
  })

  output$gaze_aoi_status <- renderUI({
    aoi <- aoi_polygon()
    if (is.null(aoi)) {
      return(div(class = "text-muted", "No AOI defined yet."))
    }
    result <- gaze_aoi_result()
    if (is.null(result)) {
      return(div(class = "text-muted", "No gaze samples in the current time range to test against the AOI."))
    }
    div(
      class = "alert alert-info",
      sprintf(
        "%d of %d gaze point(s) in the current time range (%.1f%%) fall inside the AOI.",
        result$n_inside, result$n_total, result$pct * 100
      )
    )
  })

  # AOI definition modal: 4 numeric (x, y) corner inputs -- the simplest
  # workable v1, closest to app_lana.R's own reference implementation of
  # this same idea (see that file's own header comment for why it's kept as
  # an unintegrated reference rather than reused directly). A plotly
  # click-to-draw interaction would read nicer, but plotly's click/relayout
  # event payloads for a hand-drawn shape aren't a stable, first-class Shiny
  # input the way e.g. plotly_click point-selection already is, and building
  # that reliably was judged not worth it for a v1 whose main job is proving
  # out the underlying AOI machinery (compute_aoi_percent()/
  # points_in_polygon(), analyze_helpers.R) end to end -- swapping this modal
  # for a click-to-draw interaction later would only touch this UI, not that
  # machinery.
  observeEvent(input$gaze_define_aoi, {
    showModal(modalDialog(
      title = "Define the AOI's corners (in the same gaze coordinate units as the plot)",
      fluidRow(
        column(6, numericInput("gaze_aoi1x", "Corner 1 x", value = 0)),
        column(6, numericInput("gaze_aoi1y", "Corner 1 y", value = 0))
      ),
      fluidRow(
        column(6, numericInput("gaze_aoi2x", "Corner 2 x", value = 0)),
        column(6, numericInput("gaze_aoi2y", "Corner 2 y", value = 0))
      ),
      fluidRow(
        column(6, numericInput("gaze_aoi3x", "Corner 3 x", value = 0)),
        column(6, numericInput("gaze_aoi3y", "Corner 3 y", value = 0))
      ),
      fluidRow(
        column(6, numericInput("gaze_aoi4x", "Corner 4 x", value = 0)),
        column(6, numericInput("gaze_aoi4y", "Corner 4 y", value = 0))
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("gaze_aoi_submit", "Set AOI")
      )
    ))
  })

  observeEvent(input$gaze_aoi_submit, {
    aoi_polygon(list(
      x = c(input$gaze_aoi1x, input$gaze_aoi2x, input$gaze_aoi3x, input$gaze_aoi4x),
      y = c(input$gaze_aoi1y, input$gaze_aoi2y, input$gaze_aoi3y, input$gaze_aoi4y)
    ))
    removeModal()
  })

  observeEvent(input$gaze_clear_aoi, {
    aoi_polygon(NULL)
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
  # itself: a genuine load (a possibly brand-new directory with a different
  # file set) should reset these selections, but an auto-refresh tick
  # against the SAME directory should not -- the metric set itself never
  # changes file to file (every qcsummary.tsv carries the same fixed ~32
  # qc_metric rows), so there's nothing to gain by re-populating these
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
      updateSelectizeInput(session, "compare_metric_plot", choices = character(0), selected = character(0))
      updateSelectizeInput(session, "compare_metrics_table", choices = character(0), selected = character(0))
      return()
    }

    configured <- unique(qc_threshold_config$qc_metric[qc_threshold_config$qc_metric %in% choices])

    # Defaults to a single selected metric (a bar chart) even though this
    # control now allows up to 2 (a scatterplot) -- a fresh load defaulting
    # straight to a 2-metric scatterplot would pick an arbitrary metric PAIR
    # with no obvious rationale, where a single default metric (the first
    # configured/thresholdable one, same as before this control allowed a
    # second selection at all) is still the most useful "just loaded, show
    # me something" starting point; a user opts into the 2-metric view
    # deliberately by adding a second selection themselves.
    default_plot_metric <- if (length(configured) > 0) configured[1] else choices[1]
    updateSelectizeInput(session, "compare_metric_plot", choices = choices, selected = default_plot_metric)

    default_table_metrics <- if (length(configured) > 0) configured else choices[1]
    updateSelectizeInput(
      session, "compare_metrics_table",
      choices = choices, selected = default_table_metrics
    )
  }, ignoreInit = TRUE)

  # Repopulates the batch_name (run) filter, same pattern and same
  # manual_load_trigger()-only reasoning as the metric-choice observer just
  # above -- defaulting to every currently loaded batch_name selected
  # (compare_batch_name_choices(), analyze_helpers.R, sorted with a trailing
  # "(none)" for derive_batch_name()'s NA_character_ case) so a fresh load
  # starts from "show everything". An auto-refresh tick that picks up a
  # brand-new batch_name (e.g. a Setup tab run started under a second
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

  # Repopulates the task_name filter, exact mirror of the batch_name (run)
  # observer directly above -- same manual_load_trigger()-only reasoning,
  # same "default to every currently loaded task_name selected" behavior
  # (compare_task_name_choices(), analyze_helpers.R, sorted with a trailing
  # "(none)" for derive_task_label()'s NA_character_ case), same
  # no-auto-select-on-a-later-tick behavior for a newly appeared task_name.
  observeEvent(manual_load_trigger(), {
    choices <- compare_task_name_choices(current_load_result()$table)
    updateSelectizeInput(
      session, "compare_task_name_filter",
      choices = choices, selected = choices
    )
  }, ignoreInit = TRUE)

  output$compare_status_ui <- renderUI({
    if (length(compare_metric_choices()) == 0) {
      return(div(class = "alert alert-info", "Load qcsummary files first to compare metrics across files."))
    }
    NULL
  })

  # build_qc_comparison_plot()/build_qc_comparison_scatter() (analyze_helpers.R)
  # are handed qc_table_flagged() -- not current_load_result()$table directly
  # -- so bar/point coloring always matches the QC table's own row
  # highlighting for the same metric(s) (both trace back to the same
  # compute_qc_flags() call), rather than this view recomputing pass/fail
  # independently and risking drift from P10-02's flagging. Narrowed to the
  # batch_name(s) currently selected in the filter above via
  # filter_by_batch_name() (analyze_helpers.R) before being handed to either
  # builder -- both stay agnostic to which batch_name(s) are in view, the
  # same way they're agnostic to which directory was loaded.
  #
  # 1 selected metric -> the original bar chart; 2 -> a scatterplot instead
  # (this task's own scope decision, see compare_metric_plot's UI comment).
  # 0 (cleared) or >2 (shouldn't happen -- the UI's maxItems = 2 already
  # prevents it, but req() guards defensively anyway) render nothing.
  output$compare_plot <- renderPlot(
    {
      metrics <- input$compare_metric_plot
      req(length(metrics) %in% c(1, 2))
      tbl <- qc_table_flagged()
      req(tbl)
      tbl <- filter_by_batch_name(tbl, input$compare_batch_name_filter)
      tbl <- filter_by_task_name(tbl, input$compare_task_name_filter)
      plot <- if (length(metrics) == 1) {
        build_qc_comparison_plot(tbl, metrics[1], qc_thresholds())
      } else {
        build_qc_comparison_scatter(tbl, metrics[1], metrics[2], qc_thresholds())
      }
      req(plot)
      plot
    },
    # Bar-chart case (length(metrics) == 1): scales with the number of bars
    # the currently selected metric will produce, rather than a fixed pixel
    # height -- a static height squeezed 100+ files' bars/labels down to
    # illegibility in field testing. 28px per bar (roomy enough for the
    # legible font sizes set in build_qc_comparison_plot()) plus fixed space
    # for the title/axis, with a 500px floor so a small file count doesn't
    # render a cramped sliver. Filtered by the batch_name selection the same
    # way the plot itself is, above, so the container height matches the
    # actual bar count being drawn rather than the full, unfiltered file
    # count. This exact formula is UNCHANGED from before the 2-metric
    # scatterplot option existed -- a fixed height is used for the
    # scatterplot case instead (it doesn't grow with file count the way a
    # per-file bar chart does; every file is just one more point on the same
    # fixed-size plot area).
    height = function() {
      metrics <- input$compare_metric_plot
      tbl <- qc_table_flagged()
      if (is.null(tbl) || is.null(metrics) || length(metrics) == 0) {
        return(500)
      }
      if (length(metrics) == 1) {
        tbl <- filter_by_batch_name(tbl, input$compare_batch_name_filter)
        tbl <- filter_by_task_name(tbl, input$compare_task_name_filter)
        n <- sum(tbl$qc_metric == metrics[1], na.rm = TRUE)
        return(max(500, n * 28 + 120))
      }
      550
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
  # (analyze_helpers.R) before pivoting -- same reasoning as
  # output$compare_plot above, build_qc_comparison_table() itself stays
  # agnostic to which batch_name(s) are in view.
  output$compare_table <- renderDT({
    metrics <- input$compare_metrics_table
    validate(need(length(metrics) > 0, "Select at least one QC metric above."))
    filtered <- filter_by_batch_name(current_load_result()$table, input$compare_batch_name_filter)
    filtered <- filter_by_task_name(filtered, input$compare_task_name_filter)
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
}

shinyApp(ui = ui, server = server)
