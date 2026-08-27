# P10-12: eyeQualityApp(), the single combined entry point that replaced the
# separate runSetupApp()/runAnalyzeApp() wrappers (R/runSetupApp.R and
# R/runAnalyzeApp.R, both deleted -- no back-compat shims, this is a pre-1.0,
# never-CRAN-published package). See R/eyeQualityApp.R.
#
# eyeQualityApp() itself calls shiny::runApp(), which blocks until the app is
# closed -- not something a unit test can call for real.
# testthat::local_mocked_bindings(..., .package = "shiny") stands in for it,
# the same technique the now-deleted test-runSetupApp.R/test-runAnalyzeApp.R
# wrapper tests used.

app_dir <- system.file("shiny-apps", "app", package = "eyeQuality")
if (!nzchar(app_dir)) {
  stop("test-eyeQualityApp.R: could not locate inst/shiny-apps/app/ via system.file()")
}

test_that("eyeQualityApp() always sets shinyOptions(app_initialTab = 'Setup & Run'), unconditionally", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("shinyFiles")
  skip_if_not_installed("DT")

  testthat::local_mocked_bindings(runApp = function(...) invisible(NULL), .package = "shiny")
  on.exit(shiny::shinyOptions(app_initialTab = NULL), add = TRUE)

  eyeQuality::eyeQualityApp()

  expect_equal(shiny::getShinyOption("app_initialTab", NULL), "1. Setup & Run")
})

test_that("eyeQualityApp() with a non-NULL initialDirectory still sets app_initialTab = 'Setup & Run', not an Analyze-specific tab", {
  # Regression guard: pre-P10-12, only runAnalyzeApp() accepted
  # initialDirectory, and it set app_initialTab = "Analyze / QC Explorer".
  # The merged function keeps accepting initialDirectory (to pre-populate the
  # Analyze tabs' directory field) but always opens on Setup & Run -- there is
  # no more tab-selection argument.
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("shinyFiles")
  skip_if_not_installed("DT")

  testthat::local_mocked_bindings(runApp = function(...) invisible(NULL), .package = "shiny")
  on.exit(shiny::shinyOptions(app_initialTab = NULL, analyze_initialDirectory = NULL), add = TRUE)

  eyeQuality::eyeQualityApp(initialDirectory = "/some/study/outputs")

  expect_equal(shiny::getShinyOption("app_initialTab", NULL), "1. Setup & Run")
})

test_that("eyeQualityApp() forwards a valid single-string initialDirectory to shinyOptions(analyze_initialDirectory = ...) before shiny::runApp() would be reached", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("shinyFiles")
  skip_if_not_installed("DT")

  testthat::local_mocked_bindings(runApp = function(...) invisible(NULL), .package = "shiny")
  on.exit(shiny::shinyOptions(app_initialTab = NULL, analyze_initialDirectory = NULL), add = TRUE)

  eyeQuality::eyeQualityApp(initialDirectory = "/some/study/outputs")

  expect_equal(shiny::getShinyOption("analyze_initialDirectory", NULL), "/some/study/outputs")
})

test_that("eyeQualityApp() with no initialDirectory argument (the NULL default) leaves analyze_initialDirectory unset", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("shinyFiles")
  skip_if_not_installed("DT")

  testthat::local_mocked_bindings(runApp = function(...) invisible(NULL), .package = "shiny")
  on.exit(shiny::shinyOptions(app_initialTab = NULL, analyze_initialDirectory = NULL), add = TRUE)

  eyeQuality::eyeQualityApp()

  expect_null(shiny::getShinyOption("analyze_initialDirectory", NULL))
})

test_that("eyeQualityApp() errors clearly on a non-scalar initialDirectory", {
  expect_error(
    eyeQuality::eyeQualityApp(initialDirectory = c("/a", "/b")),
    "single path string"
  )
})

test_that("eyeQualityApp() errors clearly on an NA initialDirectory", {
  expect_error(
    eyeQuality::eyeQualityApp(initialDirectory = NA_character_),
    "single path string"
  )
})

test_that("eyeQualityApp() errors clearly on a non-character initialDirectory", {
  expect_error(
    eyeQuality::eyeQualityApp(initialDirectory = 123),
    "single path string"
  )
})

test_that("eyeQualityApp() resolves the merged app directory via system.file('shiny-apps', 'app', package = 'eyeQuality') and passes it to shiny::runApp()", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("shinyFiles")
  skip_if_not_installed("DT")

  captured_app_dir <- NULL
  testthat::local_mocked_bindings(
    runApp = function(appDir, ...) {
      captured_app_dir <<- appDir
      invisible(NULL)
    },
    .package = "shiny"
  )
  on.exit(shiny::shinyOptions(app_initialTab = NULL), add = TRUE)

  eyeQuality::eyeQualityApp()

  expect_equal(captured_app_dir, app_dir)
  expect_true(file.exists(file.path(captured_app_dir, "app.R")))
})

test_that("eyeQualityApp() forwards ... arguments through to shiny::runApp()", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("shinyFiles")
  skip_if_not_installed("DT")

  captured_args <- NULL
  testthat::local_mocked_bindings(
    runApp = function(appDir, ...) {
      captured_args <<- list(...)
      invisible(NULL)
    },
    .package = "shiny"
  )
  on.exit(shiny::shinyOptions(app_initialTab = NULL), add = TRUE)

  eyeQuality::eyeQualityApp(launch.browser = FALSE, port = 1234)

  expect_equal(captured_args$launch.browser, FALSE)
  expect_equal(captured_args$port, 1234)
})

test_that("eyeQualityApp() errors clearly when the merged app directory cannot be found via system.file()", {
  # Simulates a broken/incomplete installation (system.file() returns "" when
  # the requested path doesn't exist under the installed package), which
  # should surface as an actionable error rather than an opaque failure
  # further down in shiny::runApp().
  skip_on_cran()

  testthat::local_mocked_bindings(system.file = function(...) "")

  expect_error(
    eyeQuality::eyeQualityApp(),
    "Could not find the eyeQuality app directory"
  )
})

test_that("eyeQualityApp() errors clearly when a required Suggests-only package is not installed", {
  # requireNamespace() is a base function that eyeQualityApp() calls
  # unqualified, so local_mocked_bindings() needs `.package = "base"` (not
  # "eyeQuality", and not omitted) to find a binding to rebind -- confirmed
  # this is the only combination that works, since requireNamespace() is
  # neither imported into nor present in the eyeQuality namespace itself.
  skip_on_cran()

  testthat::local_mocked_bindings(requireNamespace = function(...) FALSE, .package = "base")

  expect_error(
    eyeQuality::eyeQualityApp(),
    "requires the 'shiny', 'shinyFiles', 'future', 'promises'"
  )
})