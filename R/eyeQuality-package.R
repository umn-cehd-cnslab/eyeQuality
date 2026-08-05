#' @keywords internal
"_PACKAGE"

.onLoad <- function(libname, pkgname) {
  # Register the built-in eye tracker adapters (R/tobii-studio-adapter.R,
  # R/tobii-pro-adapter.R) every time the package is loaded, rather than
  # relying on the side effect of sourcing those files at build time --
  # see R/adapter-interface.R (P3-02) for the registry itself.
  register_eyetracker_adapter(tobii_studio_adapter)
  register_eyetracker_adapter(tobii_pro_adapter)
}

## usethis namespace: start
#' @importFrom data.table :=
#' @importFrom data.table .BY
#' @importFrom data.table .EACHI
#' @importFrom data.table .GRP
#' @importFrom data.table .I
#' @importFrom data.table .N
#' @importFrom data.table .NGRP
#' @importFrom data.table .SD
#' @importFrom data.table data.table
## usethis namespace: end
NULL
