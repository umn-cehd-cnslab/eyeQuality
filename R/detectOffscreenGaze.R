#' Detect Offscreen Gazepoints
#'
#' @param data your dataframe
#' @param gazeX gaze point X coordinate
#' @param gazeY gaze point Y coordinate
#' @param displayResolutionX_px Integer, Display resolution X in pixels
#' @param displayResolutionY_px Integer Display resolution Y in pixels
#' @param minDisplayResolutionX_px Integer X minimum value within acceptable display resolution range Default 0 assumed for pixel space
#' @param minDisplayResolutionY_px Integer Y minimum value within acceptable display resolution range. Default 0 assumed for pixel space
#'
#' @return binary list to be saved to your dataframe as new column "offscreen.gp"
#' @export
#'
detectOffscreenGaze <-
  function(data,
           gazeX,
           gazeY,
           displayResolutionX_px,
           displayResolutionY_px,
           minDisplayResolutionX_px = 0,
           minDisplayResolutionY_px = 0) {
    # flag any gazepoints that may be offscreen based on acceptable input range
    offscreen.gp <- rep(NA, length(data[[gazeX]]))
    offscreen.gp <-
      dplyr::if_else(
        !between(data[[gazeX]], minDisplayResolutionX_px, displayResolutionX_px) |
          !between(data[[gazeY]], minDisplayResolutionY_px, displayResolutionY_px),
        "offscreen", "onscreen"
      )

    return(offscreen.gp)
  }
