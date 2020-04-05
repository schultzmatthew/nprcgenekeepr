#' Allows running \code{shiny} application with
#' \code{nprcgenekeepr::runGenekeepr()}
#'
## Copyright(c) 2017-2020 R. Mark Sharp
## This file is part of nprcgenekeepr
#' @export
runGenekeepr <- function() {
  appDir <- system.file("application", package = "nprcgenekeepr")
  if (appDir == "") {
    stop(paste0("Could not find application directory. ",
                "Try re-installing `nprcgenekeepr`."),
         call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal", port = 6012)
}
