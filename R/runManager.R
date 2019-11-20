#' Allows running \code{shiny} application with \code{nprcmanager::runManager()}
#'
## Copyright(c) 2017-2019 R. Mark Sharp
## This file is part of nprcmanager
#' @export
runManager <- function() {
  appDir <- system.file("application", package = "nprcmanager")
  if (appDir == "") {
    stop(paste0("Could not find application directory. ",
                "Try re-installing `nprcmanager`."),
         call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal", port = 6012)
}
