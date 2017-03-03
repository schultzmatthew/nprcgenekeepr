#' @export
runManager <- function() {
  appDir <- system.file("application", "myapp", package = "nprcmanager")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `nprcmanager`.",
         call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
}