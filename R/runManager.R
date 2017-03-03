#' @export
runManager <- function() {
  appDir <- system.file("application", package = "nprcmanager")
  if (appDir == "") {
    stop("Could not find application directory. Try re-installing `nprcmanager`.",
         call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}