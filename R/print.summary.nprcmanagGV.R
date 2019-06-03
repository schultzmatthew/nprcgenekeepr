#' print.summary.nprcmanagGV
#'
#' @return object to send to generic print function
#'
#' @rdname print
#' @method print summary.nprcmanagGV
#' @param txt object of class summary.nprcmanagGV
#' @param ... additional arguments for the \code{print.default} statement
#' @export
print.summary.nprcmanagGV <- function(txt, ...) {
  cl <- oldClass(txt)
  #cat("This is a summary printout from nprcmanagGV\n\n")
  for (x in txt) {
    cat(x, "\n")
  }
  oldClass(txt) <- cl[cl != "nprcmanagGV"]
  #NextMethod("print")
  invisible(txt)
}
