#' print.summary.nprcmanagErr print.summary.nprcmanagGV
#'
#' @return object to send to generic print function
#'
#' @rdname print
#' @method print summary.nprcmanagErr
#' @param x object of class summary.nprcmanagErr and class list
#' @param ... additional arguments for the \code{summary.default} statement
#' @importFrom stringi stri_c
#' @export
print.summary.nprcmanagErr <- function(x, ...) {
  cl <- oldClass(x)
  txt <- x
  #cat("This is a summary printout from nprcmanager\n\n")
  for (x in txt$txt) {
    cat(x, "\n")
  }
  if (nrow(txt$sp) > 0) {
    cat(stri_c("Animal records where parent records are suspicous because ",
               "of dates.\n",
               "One or more parents appear too young at time of birth.\n"))
    print(txt$sp, digits = 2, row.names = TRUE, ...)
  }
  oldClass(txt) <- cl[cl != "nprcmanagErr"]
  #NextMethod("print")
  invisible(txt)
}
#' @rdname print
#' @return object to send to generic print function
#' @method print summary.nprcmanagGV
#' @export
print.summary.nprcmanagGV <- function(x, ...) {
  cl <- oldClass(x)
  #cat("This is a summary printout from nprcmanagGV\n\n")
  for (line in x) {
    cat(line, "\n")
  }
  oldClass(x) <- cl[cl != "nprcmanagGV"]
  #NextMethod("print")
  invisible(x)
}
