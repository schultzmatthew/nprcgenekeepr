#' print.summary.nprcmanagErr
#'
#' @return object to send to generic print function
#'
#' @rdname print
#' @method print summary.nprcmanagErr
#' @param x object of class summary.nprcmanagErr
#' @param ... additional arguments for the \code{print.default} statement
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
