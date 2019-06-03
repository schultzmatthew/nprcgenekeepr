#' summary.nprcmanagGV Summary function for class nprcmanagGV
#'
#' @return object of class summary.nprcmanagGV
#'
#' @rdname summary
#' @method summary nprcmanagGV
#' @param object object of class nprcmanagGV and class list
#' @param ... additional arguments for the \code{summary.default} statement
#' @importFrom stringi stri_c
#' @export
summary.nprcmanagGV <- function(object, ...) {
  gvReport <- object
  stopifnot(inherits(gvReport, "nprcmanagGV"))
  rpt <- gvReport[["report"]]
  kmat <- gvReport[["kinship"]]
  f <- gvReport[["total"]]
  mf <- gvReport[["maleFounders"]]
  ff <- gvReport[["femaleFounders"]]
  fe <- gvReport[["fe"]]
  fg <- gvReport[["fg"]]
  txt <- "The genetic value report"
  txt <- c(txt, stri_c("Individuals in Pedigree: ", nrow(rpt)))
  txt <- c(txt, stri_c("Male Founders: ", mf, "\nFemale Founders: ", ff, "\nTotal Founders: ", f))
  txt <- c(txt, stri_c("Founder Equivalents: ", round(fe, 2)))
  txt <- c(txt, stri_c("Founder Genome Equivalents: ", round(fg, 2)))
  txt <- c(txt, stri_c("Live Offspring: ", sum(rpt$livingOffspring)))
  txt <- c(txt, stri_c("High Value Individuals: ", nrow(rpt[rpt$value == "High Value", ])))
  txt <- c(txt, stri_c("Low Value Individuals: ", nrow(rpt[rpt$value == "Low Value", ])))
  class(txt) <- "summary.nprcmanagGV"
  txt
}
