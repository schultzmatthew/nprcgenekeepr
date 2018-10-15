#' summary.nprcmanagerErr Summary function for class nprcmanagErr
#'
#' @return object of class summary.nprcmanagErr
#'
#' @rdname summary
#' @method summary nprcmanagErr
#' @param object object of class nprcmanagErr and class list
#' @param ... additional arguments for the \code{summary.default} statement
#' @importFrom stringi stri_c
#' @export
summary.nprcmanagErr <- function(object, ...) {
  errorLst <- object
  stopifnot(inherits(errorLst, "nprcmanagErr"))
  txt <- ""
  txt <- addErrTxt(txt, errorLst$missingColumns,
                   "missing column is",
                   "missing columns are")
  txt <- addErrTxt(txt, errorLst$invalidDateRows,
                   "row having an invalid date is",
                   "rows (up to the first 5) having an invalid date are")
  txt <- addErrTxt(txt, errorLst$sireIsDam,
                   "animal listed as both a sire and dam is",
                   "animals listed as both sire and dam are")
  txt <- addErrTxt(txt, errorLst$femaleSires,
                   "animal listed as a sire and female is",
                   "animals listed as sires and female are")
  txt <- addErrTxt(txt, errorLst$maleDams,
                   "animal listed as a dam and male is",
                   "animals listed as dams and male are")
  txt <- addErrTxt(txt, errorLst$duplicateIds,
                   "animal listed more than once is",
                   "animals listed more than once are")
  txt <- addErrTxt(txt, errorLst$changedCols$caseChange,
                   "column where case was changed is",
                   "columns where case was changed are")
  txt <- addErrTxt(txt, errorLst$changedCols$spaceRemoved,
                   "column where space was removed is",
                   "columns where space was removed are")
  txt <- addErrTxt(txt, errorLst$changedCols$periodRemoved,
                   "column where period was removed is",
                   "columns where period was removed are")
  txt <- addErrTxt(txt, errorLst$changedCols$underScoreRemoved,
                   "column where underscore was removed is",
                   "columns where underscore was removed are")
  txt <- addErrTxt(txt, errorLst$changedCols$egoidToId,
                   "column changed from",
                   "columns changed from")
  txt <- addErrTxt(txt, errorLst$changedCols$sireIdToSire,
                   "column changed from",
                   "columns changed from")
  txt <- addErrTxt(txt, errorLst$changedCols$damIdToDam,
                   "column changed from",
                   "columns changed from")
  txt <- addErrTxt(txt, errorLst$changedCols$birthdateToBirth,
                   "column changed from",
                   "columns changed from")
  txt <- addErrTxt(txt, errorLst$changedCols$deathdateToDeath,
                   "column changed from",
                   "columns changed from")
  txt <- list(txt = txt, sp = errorLst$suspiciousParents)

  class(txt) <- "summary.nprcmanagErr"
  txt
}
