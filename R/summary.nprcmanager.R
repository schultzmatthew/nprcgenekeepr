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
  txt <- "Changes made to pedigree file during import are listed below.\n"
  txt <- addErrTxt(txt, errorLst$missingColumns,
                   "missing column is",
                   "missing columns are")
  txt <- addErrTxt(txt, errorLst$invalidDateRows,
                   "row having an invalid date is",
                   "row (up to the first 5) having an invalid date are")
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
                   "column changed from egoid to id is",
                   "columns changed from egoid to id are")
  txt <- addErrTxt(txt, errorLst$changedCols$sireIdToSire,
                   "column changed from sireid to sire is",
                   "columns changed from sireid to sire are")
  txt <- addErrTxt(txt, errorLst$changedCols$damIdToDam,
                   "column changed from damid to dam is",
                   "columns changed from damid to dam are")
  txt <- addErrTxt(txt, errorLst$changedCols$birthdateToBirth,
                   "column changed from birthdate to birth is",
                   "columns changed from birthdate to birth are")
  txt <- addErrTxt(txt, errorLst$changedCols$deathdateToDeath,
                   "column changed from deathdate to death is",
                   "columns changed from deathdate to death are")
  txt <- list(txt = txt, sp = errorLst$suspiciousParents)

  class(txt) <- "summary.nprcmanagErr"
  txt
}
