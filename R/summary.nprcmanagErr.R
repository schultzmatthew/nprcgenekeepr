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
                   "Error: The missing column is",
                   "Error: The missing columns are")
  txt <- addErrTxt(txt, errorLst$invalidDateRows,
                   "Error: The row having an invalid date is",
                   "Error: The rows (up to the first 5) having an invalid date are")
  txt <- addErrTxt(txt, errorLst$sireIsDam,
                   "Error: The animal listed as both a sire and dam is",
                   "Error: The animals listed as both sire and dam are")
  txt <- addErrTxt(txt, errorLst$femaleSires,
                   "Error: The animal listed as a sire and female is",
                   "Error: The animals listed as sires and female are")
  txt <- addErrTxt(txt, errorLst$maleDams,
                   "Error: The animal listed as a dam and male is",
                   "Error: The animals listed as dams and male are")
  txt <- addErrTxt(txt, errorLst$duplicateIds,
                   "Error: The animal listed more than once is",
                   "Error: The animals listed more than once are")
  txt <- addErrTxt(txt, errorLst$changedCols$caseChange,
                   "Change: The column where case was changed is",
                   "Change: The columns where case was changed are")
  txt <- addErrTxt(txt, errorLst$changedCols$spaceRemoved,
                   "Change: The column where space was removed is",
                   "Change: The columns where space was removed are")
  txt <- addErrTxt(txt, errorLst$changedCols$periodRemoved,
                   "Change: The column where period was removed is",
                   "Change: The columns where period was removed are")
  txt <- addErrTxt(txt, errorLst$changedCols$underScoreRemoved,
                   "Change: The column where underscore was removed is",
                   "Change: The columns where underscore was removed are")
  txt <- addErrTxt(txt, errorLst$changedCols$egoToId,
                   "Change: The column changed from",
                   "Change: The columns changed from")
  txt <- addErrTxt(txt, errorLst$changedCols$egoidToId,
                   "Change: The column changed from",
                   "Change: The columns changed from")
  txt <- addErrTxt(txt, errorLst$changedCols$sireIdToSire,
                   "Change: The column changed from",
                   "Change: The columns changed from")
  txt <- addErrTxt(txt, errorLst$changedCols$damIdToDam,
                   "Change: The column changed from",
                   "Change: The columns changed from")
  txt <- addErrTxt(txt, errorLst$changedCols$birthdateToBirth,
                   "Change: The column changed from",
                   "Change: The columns changed from")
  txt <- addErrTxt(txt, errorLst$changedCols$deathdateToDeath,
                   "Change: The column changed from",
                   "Change: The columns changed from")
  txt <- list(txt = txt, sp = errorLst$suspiciousParents)

  class(txt) <- "summary.nprcmanagErr"
  txt
}
