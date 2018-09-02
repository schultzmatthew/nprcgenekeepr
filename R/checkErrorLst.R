#' checkErrorLst examines list for non-empty fields
#'
#' @return Returns \code{NULL} is all fields are empty
#' else the entire list is returned.
#' @param errorLst list with fields for each type of error detectable by
#' \code{qcStudbook}.
checkErrorLst <- function(errorLst) {
  if (length(errorLst$missingColumns) > 0 |
      length(errorLst$invalidDateRows) > 0 |
      length(errorLst$sireIsDam) > 0 |
      length(errorLst$femaleSires) > 0 |
      length(errorLst$maleDams) > 0 |
      length(errorLst$duplicateIds) > 0 |
      nrow(errorLst$suspiciousParents) > 0 |
      length(errorLst$changedCols$caseChange) > 0 |
      length(errorLst$changedCols$spaceRemoved) > 0 |
      length(errorLst$changedCols$backslashRemoved) > 0 |
      length(errorLst$changedCols$underScoreRemoved) > 0 |
      length(errorLst$changedCols$egoidToId) > 0 |
      length(errorLst$changedCols$sireIdToSire) > 0 |
      length(errorLst$changedCols$damIdToDam) > 0 |
      length(errorLst$changedCols$birthdateToBirth) > 0 |
      length(errorLst$changedCols$deathdateToDeath) > 0) {
    return(errorLst)
  } else {
    return(NULL)
  }
}
