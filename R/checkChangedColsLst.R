#' checkChangedColsLst examines list for non-empty fields
#'
## Copyright(c) 2017-2019 R. Mark Sharp
## This file is part of nprcgenekeepr
#' @return Returns \code{NULL} if all fields are empty
#' else the entire list is returned.
#' @param changedCols list with fields for each type of column change
#' \code{qcStudbook}.
checkChangedColsLst <- function(changedCols) {
  if (length(changedCols$caseChange) > 0 |
      length(changedCols$spaceRemoved) > 0 |
      length(changedCols$periodRemoved) > 0 |
      length(changedCols$underScoreRemoved) > 0 |
      length(changedCols$egoToId) > 0 |
      length(changedCols$egoidToId) > 0 |
      length(changedCols$sireIdToSire) > 0 |
      length(changedCols$damIdToDam) > 0 |
      length(changedCols$birthdateToBirth) > 0 |
      length(changedCols$deathdateToDeath) > 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
