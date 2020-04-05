#' checkChangedColAndErrorLst examines errorLst for errors and
#' errorLst$changeCols non-empty fields
## Copyright(c) 2017-2020 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' @return Returns \code{NULL} is all fields are empty
#' else the entire list is returned.
#'
#' @examples
#' \donttest{
#' library(nprcgenekeepr)
#' errorLst <- qcStudbook(nprcgenekeepr::pedFemaleSireMaleDam,
#'                        reportErrors = TRUE)
#' nprcgenekeepr:::checkChangedColAndErrorLst(errorLst)
#' errorLst <- qcStudbook(nprcgenekeepr::pedGood, reportErrors = TRUE)
#' nprcgenekeepr:::checkChangedColAndErrorLst(errorLst)
#' errorLst <- qcStudbook(nprcgenekeepr::pedOne, reportErrors = TRUE)
#' nprcgenekeepr:::checkChangedColAndErrorLst(errorLst)[
#'   c("suspiciousParents", "femaleSires", "maleDams")]
#' }
#'
#' @param errorLst list with fields for each type of changed column and
#' error detectable by \code{qcStudbook}.
checkChangedColAndErrorLst <- function(errorLst) {
  if (checkErrorLst(errorLst) |
      checkChangedColsLst(errorLst$changedCols))
    return(errorLst)
  else
    return(NULL)
}
