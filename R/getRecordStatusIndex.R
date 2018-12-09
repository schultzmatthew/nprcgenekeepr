#' getRecordStatusIndex returns record numbers with selected \code{recordStatus}.
#'
#' @return integer vector of records with \code{recordStatus} == \code{status}.
#'
#' @param ped pedigree dataframe
#' @param status character vector with value of \code{"added"} or \code{"original"}.
getRecordStatusIndex <- function(ped, status = "added") {
  if (any("recordStatus" %in% names(ped)))
    seq_along(ped$recordStatus)[ped$recordStatus == status]
  else
    integer(0)
}
