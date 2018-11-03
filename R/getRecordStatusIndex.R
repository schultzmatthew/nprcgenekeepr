#' getRecordStatusIndex returns record numbers with selected \code{record_status}.
#'
#' @return integer vector of records with \code{record_status} == \code{status}.
#'
#' @param ped pedigree dataframe
#' @param status character vector with value of \code{added} or \code{original}.
getRecordStatusIndex <- function(ped, status = "added") {
  if (any("record_status" %in% names(ped)))
    seq_along(ped$record_status)[ped$record_status == status]
  else
    integer(0)
}
