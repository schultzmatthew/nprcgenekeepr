#' Remove duplicate records from pedigree
#'
## Copyright(c) 2017-2019 R. Mark Sharp
## This file is part of nprcgenekeepr
#' Part of Pedigree Curation
#'
#' Returns an updated dataframe with duplicate rows removed.
#'
#' Returns an error if the table has duplicate IDs with differing data.
#'
#' @param ped dataframe that is the `Pedigree`. It contains pedigree
#' information. The \code{id} column is required.
#' @param reportErrors logical value if TRUE will scan the entire file and
#' make a list of all errors found. The errors will be returned in a
#' list of list where each sublist is a type of error found.
#' @export
removeDuplicates <- function(ped, reportErrors = FALSE) {
  if (!all(c("id", "recordStatus") %in% names(ped)))
    stop("ped must have columns \"id\" and \"recordStatus\".")
  if (reportErrors) {
    if (sum(duplicated(ped$id[ped$recordStatus == "original"])) == 0) {
      return(NULL)
    } else {
      return(ped$id[duplicated(ped$id[ped$recordStatus == "original"])])
    }
  } else {
    p <- unique(ped)
    if (sum(duplicated(p$id)) == 0) {
      return(p)
    } else{
      stop("Duplicate IDs with mismatched information present")
    }
  }
}
