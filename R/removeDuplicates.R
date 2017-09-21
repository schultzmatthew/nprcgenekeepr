#' Remove duplicate records from pedigree
#'
#' Part of Pedigree Curation
#'
#' Returns an updated dataframe with duplicate rows removed.
#'
#' Returns an error if the table has duplicate IDs with differing data.
#'
#' @param ped dataframe that is the `Pedigree`. It contains pedigree
#' information. The \code{id} column is required.
#' @export
removeDuplicates <- function(ped) {
  p <- unique(ped)

  if (sum(duplicated(p$id)) == 0) {
    return(p)
  }
  else{
    stop("Duplicate IDs with mismatched information present")
  }
}
