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
#' @param errors logical value if TRUE will scan the entire file and
#' make a list of all errors found. The errors will be returned in a
#' list of list where each sublist is a type of error found.
#' @export
removeDuplicates <- function(ped, errors = FALSE) {
  if (errors) {
    if (sum(duplicated(ped$id)) == 0) {
      return(NULL)
    } else {
      return(ped$id[duplicated(ped$id)])
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
