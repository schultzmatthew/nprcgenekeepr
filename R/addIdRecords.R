#' addIdRecords Adds Ego records added having NAs for parent IDs
#'
#' @return Pedigree with Ego records added having NAs for parent IDs
#' @param ids character vector of IDs to be added as Ego records having
#' NAs for parent IDs
#' @param fullPed a trimmed pedigree
#' @param partialPed a trimmed pedigree dataframe with uninformative founders removed.
#' @export
addIdRecords <- function(ids, fullPed, partialPed) {
  if (length(ids) > 0) {
    addToPed <- fullPed[fullPed$id %in% ids, ]
    addToPed$sire <- NA
    addToPed$dam <- NA
    partialPed <- rbind(partialPed, addToPed,
                        stringsAsFactors = FALSE)
  }
  partialPed[!duplicated(partialPed$id), ]
}
