#' Add back single parents
#'
#' Uses the \code{ped} dataframe, which has full complement of parents and the
#' \code{uPed} dataframe, which has all uninformative parents removed to
#' add back single parents to the \code{uPed} dataframe where one parent is
#' known. The parents are added back to the pedigree as an ID record with
#' NA for both sire and dam of the added back ID.
#' @param uPed a trimmed pedigree dataframe with uninformative founders removed.
#' @param ped a trimmed pedigree
#' @export
addBackSecondParents <- function(uPed, ped) {

  # Adding back second parents where one is known
  idsWithSingleParents <- getIdsWithSingleParents(uPed)
  addBack <- c()
  for (id in idsWithSingleParents) {
    if (hasBothParents(id, ped)) {
      if (is.na(uPed$sire[uPed$id == id])) {
        addBack <- c(addBack, ped$sire[ped$id == id])
        uPed[(uPed$id == id), "sire"] <- ped$sire[ped$id == id]
      } else{
        addBack <- c(addBack, ped$dam[ped$id == id])
        uPed[(uPed$id == id), "dam"] <- ped$dam[ped$id == id]
      }
    }
  }
  if (length(addBack) > 0) {
    addBack <- ped[(ped$id %in% addBack), ]
    addBack$sire <- NA
    addBack$dam <- NA

    uPed <- rbind(uPed, addBack)
  }
  return(uPed)
}
