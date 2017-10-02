#' Add back single parents
#'
#' Uses the \code{ped} dataframe, which has full complement of parents and the
#' \code{uPed} dataframe, which has all uninformative parents removed to
#' add back single parents to the \code{uPed} dataframe where one parent is
#' known.
#' @param uPed a trimmed pedigree dataframe with uninformative founders removed.
#' @param ped a trimmed pedigree
#' @export
addBackSecondParents <- function(uPed, ped) {

  # Adding back second parents where one is known
  single_parents <- uPed$id[(is.na(uPed$sire) & !is.na(uPed$dam)) |
                           (!is.na(uPed$sire) & is.na(uPed$dam))]

  add_back <- c()
  for (id in single_parents) {
    if (!is.na(ped$sire[ped$id == id]) & !is.na(ped$dam[ped$id == id])) {

      if (is.na(uPed$sire[uPed$id == id])) {
        add_back <- c(add_back, ped$sire[ped$id == id])
        uPed[(uPed$id == id), "sire"] <- ped$sire[ped$id == id]
      } else{
        add_back <- c(add_back, ped$dam[ped$id == id])
        uPed[(uPed$id == id), "dam"] <- ped$dam[ped$id == id]
      }
    }
  }
  if (length(add_back) > 0) {
    add_back <- ped[(ped$id %in% add_back), ]
    add_back$sire <- NA
    add_back$dam <- NA

    uPed <- rbind(uPed, add_back)
  }
  return(uPed)
}
