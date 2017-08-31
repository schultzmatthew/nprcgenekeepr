#' Add back single parents
#'
#' Used \code{ped} dataframe, which has full complement of parents and the
#' \code{uPed) } dataframe, which has all uninformative parents removed to
#' add back single parents to the \code{uPed} dataframe where one parent is
#' known.
#' @param uPed a trimmed pedigree dataframe with uninformative founders removed.
#' @param ped a trimmed pedigree
#' @export
addBackSingleParents <- function(uPed, ped) {

  # Adding back second parents where one is known
  single_parents <- p$id[(is.na(p$sire) & !is.na(p$dam)) |
                           (!is.na(p$sire) & is.na(p$dam))]

  add_back <- c()
  for (id in single_parents) {
    if (!is.na(ped$sire[ped$id == id]) & !is.na(ped$dam[ped$id == id])) {

      if (is.na(p$sire[p$id == id])) {
        add_back <- c(add_back, ped$sire[ped$id == id])
        p[(p$id == id), "sire"] <- ped$sire[ped$id == id]
      } else{
        add_back <- c(add_back, ped$dam[ped$id == id])
        p[(p$id == id), "dam"] <- ped$dam[ped$id == id]
      }
    }
  }
  add_back <- ped[(ped$id %in% add_back), ]
  add_back$sire <- NA
  add_back$dam <- NA

  p <- rbind(p, add_back)
  return(p)
}
