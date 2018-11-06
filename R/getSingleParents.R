#' getIdsWithSingleParents
#'
#' @return character vector of all IDs having a single parent in uPed
#'
#' @param uPed a trimmed pedigree dataframe with uninformative founders removed.
#' Thus, having a single parent in uPed does not mean the other parent is not
#' known. It only means the other parent is not informative. That is it has
#' no other relatives in the pedigree.
#' @export
getIdsWithSingleParents <- function(uPed) {
  uPed$id[(is.na(uPed$sire) & !is.na(uPed$dam)) |
            (!is.na(uPed$sire) & is.na(uPed$dam))]
}
