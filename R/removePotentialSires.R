#' Removes potential sires from list of Ids
#'
#' @return character vector of Ids with any potential sire Ids removed.
#'
#' @param ids character vector of IDs of the animals
#' @param minAge integer value indicating the minimum age to consider in group
#' formation. Pairwise kinships involving an animal of this age or younger will
#'  be ignored. Default is 1 year.
#' @param ped dataframe that is the `Pedigree`. It contains pedigree
#' information including the IDs listed in \code{candidates}.
#' @export
removePotentialSires <- function(ids, minAge, ped) {
  setdiff(ids, potentialSires(ids, minAge, ped))
}
