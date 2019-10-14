#' Provides list of potential sires
#'
## Copyright(c) 2017-2019 R. Mark Sharp
## This file is part of nprcmanager
#' @return character vector of potential sire Ids
#'
#' @param ids character vector of IDs of the animals
#' @param minAge integer value indicating the minimum age to consider in group
#' formation. Pairwise kinships involving an animal of this age or younger will
#'  be ignored. Default is 1 year.
#' @param ped dataframe that is the `Pedigree`. It contains pedigree
#' information including the IDs listed in \code{candidates}.
#' @export
getPotentialSires <- function(ids, minAge = 1, ped) {
  ped <- ped[!is.na(ped$birth), ]
  ped$id[ped$id %in% ids & ped$sex == "M" & getCurrentAge(ped$birth) >= minAge &
           !is.na(ped$birth)]
}
