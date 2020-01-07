#' getIdsWithOneParent extracts IDs of animals pedigree without either a
#' sire or a dam
## Copyright(c) 2017-2019 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' @return character vector of all single parents
#' @param uPed a trimmed pedigree dataframe with uninformative founders removed.
#' @export
getIdsWithOneParent <- function(uPed) {
uPed$id[(is.na(uPed$sire) & !is.na(uPed$dam)) |
            (!is.na(uPed$sire) & is.na(uPed$dam))]
}
