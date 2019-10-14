#' Removing IDs having "UNKNOWN" regardless of case
#'
## Copyright(c) 2017-2019 R. Mark Sharp
## This file is part of nprcmanager
#' Someone started entering "unknown" for unknown parents instead of leaving
#' the field blank in PRIMe.
#' @param ped  A dataframe containing at least and "id" field
#' @export
unknown2NA <- function(ped) {
  if ("id" %in% names(ped))
    ped <- ped[toupper(ped$id) != "UNKNOWN", ]
  if ("sire" %in% names(ped))
    ped$sire[toupper(ped$sire) == "UNKNOWN"] <- NA
  if ("dam" %in% names(ped))
    ped$dam[toupper(ped$dam) == "UNKNOWN"] <- NA
  ped
}
