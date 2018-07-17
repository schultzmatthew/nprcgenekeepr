#' Removing IDs having "UNKNOWN" regardless of case
#'
#' Someone started entering "unknown" for unknown parents instead of leaving
#' the field blank in PRIMe.
#' @param ped  A dataframe containing at least and "id" field
#' @export
unknown2NA <- function(ped) {
  ped <- ped[toupper(ped$id) != "UNKNOWN", ]
  ped$sire[toupper(ped$sire) == "UNKNOWN"] <- NA
  ped$dam[toupper(ped$dam) == "UNKNOWN"] <- NA
  ped
}
