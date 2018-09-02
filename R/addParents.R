#' Add parents
#'
#' Pedigree curation function
#' Given a pedigree, find any IDs listed in the "sire" or "dam" columns
#' that lack their own line entry and generate one.
#'
#' @param ped datatable that is the `Pedigree`. It contains pedigree
#' information including the IDs listed in \code{candidates}.
#'
#' @return An updated pedigree with entries added as necessary.
#' Entries have the id and sex specified; all remaining columns are filled
#' with \code{NA}.
#' @export
addParents <- function(ped) {
  sires <- ped$sire
  dams <- ped$dam

  # Finding sires and dams not in the id column
  a1 <- sires[!(sires %in% ped$id) & !is.na(sires)]
  a1 <- a1[!duplicated(a1)]
  a2 <- dams[!(dams %in% ped$id) & !is.na(dams)]
  a2 <- a2[!duplicated(a2)]

  a1 <- data.frame(id = a1, stringsAsFactors = FALSE)
  a2 <- data.frame(id = a2, stringsAsFactors = FALSE)

  # Adding line entries for these parents
  if (nrow(a1) > 0) {
    a1$sire <- NA
    a1$dam <- NA
    a1$sex <- "M"
    ped <- rbindFill(ped, a1)
  }

  if (nrow(a2) > 0) {
    a2$sire <- NA
    a2$dam <- NA
    a2$sex <- "F"
    ped <- rbindFill(ped, a2)
  }
  return(ped)
}
