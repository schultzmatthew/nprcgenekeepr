#' Add parents
#'
#' Pedigree curation function
#' Given a pedigree, find any IDs listed in the "sire" or "dam" columns
#' that lack their own line entry and generate one.
#'
#' This must be run after to \code{addUIds} since the IDs made there are
#' used by \code{addParents}

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

  # Add record_status to identify original records
  # if (length(ped) > 4) {
  #   ped <- cbind(ped[ , c("id", "sire", "dam", "sex")],
  #                record_status = "original",
  #                ped[ , names(ped)[5:length(ped)], drop = FALSE],
  #                stringsAsFactors = FALSE)
  # } else {
    ped <- cbind(ped, record_status = "original", stringsAsFactors = FALSE)
  #}
  # Adding line entries for these parents
  if (nrow(a1) > 0) {
    a1$sire <- NA
    a1$dam <- NA
    a1$sex <- "M"
    a1$record_status <- "added"
    ped <- rbindFill(ped, a1)
  }

  if (nrow(a2) > 0) {
    a2$sire <- NA
    a2$dam <- NA
    a2$sex <- "F"
    a2$record_status <- "added"
    ped <- rbindFill(ped, a2)
  }
  return(ped)
}
