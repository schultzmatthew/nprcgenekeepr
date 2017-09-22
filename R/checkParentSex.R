#' Sets sex for animals listed as either a sire or dam.
#'
#' Part of Pedigree Curation
#'
#'
#' @param id character vector with unique identifier for an individual
#' @param sire character vector with unique identifier for an
#' individual's father (\code{NA} if unknown).
#' @param dam character vector with unique identifier for an
#' individual's mother (\code{NA} if unknown).
#' @param sex factor with levels: "M", "F", "U". Sex specifier for an
#' individual.
#' @return A factor with levels: "M", "F", "H", and "U"
#' representing the sex codes for the ids provided
#' @export
correctParentSex <- function(id, sire, dam, sex) {
  # Get all sires and dams
  sires <- unique(sire)
  sires <- sires[!is.na(sires)]
  dams <- unique(dam)
  dams <- dams[!is.na(dams)]

  # Check if any ids are listed in both the sire and dam columns (error)
  err <- intersect(sires, dams)
  if (length(err > 0)) {
    stop(err, " : Subject(s) listed as both sire and dam")
  }

  # Update gender for sires and dams
  sex[((id %in% sires) & (sex != "M"))] <- "M"
  sex[((id %in% dams) & (sex != "F"))] <- "F"
  return(sex)
}
