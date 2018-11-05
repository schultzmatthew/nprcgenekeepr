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
#' @param errors logical value if TRUE will scan the entire file and
#' make a list of all errors found. The errors will be returned in a
#' list of list where each sublist is a type of error found.
#' @return A factor with levels: "M", "F", "H", and "U"
#' representing the sex codes for the ids provided
#' @export
correctParentSex <- function(id, sire, dam, sex, errors = FALSE) {
  # Get all sires and dams
  sires <- unique(sire)
  sires <- sires[!is.na(sires)]
  dams <- unique(dam)
  dams <- dams[!is.na(dams)]

  # Check if any ids are listed in both the sire and dam columns (error)
  err <- intersect(sires, dams)
  if (length(err > 0)) {
    if (!errors) {
      stop(err, " : Subject(s) listed as both sire and dam")
    }
  }
  if (errors) {
    femaleSires <- id[(id %in% sires) & (!sex %in% c("H", "U", "M"))]
    maleDams <- id[(id %in% dams) & (!sex %in% c("H", "U", "F"))]
    if (length(femaleSires) > 0 & length(maleDams) > 0) {
      list(sireAndDam = err, femaleSires = femaleSires, maleDams = maleDams)
    } else if (length(femaleSires) > 0) {
      list(sireAndDam = err, femaleSires = femaleSires, maleDams = NULL)
    } else if (length(maleDams) > 0) {
      list(sireAndDam = err, femaleSires = NULL, maleDams = maleDams)
    } else {
      list(sireAndDam = err, femaleSires = NULL, maleDams = NULL)
    }
  } else {
    # Update gender for sires and dams
    sex[((id %in% sires) & (sex != "M"))] <- "M"
    sex[((id %in% dams) & (sex != "F"))] <- "F"
    return(sex)
  }
}
