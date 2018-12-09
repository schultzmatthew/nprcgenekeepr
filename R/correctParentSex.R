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
#' @param reportErrors logical value if TRUE will scan the entire file and
#' make a list of all errors found. The errors will be returned in a
#' list of list where each sublist is a type of error found.
#' @param recordStatus character vector with value of \code{"added"} or
#' \code{"original"}, which indicates whether an animal was added or an
#' original animal.
#' @return A factor with levels: "M", "F", "H", and "U"
#' representing the sex codes for the ids provided
#' @export
correctParentSex <- function(id, sire, dam, sex, recordStatus,
                             reportErrors = FALSE) {
  # Get all sires and dams
  sires <- unique(sire)
  sires <- sires[!is.na(sires)]
  dams <- unique(dam)
  dams <- dams[!is.na(dams)]

  # Check if any ids are listed in both the sire and dam columns (error)
  sireAndDam <- intersect(sires, dams)
  if (length(sireAndDam > 0)) {
    if (!reportErrors) {
      stop(sireAndDam, " : Subject(s) listed as both sire and dam")
    }
  }
  if (reportErrors) {
    femaleSires <- id[(id %in% sires) & (!sex %in% c("H", "U", "M")) &
                        recordStatus == "original"]
    maleDams <- id[(id %in% dams) & (!sex %in% c("H", "U", "F")) &
                     recordStatus == "original"]
    if (length(femaleSires) == 0)
      femaleSires <- NULL
    if (length(maleDams) == 0)
      maleDams <- NULL
    if (length(sireAndDam) == 0)
      sireAndDam <- NULL
    list(sireAndDam = sireAndDam, femaleSires = femaleSires,
         maleDams = maleDams)
  } else {
    # Update gender for sires and dams
    sex[((id %in% sires) & (sex != "M"))] <- "M"
    sex[((id %in% dams) & (sex != "F"))] <- "F"
    return(sex)
  }
}
