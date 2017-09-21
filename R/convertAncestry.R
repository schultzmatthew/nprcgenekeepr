#' Converts the ancestry information to a standardized code
#'
#' Part of Pedigree Curation
#'
#'
#' @param ancestry character vector or NA with free-form text providing
#' information about the geographic population of origin.
#'
#' @return factor vector of standardized designators specifying if an animal is
#' a Chinese rhesus, Indian rhesus, Chinese-Indian hybrid rhesus, or
#' Japanese macaque. Levels: CHINESE, INDIAN, HYBRID, JAPANESE, OTHER, UNKNOWN.
#' @export
convertAncestry <- function(ancestry) {
  ancestry <- tolower(ancestry)

  # Find entries containing non-standardized indications of population
  chinese <- grepl("chin", ancestry) & !grepl("ind", ancestry)
  indian <- !grepl("chin", ancestry) & grepl("ind", ancestry)
  hybrid <- ((grepl("chin", ancestry) & grepl("ind", ancestry)) |
               grepl("hyb", ancestry))
  japanese <- grepl("jap", ancestry)
  unknown <- is.na(ancestry)

  other <- !(chinese | indian | hybrid | japanese) & !unknown

  ancestry[chinese] <- "CHINESE"
  ancestry[indian] <- "INDIAN"
  ancestry[hybrid] <- "HYBRID"
  ancestry[japanese] <- "JAPANESE"
  ancestry[unknown] <- "UNKNOWN"
  ancestry[other] <- "OTHER"

  ancestry <- factor(ancestry, levels = c("INDIAN", "CHINESE", "HYBRID",
                                          "JAPANESE", "OTHER", "UNKNOWN"))
  return(ancestry)
}
