#' Converts sex indicator for an individual to a standardized codes.
#'
#' Part of Pedigree Curation
#'
#'
#' Standard sex codes are
#' \itemize{
#' \item{F} {-- replacing "FEMALE" or "2"}
#' \item{M} {-- replacing "MALE" or "1"}
#' \item{H} {-- replacing "HERMAPHRODITE" or "4", if igore.herm == FALSE}
#' \item{U} {-- replacing "HERMAPHRODITE" or "4", if igore.herm == TRUE}
#' \item{U} {-- replacing "UNKNOWN" or "3"}}
#'
#' @param sex factor indicating sex of a set of individuals
#' @param ignore.herm logical flag indicating if hermaphrodites should be
#' treated as unknown sex ("U"), default is \code{TRUE}.
#' @export
convertSexCodes <- function(sex, ignore.herm = TRUE) {
  sex <- toupper(sex)
  sex[is.na(sex)] <- "U"

  sex[sex %in% c("MALE", "M", "1")] <- "M"
  sex[sex %in% c("FEMALE", "F", "2")] <- "F"
  sex[sex %in% c("UNKNOWN", "U", "3")] <- "U"

  if (ignore.herm) {
    sex[sex %in% c("HERMAPHRODITE", "H", "4")] <- "U"
  } else {
    sex[sex %in% c("HERMAPHRODITE", "H", "4")] <- "H"
  }
  sex <- factor(sex, levels = c("F", "M", "H", "U"))
  return(sex)
}
