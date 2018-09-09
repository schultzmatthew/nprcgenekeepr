#' fixColumnNames changes original column names and into standardized names.
#'
#' @return list object with \code{newColNames} and \code{errorLst} with
#' a record of all changes made.
#' @param orgCols character vector with ordered list of column names
#' found in a pedigree file.
#' @param errorLst list object with places to store the various column
#' name changes.
#' @export
fixColumnNames <- function(orgCols, errorLst) {
  cols <- tolower(orgCols)
  errorLst$changedCols$caseChange <- orgCols[!orgCols %in% cols]
  newCols <- gsub(" ", "", cols)
  errorLst$changedCols$spaceRemoved <- cols[!cols %in% newCols]
  cols <- newCols
  newCols <- gsub("\\.", "", cols)
  errorLst$changedCols$periodRemoved <- cols[!cols %in% newCols]
  cols <- newCols
  newCols <- gsub("_", "", cols)
  errorLst$changedCols$underScoreRemoved <- cols[!cols %in% newCols]
  cols <- newCols
  newCols <- gsub("egoid", "id", cols)
  errorLst$changedCols$egoidToId <- cols[!cols %in% newCols]
  cols <- newCols
  newCols <- gsub("sireid", "sire", cols)
  errorLst$changedCols$sireIdToSire <- cols[!cols %in% newCols]
  cols <- newCols
  newCols <- gsub("damid", "dam", cols)
  errorLst$changedCols$damIdToDam <- cols[!cols %in% newCols]
  cols <- newCols
  newCols <- gsub("birthdate", "birth", cols)
  errorLst$changedCols$birthdateToBirth <- cols[!cols %in% newCols]
  cols <- newCols
  newCols <- gsub("deathdate", "death", cols)
  errorLst$changedCols$deathdateToDeath <- cols[!cols %in% newCols]
  list(newColNames = newCols, errorLst = errorLst)
}
