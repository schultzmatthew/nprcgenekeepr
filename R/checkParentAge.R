#' Check parent age
#'
#' Ensure parents are sufficiently older than offspring
#' @param sb A dataframe containing a table of pedigree and demographic
#' information.
#' @param minParentAge numeric values to set the minimum age in years for
#' an animal to have an offspring. Defaults to 2 years. The check is not
#' performed for animals with missing birth dates.
#' @return A dataframe containing rows for each animal where one or more
#' parent was less than \code{minParentAge}. It cantains all of the columns
#' in the original \code{sb} dataframe with the following added columns:
#' \enumerate{
#' \item {sireBirth} {sire's birth date}
#' \item{sireAge} {age of sire in years on the date indicated by \code{birth}.
#' \item{damBirth} for those offspring where one or more parent was less than
#' \code{damAge} {age of dam in years on the date indicated by \code{birth}.}
#' }
#'
#' @export
checkParentAge <- function(sb, minParentAge = 2) {
  sireBirth <- data.frame(
    id = sb$id[sb$id %in% sb$sire & !is.na(sb$birth)],
    sireBirth = sb$birth[sb$id %in% sb$sire & !is.na(sb$birth)],
    stringsAsFactors = FALSE)
  damBirth <- data.frame(
    id = sb$id[sb$id %in% sb$dam & !is.na(sb$birth)],
    damBirth = sb$birth[sb$id %in% sb$dam & !is.na(sb$birth)],
    stringsAsFactors = FALSE)
  sb <- merge(sb, sireBirth, by.x = "sire", by.y = "id", all = TRUE)
  sb <- merge(sb, damBirth, by.x = "dam", by.y = "id", all = TRUE)
  sb$sireAge <- NA
  sb$sireAge[!is.na(sb$sireBirth)] <- (sb$sireBirth[!is.na(sb$sireBirth)] -
                                         sb$birth[!is.na(sb$sireBirth)]) /
    dyears(1)
  sb$damAge[!is.na(sb$damBirth)] <- (sb$damBirth[!is.na(sb$damBirth)] -
                                         sb$birth[!is.na(sb$damBirth)]) /
    dyears(1)
  sb <- sb[!is.na(sb$birth), ]
  sb <- sb[(sb$sireAge < minParentAge & !is.na(sb$sireBirth)) |
              (sb$damAge < minParentAge & !is.na(sb$damBirth)), ]
  sb
}
