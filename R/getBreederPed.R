#' Get pedigree based on list of breeders
#'
#' @return A pedigree file compatible with pedigree other pedigree files in
#' this package.
#'
#' @param fileName character vector of temporary file path.
#' @import tacr
#' @export
getBreederPed <- function(fileName, sep = ",") {
  breeders <- read.csv(fileName,
                       header = TRUE,
                       sep = sep,
                       stringsAsFactors = FALSE,
                       na.strings = c("", "NA"),
                       check.names = FALSE)
  breeders <- as.character(breeders$id)
  baseUrl <- getSiteInfo()$baseUrl
  labkeyNode <- stri_split_fixed(baseUrl, "/")[[1]][[3]]
  colSelect <- c("Id", "sire", "dam", "gender", "lastDayAtCenter", "birth",
                 "death")
  ped <- getLkDirectAncestors(labkeyNode, ids = breeders,
                                    colSelect = colSelect)
  names(ped) <- c("id", "sire", "dam", "sex", "departure", "birth", "death")
  ped[!is.na(ped$id), ]
}
