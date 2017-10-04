#' Get pedigree based on list of breeders
#'
#' @return A pedigree file compatible with others in this package.
#'
#' @param fileName character vector of temporary file path.
#' @param sep column separator in CSV file
#' @export
getBreederPed <- function(fileName, sep = ",") {
  flog.debug(paste0("in getBreederPed\n"),
             name = "nprcmanager")

  breeders <- read.csv(fileName,
                       header = TRUE,
                       sep = sep,
                       stringsAsFactors = FALSE,
                       na.strings = c("", "NA"),
                       check.names = FALSE)
  flog.debug(paste0("in getBreederPed after read.csv, nrow(breeders) = ",
             nrow(breeders), "\n"), name = "nprcmanager")
  breeders <- as.character(breeders[ , 1])
  ped <- getLkDirectRelatives(ids = breeders)
  names(ped) <- c("id", "sex", "birth", "death", "departure", "dam", "sire")
  ped <- ped[!is.na(ped$id), ]
  ped$birth <- format(ped$birth, format = "%Y-%m-%d")
  ped$death <- format(ped$death, format = "%Y-%m-%d")
  ped$departure <- format(ped$departure, format = "%Y-%m-%d")
  ped
}
