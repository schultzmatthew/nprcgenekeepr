#' Get pedigree based on list of breeders
#'
## Copyright(c) 2017-2019 R. Mark Sharp
## This file is part of nprcmanager
#' @return A pedigree file compatible with others in this package.
#'
#' @param fileName character vector of temporary file path.
#' @param sep column separator in CSV file
#' @import futile.logger
#' @importFrom readxl excel_format
#' @importFrom utils read.table
#' @export
getBreederPed <- function(fileName, sep = ",") {
  flog.debug(paste0("in getBreederPed\n"),
             name = "nprcmanager")
  if (excel_format(fileName) %in% c("xls", "xlsx")) {
    breeders <- readExcelPOSIXToCharacter(fileName)
    flog.debug(paste0("in getBreederPed after readxl, nrow(breeders) = ",
                      nrow(breeders), "\n"), name = "nprcmanager")
  } else {
    breeders <- read.csv(fileName,
                         header = TRUE,
                         sep = sep,
                         stringsAsFactors = FALSE,
                         na.strings = c("", "NA"),
                         check.names = FALSE)
    flog.debug(paste0("in getBreederPed after read.csv, nrow(breeders) = ",
                      nrow(breeders), "\n"), name = "nprcmanager")
  }
  breeders <- as.character(breeders[ , 1])
  ped <- getLkDirectRelatives(ids = breeders)
  if (is.null(ped)) {
    flog.debug(paste0("in getBreederPed after getLkDirectRelatives, which ",
                      "returned NULL.\n"), name = "nprcmanager")
    errorLst <- getEmptyErrorLst()
    errorLst$failedDatabaseConnection <-
      "Database connection failed: configuration or permissions are invalid."
    return(errorLst)
  }
  flog.debug(paste0("in getBreederPed after getLkDirectRelatives, which ",
                    "returned ped with ", nrow(ped), "rows.\n"),
             name = "nprcmanager")
  names(ped) <- c("id", "sex", "birth", "death", "departure", "dam", "sire")
  ped <- ped[!is.na(ped$id), ]
  ped$birth <- format(ped$birth, format = "%Y-%m-%d")
  ped$death <- format(ped$death, format = "%Y-%m-%d")
  ped$departure <- format(ped$departure, format = "%Y-%m-%d")
  ped
}
