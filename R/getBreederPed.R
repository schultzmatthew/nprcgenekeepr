#' Get pedigree based on list of focal animals
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
getFocalAnimalPed <- function(fileName, sep = ",") {
  flog.debug(paste0("in getFocalAnimalPed\n"),
             name = "nprcmanager")
  if (excel_format(fileName) %in% c("xls", "xlsx")) {
    focalAnimals <- readExcelPOSIXToCharacter(fileName)
    flog.debug(paste0("in getFocalAnimalPed after readxl, nrow(focalAnimals) = ",
                      nrow(focalAnimals), "\n"), name = "nprcmanager")
  } else {
    focalAnimals <- read.csv(fileName,
                         header = TRUE,
                         sep = sep,
                         stringsAsFactors = FALSE,
                         na.strings = c("", "NA"),
                         check.names = FALSE)
    flog.debug(paste0("in getFocalAnimalPed after read.csv, nrow(focalAnimals) = ",
                      nrow(focalAnimals), "\n"), name = "nprcmanager")
  }
  focalAnimals <- as.character(focalAnimals[ , 1])
  ped <- getLkDirectRelatives(ids = focalAnimals)
  if (is.null(ped)) {
    flog.debug(paste0("in getFocalAnimalPed after getLkDirectRelatives, which ",
                      "returned NULL.\n"), name = "nprcmanager")
    errorLst <- getEmptyErrorLst()
    errorLst$failedDatabaseConnection <-
      "Database connection failed: configuration or permissions are invalid."
    return(errorLst)
  }
  flog.debug(paste0("in getFocalAnimalPed after getLkDirectRelatives, which ",
                    "returned ped with ", nrow(ped), "rows.\n"),
             name = "nprcmanager")
  names(ped) <- c("id", "sex", "birth", "death", "departure", "dam", "sire")
  ped <- ped[!is.na(ped$id), ]
  ped$birth <- format(ped$birth, format = "%Y-%m-%d")
  ped$death <- format(ped$death, format = "%Y-%m-%d")
  ped$departure <- format(ped$departure, format = "%Y-%m-%d")
  ped
}
