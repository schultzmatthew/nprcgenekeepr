#' Get pedigree from file
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
getPedigree <- function(fileName, sep = ",") {
  flog.debug(paste0("in getPedigree\n"),
             name = "nprcmanager")
  if (excel_format(fileName) %in% c("xls", "xlsx")) {
    pedigree <- readExcelPOSIXToCharacter(fileName)
    flog.debug(paste0("in getPedigree after readxl, nrow(pedigree) = ",
                      nrow(pedigree), "\n"), name = "nprcmanager")
  } else {
    pedigree <- read.table(fileName,
                         header = TRUE,
                         sep = sep,
                         stringsAsFactors = FALSE,
                         na.strings = c("", "NA"),
                         check.names = FALSE)
    flog.debug(paste0("in getPedigree after read.csv, nrow(pedigree) = ",
                      nrow(pedigree), "\n"), name = "nprcmanager")
  }
  pedigree
}
