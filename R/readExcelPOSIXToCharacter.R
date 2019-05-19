#' Read in Excel file and convert POSIX dates to character
#'
#' @return A pedigree file compatible with others in this package.
#'
#' @param fileName character vector of temporary file path.
#' @importFrom stringi stri_detect_fixed
#' @importFrom readxl read_excel
#' @export
readExcelPOSIXToCharacter <- function(fileName) {
  pedigree <- as.data.frame(read_excel(path = fileName))
  cols <- sapply(pedigree, class)
  cols <- suppressWarnings(names(cols)[stri_detect_fixed(cols, "POSIX")])
  pedigree <- toCharacter(pedigree, headers = cols)
  pedigree
}
