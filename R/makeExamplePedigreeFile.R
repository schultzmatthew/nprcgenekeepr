#' Write copy of nprcmanager::examplePedigree into a file
#'
#' Uses \code{examplePedigree} data structure to create an example data file
#' @return full path name of file saved.
#' @param fileType character vector of length one with possible values of
#' \code{"txt"}, \code{"csv"}, or \code{"xlsx"}. Default value is \code{"csv"}.
#'
#' @importFrom utils write.table
## ## import rmsutilityr
#' @export
makeExamplePedigreeFile <- function(fileType = "csv") {
  filename <- file.choose(new = TRUE)
  stopifnot(any(fileType %in% c("txt", "csv", "excel")))
  if (fileType == "csv") {
    write.csv(nprcmanager::examplePedigree,
              file = filename, row.names = FALSE)
  } else if (fileType == "excel") {
    status <-
      create_wkbk(file = filename,
                               df_list = list(nprcmanager::examplePedigree),
                               sheetnames = "Example_Pedigree", create = TRUE)
    if (!status)
      stop(paste0("Failed to write example data out to ", filename, "."))
  } else {
    utils::write.tqble(nprcmanager::examplePedigree,
              file = filename, row.names = FALSE, sep = "\t")
  }
  filename
}
