#' Write copy of ExamplePedigree.csv file
#'
#' Uses \code{examplePedigree} data structure to create an example data file
#' @return full path name of file saved.
#' @importFrom utils data
#' @export
makeExamplePedigreeFile <- function() {
  filename <- file.choose(new = TRUE)
  write.csv(nprcmanager::examplePedigree,
            file = filename, row.names = FALSE)
  filename
}
