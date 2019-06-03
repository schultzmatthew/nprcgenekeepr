#' Write copy of ExamplePedigree.csv file
#'
#' Uses \code{examplePedigree} data structure to create an example data file
#' @importFrom utils data
#' @export
makeExamplePedigreeFile <- function() {

  write.csv(data("examplePedigree", envir = environment()),
            file = file.choose(new = TRUE), row.names = FALSE)
}
