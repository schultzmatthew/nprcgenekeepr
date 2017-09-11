#' Get the population of interest for the Genetic Value analysis.
#'
#' If user has limited the population of iterest by defining \code{pop},
#' that information is incorporated via the \code{ped$population} column.
#'
#' @param ped the pedigree information in datatable format
#' @param pop character vector with animal IDs to consider as the population of
#' interest. The default is NULL.
#' @export
getGVPopulation <- function(ped, pop) {
  if (!is.null(pop)) {
    ped$population <- FALSE
    ped$population[ped$id %in% pop] <- TRUE
  } else if (is.null(ped$population)) {
    ped$population <- TRUE
  }
  ped$population
}
