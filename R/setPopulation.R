#' Population designation function
#'
#' Part of the pedigree filtering toolset.
#' @param ped datatable that is the `Pedigree`. It contains pedigree
#' information. The \code{id} column is required.
#' @param ids character vector of IDs to be flagged as part of the population
#' under consideration.
#'
#' @return An updated pedigree with the \code{population} column added or
#' updated by being set to \code{TRUE} for the animal IDs in \code{ped$id} and
#' \code{FALSE} otherwise.
#' @export
setPopulation <- function(ped, ids) {
  ped$population <- FALSE

  if (length(ids) == 0) {
    ped$population <- TRUE
  } else {
    ped$population[ped$id %in% ids] <- TRUE
  }
  return(ped)
}
