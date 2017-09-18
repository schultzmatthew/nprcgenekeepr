#' Finds the total number of offspring for each animal in the pedigree
#'
#' Optionally find the number that are part of the population of interest.
#'
#' @param probands character vector of egos for which offspring should be
#' counted.
#' @param ped the pedigree information in datatable format.  Pedigree
#' (req. fields: id, sire, dam, gen, population).
#' This is the complete pedigree.
#' @param consider.pop logical value indication whether or not the number of
#' offspring that are part of the focal population are to be counted?
#' Default is \code{FALSE}.
#'
#' @return A dataframe with at least \code{id} and \code{totalOffspring}
#' required and \code{living.offspring} optional.
#'
#' @export
offspringCounts <- function(probands, ped, consider.pop = FALSE) {
  totalOffspring <- findOffspring(probands, ped)
  results <- as.data.frame(totalOffspring)

  if (consider.pop && !is.null(ped$population)) {
    pop <- ped[ped$population, ]
    living.offspring <- findOffspring(probands, pop)
    results <- cbind(results, living.offspring)
  }
  return(results)
}
