#' Finds the total number of offspring for each animal in the pedigree
#'
## Copyright(c) 2017-2019 R. Mark Sharp
## This file is part of nprcmanager
#' Optionally find the number that are part of the population of interest.
#'
#' @param probands character vector of egos for which offspring should be
#' counted.
#' @param ped the pedigree information in datatable format.  Pedigree
#' (req. fields: id, sire, dam, gen, population).
#' This is the complete pedigree.
#' @param considerPop logical value indication whether or not the number of
#' offspring that are part of the focal population are to be counted?
#' Default is \code{FALSE}.
#'
#' @return A dataframe with at least \code{id} and \code{totalOffspring}
#' required and \code{livingOffspring} optional.
#'
#' @export
offspringCounts <- function(probands, ped, considerPop = FALSE) {
  totalOffspring <- findOffspring(probands, ped)
  results <- as.data.frame(totalOffspring)

  if (considerPop && !is.null(ped$population)) {
    pop <- ped[ped$population, ]
    livingOffspring <- findOffspring(probands, pop)
    results <- cbind(results, livingOffspring)
  }
  return(results)
}
