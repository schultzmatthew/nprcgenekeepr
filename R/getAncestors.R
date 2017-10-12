#' Recursively create a character vector of ancestors for an individual ID.
#'
#' Part of Pedigree Sampling
#' From PedigreeSampling.R
#' 2016-01-28
#'
#' Contains functions to build pedigrees from sub-samples
#' of genotyped individuals.
#'
#' The goal of sampling is to reduce the number of inbreeding
#' loops in the resulting pedigree, and thus, reduce the
#' amount of time required to perform calculations with
#' SIMWALK2 or similar programs.
#'
#'
#' @param id character vector of length 1 having the ID of interest
#' @param ptree a list of lists forming a pedigree tree as constructed by
#' \code{createPedTree(ped)} where \code{ped} is a standard pedigree dataframe.
#' @return A character vector of ancestors for an individual ID.
#' @export
getAncestors <- function(id, ptree) {

  if (is.na(id)) {
    return(c())
  }

  sire <- ptree[[id]]$sire
  dam <- ptree[[id]]$dam

  if (!is.na(sire)) {
    sAnc <- getAncestors(sire, ptree)
    sireLineage <- c(sire, sAnc)
  } else{
    sireLineage <- c()
  }

  if (!is.na(dam)) {
    dAnc <- getAncestors(dam, ptree)
    damLineage <- c(dam, dAnc)
  } else{
    damLineage <- c()
  }

  return(c(sireLineage, damLineage))
}
