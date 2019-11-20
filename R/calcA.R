#' Calculates \code{a}, the number of an individual's alleles that are rare in
#' each simulation.
## Copyright(c) 2017-2019 R. Mark Sharp
## This file is part of nprcmanager
#'
#' Part of Genetic Value Analysis
#'
#' @param alleles a matrix with {id, parent, V1 ... Vn} providing the alleles
#' an animal recieved during each simulation.
#' The first 2 columns provide the animal ID and the parent the allele came
#' from. Remaining columns provide alleles.
#' @param threshold an integer indicating the maximum number of copies of an
#'  allele that can be present in the population for it to be considered rare.
#'  Default is 1.
#' @param byID logical varioable of length 1 that is passed through to
#' eventually be used by \code{alleleFreq()}, which calculates the count of each
#'  allele in the provided vector. If \code{byID} is TRUE and ids are provided,
#'  the function will only count the unique alleles for an individual
#'   (homozygous alleles will be counted as 1).
#' @return A matrix with named rows indicating the number of unique alleles
#'   an animal had during each round of simulation (indicated in columns).
#' @export
calcA <- function(alleles, threshold = 1, byID = FALSE) {
  ids <- alleles$id
  alleles <- alleles[, !(names(alleles) %in% c("id", "parent"))]
#'
  countRare <- function(a) {
    if (byID) {
      f <- alleleFreq(a, ids)
    } else {
      f <- alleleFreq(a)
    }
    rare.alleles <- f$allele[f$freq <= threshold]
    a <- (a %in% rare.alleles)
    return(tapply(a, ids, sum))
  }

  return(apply(alleles, 2, countRare))
}
