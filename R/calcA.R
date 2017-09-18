#' Calculates \code{a}, the number of an individual's alleles that are rare in
#' each simulation.
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
#' @param by.id logical varioable of length 1 that is passed through to
#' eventually be used by \code{alleleFreq()}, which calculates the count of each
#'  allele in the provided vector. If \code{by.id} is TRUE and ids are provided,
#'  the function will only count the unique alleles for an individual
#'   (homozygous alleles will be counted as 1).
#' @return A matrix with named rows indicating the number of unique alleles
#'   an animal had during each round of simulation (indicated in columns).
#' @export
calcA <- function(alleles, threshold = 1, by.id = FALSE) {
  ids <- alleles$id
  parents <- alleles$parent
  alleles <- alleles[, !(names(alleles) %in% c("id", "parent"))]

  count.rare <- function(a) {
    if (by.id) {
      f <- alleleFreq(a, ids)
    } else {
      f <- alleleFreq(a)
    }
    rare.alleles <- f$allele[f$freq <= threshold]
    a <- (a %in% rare.alleles)
    return(tapply(a, ids, sum))
  }

  return(apply(alleles, 2, count.rare))
}
