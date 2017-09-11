#' Get Genetic Valuie Genotype data structure for reportGV function.
#'
#' Extracts genotype data if available otherwise NULL is returned.
#' @param ped the pedigree information in datatable format
#' @export
getGVGenotype <- function(ped) {
  if (hasGenotype(ped)) {
    genotype <- ped[ , c("id", "first", "second")]
  } else {
    genotype <- NULL
  }
  genotype
}
