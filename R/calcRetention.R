#' Calculates Allelic Retention
#'
## Copyright(c) 2017-2019 R. Mark Sharp
## This file is part of nprcmanager
#' Part of Genetic Value Analysis
#'
#' @param ped the pedigree information in datatable format.  Pedigree
#' (req. fields: id, sire, dam, gen, population).
#'
#' It is assumed that the pedigree has no partial parentage
#' @param alleles dataframe of containing an \code{AlleleTable}. This is a
#' table of allele information produced by \code{geneDrop()}.
#' @export
calcRetention <- function(ped, alleles) {
  # ASSUME: Pedigree has no partial parentage
  founders <- ped$id[is.na(ped$sire) & is.na(ped$dam)]
  descendants <- ped$id[ped$population & !(ped$id %in% founders)]

  founders <- alleles[(alleles$id %in% founders), c("id", "V1")]
  colnames(founders) <- c("id", "allele")

  alleles <- alleles[(alleles$id %in% descendants),
                     !(colnames(alleles) %in% c("id", "parent"))]

  retained <- apply(alleles, 2, function(a) {
    founders$allele %in% a
  })
  retained <- rowSums(retained, na.rm = TRUE) / ncol(retained)
  founders <- cbind(founders, retained)

  founders <- tapply(founders$retained, founders$id, mean)
  return(founders)
}
