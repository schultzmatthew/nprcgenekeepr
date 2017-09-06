#' Add genotype data to pedigree file
#'
#' @param ped pedigree dataframe. \code{ped} is to be provided by
#' \code{qc.Studbook} so it is not checked.
#' @param genotype genotype dataframe. \code{genotype} is to be provided by
#' \code{checkGenotypeFile} so it is not checked.
#' @export
addGenotype <- function(ped, genotype) {
  genotypeNames <- names(genotype)[2:3]
  geno <- sort(unique(c(genotype[ , genotypeNames[1]],
                        genotype[ , genotypeNames[2]])))
  genoDict <- seq_along(geno) + 10000
  names(genoDict) <- geno
  first <- as.character(genoDict[genotype[ , 2]])
  second <- as.character(genoDict[genotype[ , 3]])
  genotype <- cbind(genotype,
                    first = as.character(genoDict[genotype[ , 2]]),
                    second = as.character(genoDict[genotype[ , 3]]))
  newPed <- merge(ped, genotype, by = "id", all = TRUE)
  newPed
}
