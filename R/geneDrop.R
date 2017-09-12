#' Performs a gene drop simulation based on the provided pedigree information
#'
#' @param id character vector of IDs for a set of animals.
#' @param sire character vector with IDS of the sires for the set of
#'  animals. \code{NA} is used for missing sires.
#' @param dam character vector with IDS of the dams for the set of
#'  animals. \code{NA} is used for missing dams.
#' @param gen integer vector indicating the generation number for each animal.
#' @param genotype is a dataframe containing known genotypes. It has three
#' columns:  \code{id}, \code{first}, and \code{second}. The second and third
#' columns contain the integers indicating the observed genotypes.
#'
#' Currently there is no means of handling knowing only one haplotype.
#' It will be easy to add another column to handle situations where only one
#' allele is observed and it is not known to be homozygous or heterozygous. The
#' new fourth column could have a frequency for homozygosity that could be
#' used in the gene dropping algorithm.
#'
#' The genotypes are using indirection (integer instead of character) to
#' indicate the genes because the minipulation of character strings was found
#' to take 20-35 times longer to perform.
#'
#' Adding additional columns to \code{genotype} does not significantly affect
#' the time require. Thus, it is convenient to add the corresponding haplotype
#' names to the dataframe using \code{first_name} and \code{second_name}.
#' @param n integer indicating the number of iterations to simulate.
#' Default is 5000.
#' @param updateProgress function or NULL. If this function is defined, it
#' will be called during each iteration to update a
#' \code{shiny::Progress} object.
#'
#' @return data.frame \code{id, parent, V1 ... Vn}
#' A data.frame providing the maternal and paternal alleles for an animal
#' for each iteration. The first two columns provide the animal's ID and
#' whether the allele came from the sire or dam. These are followed by
#' \code{n} columns indicating the allele for that iteration.
#'
#' @export
geneDrop <- function(id, sire, dam, gen, genotype = NULL, n = 5000,
                     updateProgress = NULL) {
  ## Sort the IDs by generation so older generations are first
  ped <- data.frame(id, sire, dam, gen, stringsAsFactors = FALSE)
  rownames(ped) <- id
  ped <- ped[order(gen), ]
  if (!is.null(genotype)) {
    genotype <- genotype[!is.na(genotype$first), ]
    genoDefined <- TRUE
  } else {
    genoDefined <- FALSE
  }

  alleles <- list(alleles = list(), counter = 1)


  if (!is.null(updateProgress)) {
    updateProgress(detail = "Performing Gene-drop Simulation", value = 0,
                   reset = TRUE)
  }

  ## Iterate through each ID and get the maternal and paternal alleles
  for (id in ped$id) {
    alleles$alleles[[id]] <- list()
    s <- ped[id, "sire"]
    d <- ped[id, "dam"]
    assigned <- FALSE
    if (genoDefined) {
      if (any(genotype$id == id)) {
        alleles$alleles[[id]][["sire"]] <-
          rep(genotype$first[genotype$id == id], n)
        alleles$alleles[[id]][["dam"]] <-
          rep(genotype$second[genotype$id == id], n)
        assigned <- TRUE
      }
    }
    if (!assigned) {
      alleles <- assignAlleles(alleles, "sire", s, id, n)
      alleles <- assignAlleles(alleles, "dam", d, id, n)
    }

    if (!is.null(updateProgress)) {
      updateProgress(n = nrow(ped))
    }
  }

  # Convert the list of alleles to a data.frame
  alleles <- as.data.frame(t(data.frame(alleles$alleles, check.names = FALSE)))
  keys <- strsplit(rownames(alleles), ".", fixed = TRUE)

  id <- c()
  parent <- c()
  for (i in 1:length(keys)) {
    key <- keys[[i]]
    id <- c(id, key[1])
    parent <- c(parent, key[2])
  }
  #id <- gsub("X", "", id)

  alleles$id <- id
  alleles$parent <- parent
  rownames(alleles) <- 1:nrow(alleles)
  return(alleles)
}
