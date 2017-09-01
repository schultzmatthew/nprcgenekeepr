#' Performs a gene drop simulation based on the provided pedigree information
#'
#' @param id character vector of IDs for a set of animals.
#' @param sire character vector with IDS of the sires for the set of
#'  animals. \code{NA} is used for missing sires.
#' @param dam character vector with IDS of the dams for the set of
#'  animals. \code{NA} is used for missing dams.
#' @param gen integer vector indicating the generation number for each animal.
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
geneDrop <- function(id, sire, dam, gen, n = 5000, updateProgress = NULL) {
  ## Sort the IDs by generation so older generations are first
  ped <- data.frame(id, sire, dam, gen, stringsAsFactors = FALSE)
  rownames(ped) <- id
  ped <- ped[order(gen), ]

  alleles <- list()
  a <- 1

  if (!is.null(updateProgress)) {
    updateProgress(detail = "Performing Gene-drop Simulation", value = 0,
                   reset = TRUE)
  }

  ## Iterate through each ID and get the maternal and paternal alleles
  for (id in ped$id) {
    alleles[[id]] <- list()
    s <- ped[id, "sire"]
    d <- ped[id, "dam"]

    if (is.na(s)) {
      # If the sire is unknown, create a unique set of alleles for him
      alleles[[id]][["sire"]] <- rep(a, n)
      a <- a + 1
    } else {
      # Otherwise get his two sets of alleles and randomly select one
      # for each iteration
      s1 <- alleles[[s]][["sire"]]
      s2 <- alleles[[s]][["dam"]]
      alleles[[id]][["sire"]] <- chooseAlleles(s1, s2)
    }

    if (is.na(d)) {
      # If the dam is unknown, create a unique set of alleles for her
      alleles[[id]][["dam"]] <- rep(a, n)
      a <- a + 1
    } else {
      # Otherwise get her two sets of alleles and randomly select one
      # for each iteration
      d1 <- alleles[[d]][["sire"]]
      d2 <- alleles[[d]][["dam"]]
      alleles[[id]][["dam"]] <- chooseAlleles(d1, d2)
    }

    if (!is.null(updateProgress)) {
      updateProgress(n = nrow(ped))
    }
  }

  # Convert the list of alleles to a data.frame
  alleles <- as.data.frame(t(data.frame(alleles, check.names = FALSE)))
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
