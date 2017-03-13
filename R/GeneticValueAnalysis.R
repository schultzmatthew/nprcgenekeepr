#' Get the superset of columns that can be in a pedigree file.
#'
#' @return Superset of columns that can be in a pedigree file.
#' examples
#' get_inlcude_columns()
#'
#' @export
get_include_columns <- function() { # Replaces INCLUDE.COLUMNS data statement.
  c("id", "sex", "age", "birth", "exit", "population", "condition", "origin")
}
#' Generates a genetic value report for a provided pedigree.
#'
#' This is the main function for the Genetic Value Analysis.
#'
#' @param ped the pedigree information in datatable format
#' @param gu.iter integer indicating the number of iterations for the gene-drop
#'  analysis. Default is 5000 iterations
#' @param gu.thresh integer indicating the threshold number of animals for
#' defining a unique allele. Default considers an allele "unique"
#' if it is found in only 1 animal.
#' @param pop character 1 vector with animal IDs to consider as the population of
#' interest. The default is NULL.
#' @param by.id logical varioable of length 1 that is passed through to
#' eventually be used by \code{freq()}, which alculates the count of each
#'  allele in the provided vector. If \code{by.id} is TRUE and ids are provided,
#'  the function will only count the unique alleles for an individual
#'   (homozygous alleles will be counted as 1).
#' @param updateProgress function or NULL. If this function is defined, it
#' will be called during each iteration to update a
#' \code{shiny::Progress} object.
#'
#' @return A dataframe with the genetic value report. Animals are ranked
#' in order of descending value.
#'
#' @export
reportGV <- function(ped, gu.iter = 5000, gu.thresh = 1, pop = NULL,
                     by.id = TRUE, updateProgress = NULL) {
  # Generates a genetic value report for a provided pedigree

  if (!is.null(pop)) {
    ped$population <- FALSE
    ped$population[ped$id %in% pop] <- TRUE
  } else if (is.null(ped$population)) {
    ped$population <- TRUE
  }

  # Get a list of animals in the population to consider
  probands <- as.character(ped$id[ped$population])

  # Generate the kinship matrix and filter down to the animals of interest
  kmat.raw <- kinship(ped$id, ped$sire, ped$dam, ped$gen)
  kmat <- filterKinMatrix(probands, kmat.raw)

  # Calculate the average kinship, and convert to z-scores
  indiv.avgs <- avgKinship(kmat)
  indiv.avgs <- indiv.avgs[probands] # making sure the order is correct
  z.scores <- scale(indiv.avgs)

  # Perform the gene drop simulation
  alleles <- gene.drop(ped$id, ped$sire, ped$dam, ped$gen,
                       n = gu.iter, updateProgress = updateProgress)

  if (!is.null(updateProgress)) {
    updateProgress(detail = "Calculating Genome Uniqueness", value = 1,
                   reset = TRUE)
  }

  # Calculate genome uniqueness and order the rows of the returned data.frame
  gu <- calc.gu(alleles, threshold = gu.thresh, by.id = by.id, pop = probands)
  gu <- gu[probands,]

  if (!is.null(updateProgress)) {
    updateProgress(detail = "Calculating Numbers of Offspring", value = 1,
                   reset = TRUE)
  }

  # Get a data.frame of offspring counts for the probands
  offspring <- offspringCounts(probands, ped, consider.pop = TRUE)

  include.cols <- intersect(get_include_columns(), names(ped))

  # Subsetting out the needed demographic information from the pedigree
  rownames(ped) <- ped$id
  demographics <- ped[probands, include.cols]

  if (!is.null(updateProgress)) {
    updateProgress(detail = "Calculating Founder Equivalents", value = 1,
                   reset = TRUE)
  }

  # Calculating founder equivalents and founder genome equivalents
  fe <- calc.fe(ped)
  fg <- calc.fg(ped, alleles)

  # Calculating known founders
  founders <- ped[is.na(ped$sire) & is.na(ped$dam), ]
  males <- founders[(founders$sex == "M") & !grepl("^U", founders$id,
                                                 ignore.case = TRUE), ]
  females <- founders[(founders$sex == "F") & !grepl("^U", founders$id,
                                                   ignore.case = TRUE), ]

  finalData <- cbind(demographics, indiv.avgs, z.scores, gu, offspring)
  finalData <- list(report = orderReport(finalData, ped),
                    kinship = kmat,
                    fe = fe,
                    fg = fg,
                    male.founders = nrow(males),
                    female.founders = nrow(females),
                    total = (nrow(males) + nrow(females)))
  return(finalData)
}
#' Generates a kinship matrix.
#'
#' The function previously had an internal call to the kindepth function in
#' order to provide the parameter pdepth (the generation number). This version
#' requires the generation number to be calculated elsewhere and passed into
#' the function.
#'
#' The rows (cols) of founders are just .5 * identity matrix, no further
#'    processing is needed for them.
#' Parents must be processed before their children, and then a child's
#'    kinship is just a sum of the kinship's for his/her parents.
#'
#' @param id character vector of IDs for a set of animals.
#' @param father.id character vector or NA for the IDs of the sires for the set
#' of animals.
#' @param mother.id character vector or NA for the IDs of the dams for the set
#' of animals.
#' @param pdepth integer vector indicating the generation number for each animal.
#' @param sparse logical flag. If \code{TRUE}, \code{Matrix::Diagnol()} is
#' used to make a unit diagnol matrix. If \code{FALSE}, \code{base::diag()} is
#' used to make a unit square matrix.
#'
#' @description {Kinship Matrix Functions} {
#' The code for the kinship function was written by Terry Therneau
#' at the Mayo clinic and taken from his website. This function is part of a
#' package written in S (and later ported to R) for calculating kinship and
#' other statistics.
#' }
#'
#' @author {Terry Therneau, original version}
#'
#'
#' @references {Main website} \url{http://www.mayo.edu/research/faculty/therneau-terry-m-ph-d/bio-00025991}
#'
#'
#' @references {S-Plus/R Function Page}
#' \url{http://www.mayo.edu/research/departments-divisions/department-health
#'  -sciences-research/division-biomedical-statistics-informatics/software/}
#'
#'  @description {s-plus-r-functions} {Downloaded 2014-08-26}
#'
#' All of the code on the S-Plus page was stated to be released under the
#' GNU General Public License (version 2 or later).
#'
#' The R version became the kinship2 package available on CRAN:
#' @references \url{http://cran.r-project.org/web/packages/kinship2/index.html}
#'
#' $Id: kinship.s,v 1.5 2003/01/04 19:07:53 therneau Exp $
#'
#' @references {Create the kinship matrix, using the algorithm of K Lange,
#'  Mathematical and Statistical Methods for Genetic Analysis,
#'  Springer, 1997, p 71-72.}
#'
#' @author {as modified by, M Raboin, 2014-09-08 14:44:26}
#'
#' @import Matrix
#' @export
kinship <- function(id, father.id, mother.id, pdepth, sparse = FALSE) {
  # Returns: Matrix (row and col names are 'id')
  n <- length(id)
  if (any(duplicated(id)))
    stop("All id values must be unique")
  if (sparse) {
    kmat <- Diagonal(n + 1) / 2
  }
  else {
    kmat <- diag(n + 1) / 2
  }

  kmat[n + 1, n + 1]    <- 0 # if A and B both have "unknown" dad, this ensures
  # that they won't end up 'related' in the matrix

  # id number "n + 1" is a placeholder for missing parents
  mrow <- match(mother.id, id, nomatch = n + 1) #row number of the mother
  drow <- match(father.id, id, nomatch = n + 1) #row number of the dad

  # Those at depth == 0 don't need to be processed
  # Subjects with depth = i must be processed before those at depth i + 1.
  # Any parent is guarranteed to be at a lower depth than their children
  #  The inner loop on "i" can NOT be replaced with a vectorized expression:
  # sibs' effect on each other is cumulative.
  for (depth in 1:max(pdepth)) {
    indx <- (1:n)[pdepth == depth]
    for (i in indx) {
      mom <- mrow[i]
      dad <- drow[i]
      kmat[i, ]  <- kmat[, i] <- (kmat[mom,] + kmat[dad, ]) / 2
      kmat[i, i] <- (1 + kmat[mom, dad]) / 2
    }
  }

  kmat <- kmat[1:n, 1:n]
  dimnames(kmat) <- list(id, id)
  kmat
}
#' Filters a kinship matrix to include only the egos listed in 'ids'
#'
#' @param ids character vector containing the IDs of interest.
#' The kinship matrix should be reduced to only include these rows and columns.
#' @param kmat a numeric matrix of pairwise kinship coefficients.
#' Rows and columns should be named with IDs.
#'
#' @return A numeric matrix that is the reduced kinship matrix with named
#' rows and columns (row and col names are 'ids')
#' @export
filterKinMatrix <- function(ids, kmat) {
  return(kmat[(rownames(kmat) %in% ids), (colnames(kmat) %in% ids)])
}
#' Calculates the averages kinship for each animal in a kinship matrix
#'
#' @param kmat a numeric matrix of pairwise kinship coefficients.
#' Animal IDs are the row and column names.
#'
#' @return A named numeric vecter of average kinship coefficients for each
#' animal ID. Elements are named with the IDs from the columns of kmat.

avgKinship <- function(kmat) {
  return(colMeans(kmat, na.rm = TRUE))
}
#' Calculates genome uniqueness for each ID that is part of the population.
#'
#' The following functions calculate genome uniqueness according to the equation
#' described in Ballou & Lacy.
#'
#' It should be noted, however that this function differs slightly in that it
#' does not distinguish between founders and non-founders in calculating the
#' statistic.
#'
#' Ballou & Lacy describe genome uniqueness as "the proportion of simulations
#' in which an individual receives the only copy of a founder allele." We have
#' interpretted this as meaning that genome uniqueness should only be calculated
#' for living, non-founder animals. Alleles possessed by living founders are
#' not considered when calculating genome uniqueness.
#'
#' We have a differing view on this, since a living founder can still contribute
#' to the population.
#' The function below calculates genome uniqueness for all living animals
#' and considers all alleles. It does not ignore living founders and their
#'
#' Our results for genome uniqueness will, therefore differ slightly from those
#' returned by Pedscope. Pedscope calculates genome uniqueness only for
#' non-founders and ignores the contribution of any founders in the population.
#' This will cause Pedscope's genome uniqueness estimates to possibly be
#' slightly higher for non-founders than what this function will calculate.
#'
#' The estimates of genome uniqueness for founders within the population
#' calculated by this function should match the "founder genome uniqueness"
#' measure calculated by Pedscope.
#'
#' @description {Genome Uniqueness Functions}{}
#'
#' @references Ballou JD, Lacy RC.  1995. Identifying genetically important
#' individuals for management of genetic variation in pedigreed populations,
#' p 77-111. In: Ballou JD, Gilpin M, Foose TJ, editors.
#' Population management for survival and recovery. New York (NY):
#' Columbia University Press.
#'
#' @param alleles dataframe of containing an \code{AlleleTable}. This is a
#' table of allele information produced by \code{gene.drop()}.
#' An AlleleTable contains information about alleles an ego has inherited.
#' It contains the following columns:
#' \itemize{
#'  \item {id} {--- A character vector of IDs for a set of animals.}
#'  \item {parent} {--- A factor with levels of sire and dam.}
#'  \item {V1} {--- Unnamed integer column representing allele 1.}
#'  \item {V2} {--- Unnamed integer column representing allele 2.}
#'  \item {...} {--- Unnamed integer columns representing alleles.}
#'  \item {Vn} {--- Unnamed integer coulumn representing the nth column.}}
#'
#' @param threshold an integer indicating the maximum number of copies of an
#' allele that can be present in the population for it to be considered rare.
#' Default is 1.
#' @param by.id logical varioable of length 1 that is passed through to
#' eventually be used by \code{freq()}, which alculates the count of each
#' allele in the provided vector. If \code{by.id} is TRUE and ids are provided,
#' the function will only count the unique alleles for an individual
#' (homozygous alleles will be counted as 1).
#' @param pop character vector with animal IDs to consider as the population of
#' interest, otherwise all animals will be considered. The default is NULL.
#'
#' @return Dataframe \code{rows: id, col: gu}
#'  A single-column table of genome uniqueness values as percentages.
#'  Rownames are set to 'id' values that are part of the population.
#' @export
calc.gu <- function(alleles, threshold = 1, by.id = FALSE, pop = NULL) {
  if (!is.null(pop)) {
    alleles <- alleles[alleles$id %in% pop, ]
  }

  # Calculate the number of an individual's alleles that are rare in
  # each simulation and average across all simulated alleles.
  rare <- calc.a(alleles, threshold, by.id)
  iterations <- sum(!(colnames(alleles) %in% c("id", "parent")))
  gu <- rowSums(rare) / (2*iterations)

  # convert to a percentage
  gu <- gu * 100
  gu <- as.data.frame(gu)

  return(gu)
}
#' Performs a gene drop simulation based on the provided pedigree information
#'
#' @param id character vector of IDs for a set of animals.
#' @param sire character vector or NA for the IDs of the sires for the set of
#'  animals.
#' @param dam character vector or NA for the IDs of the dams for the set of animals.
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
gene.drop <- function(id, sire, dam, gen, n = 5000, updateProgress = NULL) {
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
#' Combines two vectors of alleles by randomly selecting one allele
#' or the other at each position.
#'
#' @param a1 integer vector with first allele for each individual
#' @param a2 integer vector with second allele for each individual
#' \code{a1} and \code{a2} are equal length vectors of alleles for one
#' individual
#'
#' @return An integer vector with the result of sampling from \code{a1}
#' and \code{a2} according to Mendelian inheritance.
#' @export
chooseAlleles <- function(a1, a2) {
  s1 = sample(c(0, 1), length(a1), replace = TRUE)
  s2 = 1 - s1

  return((a1 * s1) + (a2 * s2))
}
#' Calculates \code{a}, the number of an individual's alleles that are rare in
#' each simulation.
#'
#' @param alleles a matrix with {id, parent, V1 ... Vn} providing the alleles
#' an animal recieved during each simulation.
#' The first 2 columns provide the animal ID and the parent the allele came
#' from. Remaining columns provide alleles.
#' @param threshold an integer indicating the maximum number of copies of an
#'  allele that can be present in the population for it to be considered rare.
#'  Default is 1.
#' @param by.id logical varioable of length 1 that is passed through to
#' eventually be used by \code{freq()}, which alculates the count of each
#'  allele in the provided vector. If \code{by.id} is TRUE and ids are provided,
#'  the function will only count the unique alleles for an individual
#'   (homozygous alleles will be counted as 1).
#' @return A matrix with named rows indicating the number of unique alleles
#'   an animal had during each round of simulation (indicated in columns).
calc.a <- function(alleles, threshold = 1, by.id = FALSE) {
  ids <- alleles$id
  parents <- alleles$parent
  alleles <- alleles[, !(names(alleles) %in% c("id", "parent"))]

  count.rare <- function(a) {
    if (by.id) {
      f <- freq(a, ids)
    } else {
      f <- freq(a)
    }
    rare.alleles <- f$allele[f$freq <= threshold]
    a <- (a %in% rare.alleles)
    return(tapply(a, ids, sum))
  }

  return(apply(alleles, 2, count.rare))
}
#' Calculates the count of each allele in the provided vector.
#'
#'  If ids are provided, the function will only count the unique alleles
#' for an individual (homozygous alleles will be counted as 1).
#'
#' @param alleles an integer vector of alleles in the population
#' @param ids character vector of IDs indicating to which animal each allele
#' in \code{alleles} belongs.
#'
#' @return a data.frame with columns \code{allele} and \code{freq}. This is a
#'  table of allele counts within the population.
freq <- function(alleles, ids = NULL) {
  if (!is.null(ids)) {
    alleles <- unlist(tapply(alleles, as.factor(ids), unique))
  }

  a <- as.data.frame(table(alleles))
  colnames(a) <- c("allele", "freq")
  return(a)
}

###############################################################################
# Additional Statistics:

#' Calculates founder Equivalents
#'
#' @param ped the pedigree information in datatable format.  Pedigree
#' (req. fields: id, sire, dam, gen, population).
#'
#' It is assumed that the pedigree has no partial parentage
#' @export
calc.fe <- function(ped) {
  founders <- ped$id[is.na(ped$sire) & is.na(ped$dam)]
  UID.founders <- founders[grepl("^U", founders, ignore.case = TRUE)]
  descendants <- ped$id[!(ped$id %in% founders)]

  d <- matrix(0, nrow = length(descendants), ncol = length(founders))
  colnames(d) <- founders
  rownames(d) <- descendants

  f <- diag(length(founders))
  colnames(f) <- rownames(f) <- founders

  d <- rbind(f, d)

  for (i in 1:max(ped$gen)) {
    gen <- ped[(ped$gen == i), ]

    for (j in 1:nrow(gen)) {
      ego <- gen$id[j]
      sire <- gen$sire[j]
      dam <- gen$dam[j]
      d[ego, ] <- (d[sire, ] + d[dam, ]) / 2
    }
  }

  current_desc <- ped$id[ped$population & !(ped$id %in% founders)]
  d <- d[current_desc, ]
  p <- colMeans(d)

  return(1 / sum(p^2))
}
#' Calculates Founder Genome Equivalents
#'
#' @param ped the pedigree information in datatable format.  Pedigree
#' (req. fields: id, sire, dam, gen, population).
#'
#' It is assumed that the pedigree has no partial parentage
#' @param alleles dataframe of containing an \code{AlleleTable}. This is a
#' table of allele information produced by \code{gene.drop()}.
#' @export
calc.fg <- function(ped, alleles) {
  founders <- ped$id[is.na(ped$sire) & is.na(ped$dam)]
  UID.founders <- founders[grepl("^U", founders, ignore.case = TRUE)]
  descendants <- ped$id[!(ped$id %in% founders)]

  d <- matrix(0, nrow = length(descendants), ncol = length(founders))
  colnames(d) <- founders
  rownames(d) <- descendants

  f <- diag(length(founders))
  colnames(f) <- rownames(f) <- founders

  d <- rbind(f, d)

  for (i in 1:max(ped$gen)) {
    gen <- ped[(ped$gen == i), ]

    for (j in 1:nrow(gen)) {
      ego <- gen$id[j]
      sire <- gen$sire[j]
      dam <- gen$dam[j]
      d[ego, ] <- (d[sire, ] + d[dam, ]) / 2
    }
  }

  current_desc <- ped$id[ped$population & !(ped$id %in% founders)]
  d <- d[current_desc, ]
  p <- colMeans(d)

  r <- calc.retention(ped, alleles)
  return(1 / sum((p^2) / r, na.rm = TRUE))
}
#' Calculates Allelic Retention
#'
#' @param ped the pedigree information in datatable format.  Pedigree
#' (req. fields: id, sire, dam, gen, population).
#'
#' It is assumed that the pedigree has no partial parentage
#' @param alleles dataframe of containing an \code{AlleleTable}. This is a
#' table of allele information produced by \code{gene.drop()}.
#' @export
calc.retention <- function(ped, alleles) {
  # ASSUME: Pedigree has no partial parentage
  founders <- ped$id[is.na(ped$sire) & is.na(ped$dam)]
  descendants <- ped$id[ped$population & !(ped$id %in% founders)]

  founders <- alleles[(alleles$id %in% founders), c("id", "V1")]
  colnames(founders) <- c("id", "allele")

  alleles <- alleles[(alleles$id %in% descendants),
                     !(colnames(alleles) %in% c("id", "parent"))]

  retained <- apply(alleles, 2, function(a) {founders$allele %in% a})
  retained <- rowSums(retained, na.rm = TRUE) / ncol(retained)
  founders <- cbind(founders, retained)

  founders <- tapply(founders$retained, founders$id, mean)
  return(founders)
}
###############################################################################
# Additional Report Information:
#
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
#' @return A dataframe with at least \code{id} and \code{total.offspring}
#' required and \code{living.offspring} optional.
#'
#' @export
offspringCounts <- function(probands, ped, consider.pop = FALSE) {
  total.offspring <- findOffspring(probands, ped)
  results <- as.data.frame(total.offspring)

  if (consider.pop && !is.null(ped$population)) {
    pop <- ped[ped$population, ]
    living.offspring <- findOffspring(probands, pop)
    results <- cbind(results, living.offspring)
  }
  return(results)
}

#' Finds the number of total offspring for each animal in the provided pedigree.
#'
#' @param probands character vector of egos for which offspring should be
#' counted and returned.
#' @param ped the pedigree information in datatable format.  Pedigree
#' (req. fields: id, sire, dam, gen, population).
#' This requires complete pedigree information.
#'
#' @return a named vector containing the offpsring counts for each animal in
#' \code{probands}. Rownames are set to the IDs from \code{probands}.

findOffspring <- function(probands, ped) {
  sires <- tapply(ped$id, as.factor(ped$sire), length)
  dams <- tapply(ped$id, as.factor(ped$dam), length)
  offspring <- c(sires, dams)

  idx <- match(probands, names(offspring))
  offspring <- offspring[idx]
  names(offspring)[is.na(idx)] <- probands[is.na(idx)]
  offspring[is.na(idx)] <- 0

  return(offspring)
}

###############################################################################
# Report Formatting:
#' Order the results of the genetic value analysis for use in a report.
#'
#' Takes in the results from a genetic value analysis and orders the report
#' according to the ranking scheme we have developed.
#'
#' @param rpt a dataframe with required colnames \code{id}, \code{gu},
#' \code{z.scores}, \code{import}, \code{total.offspring}, which is
#' a data.frame of results from a genetic value analysis.
#' @param ped the pedigree information in datatable format with required
#' colnames \code{id}, \code{sire}, \code{dam}, \code{gen}, \code{population}).
#' This requires complete pedigree information..
#'
#' @return A dataframe, which is \code{rpt} sorted according to the ranking scheme:
#' \itemize{
#'  \item imported animals with no offspring
#'  \item animals with genome uniqueness above 10%, ranked by descending gu
#'  \item animals with mean kinship less than 0.25, ranked by ascending mk
#'  \item all remaining animals, ranked by ascending mk
#' }
#' @export
orderReport <- function(rpt, ped) {

  finalRpt <- list()

  founders <- ped$id[is.na(ped$sire) & is.na(ped$dam)]

  if ("origin" %in% names(rpt)) {
    # imports with no offspring
    i <- (!is.na(rpt$origin) & (rpt$total.offspring == 0) &
            (rpt$id %in% founders))

    imports <- rpt[i, ]
    rpt <- rpt[!i, ]
    if ("age" %in% names(rpt)) {
      finalRpt$imports <- imports[with(imports, order(age)), ]
    }
    else {
      finalRpt$imports <- imports[with(imports, order(id)), ]
    }

    # ONPRC-born animals with no parentage
    i <- (is.na(rpt$origin) & (rpt$total.offspring == 0) &
            (rpt$id %in% founders))

    no.parentage <- rpt[i, ]
    rpt <- rpt[!i, ]
    if ("age" %in% names(rpt)) {
      finalRpt$no.parentage <- no.parentage[with(no.parentage, order(age)), ]
    }
    else {
      finalRpt$no.parentage <- no.parentage[with(no.parentage, order(id)), ]
    }
  }

  # subjects with > 10% genome uniqueness
  high.gu <- rpt[(rpt$gu > 10), ]
  finalRpt$high.gu <- high.gu[with(high.gu, order(-trunc(gu), z.scores)), ]
  rpt <- rpt[!(rpt$gu > 10), ]

  # subjects with <= 10% genome uniqueness and <= 0.25 z-score
  low.mk <- rpt[(rpt$z.scores <= 0.25), ]
  finalRpt$low.mk <- low.mk[with(low.mk, order(z.scores)), ]

  rpt <- rpt[!(rpt$z.scores <= 0.25), ]

  # subjects with <= 10% genome uniqueness and > 0.25 z-score
  finalRpt$low.val <- rpt[with(rpt, order(z.scores)), ]

  include.cols <- intersect(c("imports", "high.gu", "low.mk",
                              "low.val", "no.parentage"),
                            names(finalRpt))

  finalRpt <- finalRpt[include.cols]
  finalRpt <- rankSubjects(finalRpt)
  finalRpt <- do.call("rbind", finalRpt)
  rownames(finalRpt) <- seq(nrow(finalRpt))
  return(finalRpt)
}
#' Ranks animals based on genetic value.
#'
#' Adds a column to \code{rpt} containing integers from 1 to nrow, and provides
#' a value designation for each animal of "high value" or "low value"
#'
#' @param rpt a list of data.frame {req. colnames: value} containing genetic
#' value data for the population. Dataframes separate out those animals that
#' are imports, those that have high genome uniqueness (gu > 10%), those that
#' have low mean kinship (mk < 0.25), and the remainder.
#'
#' @return A list of dataframes with value and ranking information added.
#' @export
rankSubjects <- function(rpt) {
  rnk <- 1

  for (i in 1:length(rpt)) {
    if (nrow(rpt[[i]]) == 0) {
      next
    }

    if (names(rpt[i]) == "low.val") {
      rpt[[i]][, "value"] <- "Low Value"
    } else if (names(rpt[i]) == "no.parentage") {
      rpt[[i]][, "value"] <- "Undetermined"
    } else {
      rpt[[i]][, "value"] <- "High Value"
    }

    if (names(rpt[i]) == "no.parentage") {
      rpt[[i]][, "rank"] <- NA
    } else {
      rpt[[i]][, "rank"] <- rnk:(rnk + nrow(rpt[[i]]) - 1)
      rnk <- rnk + nrow(rpt[[i]])
    }

  }
  return(rpt)
}
