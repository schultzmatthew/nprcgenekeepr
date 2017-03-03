# GeneticValueAnalysis.R
# 2015-03-04
#
# Contains functions to calculate the kinship coefficient and genome
# uniqueness for animals listed in a Pedigree table.

###############################################################################
# Data Definitions:
#
# Pedigree
# Contains studbook information for a number of individuals
# A Pedigree is a data.frame with the following columns:
#   id : char
#     The unique identifier for an individual
#   sire : char or NA
#     Identifier of an individual's father, NA if unknown
#   dam : char or NA
#     Identifier of an individual's mother, NA if unknown
#   sex : factor {levels: M, F, U}
#     Sex specifier for an individual
#   gen : int
#     Generation number of the individual
#   birth : Date or NA (optional)
#     An individual's birth date
#   exit : Date or NA (optional)
#     An individual's exit date (death, or departure if applicable)
#   age : float or NA (optional)
#     The individual's current age or age at exit
#   population : bool (optional)
#     Is the id part of the extant population?
#   origin : char or NA (optional)
#     Name of the facility that the individual was imported from; NA if
#     the invidual was not imported
#
# ASSUME: All IDs listed in the sire or dam columns must have a row entry in
#   the id column

# AlleleTable
# A AlleleTable is a data.frame containing information about alleles an ego
# has inherited
#   id : char
#   parent : factor (levels: sire, dam)
#
#   The AlleleTable also contains n columns representing n alleles; all
#   of these columns are unnamed and conform to the scheme V1, V2, ... Vn
#   V(n) : int
#
#' Get the superset of columns that can be in a pedigree file.
#'
#' @return Superset of columns that can be in a pedigree file.
#' @usage
#' get_inlcude_columns()
#' @export
get_include_columns <- function() { # Replaces INCLUDE.COLUMNS data statement.
  c("id", "sex", "age", "birth", "exit", "population", "condition", "origin")
}

###############################################################################
# Main Function:
#'
reportGV <- function(ped, gu.iter = 5000, gu.thresh = 1, pop = NULL,
                     by.id = TRUE, updateProgress = NULL){
  # Generates a genetic value report for a provided pedigree
  #
  # Parameters
  # ----------
  # ped : `Pedigree`
  #   Pedigree information in a table.
  # gu.iter : int
  #   Number of iterations for the gene-drop analysis,
  #   default is 5000 iterations
  # gu.thresh : int
  #   Threshold number of animals for defining a unique allele.
  #   Default considers an allele "unique" if it is found in only 1 animal.
  # pop : vector (char)
  #   A list of IDs to consider as the population of interest,
  #   default is NULL
  # updateProgress : function or NULL
  #   Function that can be called during each iteration to update a
  #   shiny::Progress object.
  #
  # Return
  # ------
  # data.frame
  #   Returns a genetic value report with animals ranked in order of
  #   descending value.

  if(!is.null(pop)){
    ped$population <- FALSE
    ped$population[ped$id %in% pop] <- TRUE
  } else if(is.null(ped$population)){
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
                       n=gu.iter, updateProgress = updateProgress)

  if(!is.null(updateProgress)){
    updateProgress(detail = "Calculating Genome Uniqueness", value = 1, reset = TRUE)
  }

  # Calculate genome uniqueness and order the rows of the returned data.frame
  gu <- calc.gu(alleles, threshold = gu.thresh, by.id = by.id, pop = probands)
  gu <- gu[probands,]

  if(!is.null(updateProgress)){
    updateProgress(detail = "Calculating Numbers of Offspring", value = 1, reset=TRUE)
  }

  # Get a data.frame of offspring counts for the probands
  offspring <- offspringCounts(probands, ped, consider.pop=TRUE)

  include.cols <- intersect(get_include_columns(), names(ped))

  # Subsetting out the needed demographic information from the pedigree
  rownames(ped) <- ped$id
  demographics <- ped[probands, include.cols]

  if(!is.null(updateProgress)){
    updateProgress(detail="Calculating Founder Equivalents", value=1, reset=TRUE)
  }

  # Calculating founder equivalents and founder genome equivalents
  fe <- calc.fe(ped)
  fg <- calc.fg(ped, alleles)

  # Calculating known founders
  founders <- ped[is.na(ped$sire) & is.na(ped$dam), ]
  males <- founders[(founders$sex=="M") & !grepl("^U", founders$id, ignore.case=T), ]
  females <- founders[(founders$sex=="F") & !grepl("^U", founders$id, ignore.case=T), ]

  finalData <- cbind(demographics, indiv.avgs, z.scores, gu, offspring)
  finalData <- list(report=orderReport(finalData, ped),
                    kinship=kmat,
                    fe=fe,
                    fg=fg,
                    male.founders=nrow(males),
                    female.founders=nrow(females),
                    total=(nrow(males) + nrow(females)))
  return(finalData)
}

###############################################################################
# Kinship Matrix Functions:

# NOTE:
# M Raboin 2014-10-02
# The code for the following kinship function was written by Terry Therneau
# at the Mayo clinic, and taken from his website. This function is part of a
# package written in S (and later ported to R) for calculating kinship and
# other statistics.
#
# Main website:
# http://www.mayo.edu/research/faculty/therneau-terry-m-ph-d/bio-00025991
#
# S-Plus/R Function Page:
# http://www.mayo.edu/research/departments-divisions/department-health
#  -sciences-research/division-biomedical-statistics-informatics/software/
#  s-plus-r-functions
#
# Downloaded 2014-08-26
#
# All of the code on the S-Plus page was stated to be released under the
# GNU General Public License (version 2 or later).
#
# The R version became the kinship2 package available on CRAN:
# http://cran.r-project.org/web/packages/kinship2/index.html

# $Id: kinship.s,v 1.5 2003/01/04 19:07:53 therneau Exp $
#
# Create the kinship matrix, using the algorithm of K Lange,
#  Mathematical and Statistical Methods for Genetic Analysis,
#  Springer, 1997, p 71-72.
#
# The rows (cols) of founders are just .5 * identity matrix, no further
#    processing is needed for them.
# Parents must be processed before their children, and then a child's
#    kinship is just a sum of the kinship's for his/her parents.
#
# Modified:
# 2014-09-08 14:44:26
# M Raboin
# The function previously had an internal call to the kindepth function in
# order to provide the parameter pdepth (the generation number). This version
# requires the generation number to be calculated elsewhere and passed into
# the function.
#
kinship <- function(id, father.id, mother.id, pdepth, sparse=FALSE) {
  # Returns: Matrix (row and col names are 'id')
  n <- length(id)
  if (any(duplicated(id))) stop("All id values must be unique")
  if(sparse){
    kmat <- Diagonal(n+1) / 2
  }
  else{
    kmat <- diag(n+1) / 2
  }

  kmat[n+1,n+1]    <- 0 # if A and B both have "unknown" dad, this ensures
  # that they won't end up 'related' in the matrix

  # id number "n+1" is a placeholder for missing parents
  mrow <- match(mother.id, id, nomatch=n+1) #row number of the mother
  drow <- match(father.id, id, nomatch=n+1) #row number of the dad

  # Those at depth==0 don't need to be processed
  # Subjects with depth=i must be processed before those at depth i+1.
  # Any parent is guarranteed to be at a lower depth than their children
  #  The inner loop on "i" can NOT be replaced with a vectorized expression:
  # sibs' effect on each other is cumulative.
  for (depth in 1:max(pdepth)) {
    indx <- (1:n)[pdepth==depth]
    for (i in indx) {
      mom <- mrow[i]
      dad <- drow[i]
      kmat[i,]  <- kmat[,i] <- (kmat[mom,] + kmat[dad,])/2
      kmat[i,i] <- (1+ kmat[mom,dad])/2
    }
  }

  kmat <- kmat[1:n,1:n]
  dimnames(kmat) <- list(id, id)
  kmat
}


filterKinMatrix <- function(ids, kmat){
  # Filters a kinship matrix to include only the egos listed in 'ids'
  # Parameters
  # ----------
  # ids : char
  #   IDs of interest. The kinship matrix should be reduced to only
  #   include these rows and columns.
  # kmat : matrix
  #   Matrix of pairwise kinship coefficients. Rows and columns
  #   should be named with IDs.
  #
  # Return
  # ------
  # matrix
  #   The reduced kinship matrix with named rows and columns
  #   (row and col names are 'ids')

  return(kmat[(rownames(kmat) %in% ids), (colnames(kmat) %in% ids)])
}

avgKinship <- function(kmat){
  # Calculates the averages kinship for each animal in a kinship matrix
  # Parameters
  # ----------
  # kmat : matrix
  #   Matrix of pairwise kinship coefficients. Animal IDs are the row
  #   and column names.
  #
  # Return
  # ------
  # vector <numeric>
  #   A named vector of average kinship coefficients for each Animal ID.
  #   Elements are named with the IDs from the columns of kmat.

  return(colMeans(kmat, na.rm=TRUE))
}

###############################################################################
# Genome Uniqueness Functions:
#
# Reference:
# Ballou JD, Lacy RC.  1995.  Identifying genetically important individuals
#   for management of genetic variation in pedigreed populations, p 77-111.
#   In: Ballou JD, Gilpin M, Foose TJ, editors.  Population management for
#   survival and recovery. New York (NY): Columbia University Press.
#
# Note:
# The following functions calculate genome uniqueness according to the equation
# described in Ballou & Lacy, above.
#
# It should be noted, however that this function differs slightly in that it
# does not distinguish between founders and non-founders in calculating the
# statistic.
#
# Ballou & Lacy describe genome uniqueness as "the proportion of simulations
# in which an individual receives the only copy of a founder allele." We have
# interpretted this as meaning that genome uniqueness should only be calculated
# for living, non-founder animals. Alleles possessed by living founders are
# not considered when calculating genome uniqueness.
#
# We have a differing view on this, since a living founder can still contribute
# to the population.
# The function below calculates genome uniqueness for all living animals
# and considers all alleles. It does not ignore living founders and their
#






# Our results for genome uniqueness will, therefore differ slightly from those
# returned by Pedscope. Pedscope calculates genome uniqueness only for
# non-founders and ignores the contribution of any founders in the population.
# This will cause Pedscope's genome uniqueness estimates to possibly be
# slightly higher for non-founders than what this function will calculate.

# The estimates of genome uniqueness for founders within the population
# calculated by this function should match the "founder genome uniqueness"
# measure calculated by Pedscope.

calc.gu <- function(alleles, threshold=1, by.id=FALSE, pop=NULL){
  # Calculates genome uniqueness for each ID that is part of the population.
  # Parameters
  # ----------
  # alleles : `AlleleTable`
  #   Table of allele information produced by gene.drop()
  # iterations : int
  #   Number of iterations for the gene drop simulation, default is 5000
  # threshold : int
  #   Maximum number of copies of an allele that can be present in the
  #   population for it to be considered rare. Default is 1.
  # by.id : bool
  # pop : vector <char or NULL>
  #   If provided, this list will be used as the set of animals under
  #   consideration. Otherwise all animals will be considered.
  #
  #
  # Return
  # ------
  # data.frame {rows: id, cols: 'gu'}
  #   A single-column table of genome uniqueness values as percentages.
  #   Rownames are set to 'id' values that are part of the population.

  if(!is.null(pop)){
    alleles <- alleles[alleles$id %in% pop, ]
  }

  # Calculate the number of an individual's alleles that are rare in
  # each simulation and average across all simulated alleles.
  rare <- calc.a(alleles, threshold, by.id)
  iterations <- sum(!(colnames(alleles) %in% c("id", "parent")))
  gu <- rowSums(rare)/(2*iterations)

  # convert to a percentage
  gu <- gu * 100
  gu <- as.data.frame(gu)

  return(gu)
}

gene.drop <- function(id, sire, dam, gen, n=5000, updateProgress=NULL){
  # Performs a gene drop simulation based on the provided pedigree information
  # Parameters
  # ----------
  # id : vector <char>
  #   IDs for a set of animals.
  # sire : vector <char or NA>
  #   IDs of the sires for the set of animals.
  # dam : vector <char or NA>
  #   IDs of the dams for the set of animals.
  # gen : vector <int>
  #   Generation number for each animal.
  # n : int
  #   The number of iterations to simulate. Default is 5000.
  #
  # Return
  # ------
  # data.frame {id, parent, V1 ... Vn}
  #   A data.frame providing the maternal and paternal alleles for an animal
  #   for each iteration. The first two columns provide the animal's ID and
  #   whether the allele came from the sire or dam. These are followed by n
  #   columns indicating the allele for that iteration.

  # Sort the IDs by generation so older generations are first
  ped <- data.frame(id, sire, dam, gen, stringsAsFactors=FALSE)
  rownames(ped) <- id
  ped <- ped[order(gen), ]

  alleles <- list()
  a <- 1

  if(!is.null(updateProgress)){
    updateProgress(detail="Performing Gene-drop Simulation", value=0, reset=TRUE)
  }

  # Iterate through each ID and get the maternal and paternal alleles
  for(id in ped$id){
    alleles[[id]] <- list()
    s <- ped[id, "sire"]
    d <- ped[id, "dam"]

    if(is.na(s)){
      # If the sire is unknown, create a unique set of alleles for him
      alleles[[id]][["sire"]] <- rep(a, n)
      a <- a+1
    } else{
      # Otherwise get his two sets of alleles and randomly select one
      # for each iteration
      s1 <- alleles[[s]][["sire"]]
      s2 <- alleles[[s]][["dam"]]
      alleles[[id]][["sire"]] <- chooseAlleles(s1, s2)
    }

    if(is.na(d)){
      # If the dam is unknown, create a unique set of alleles for her
      alleles[[id]][["dam"]] <- rep(a, n)
      a <- a+1
    } else{
      # Otherwise get her two sets of alleles and randomly select one
      # for each iteration
      d1 <- alleles[[d]][["sire"]]
      d2 <- alleles[[d]][["dam"]]
      alleles[[id]][["dam"]] <- chooseAlleles(d1, d2)
    }

    if(!is.null(updateProgress)){
      updateProgress(n=nrow(ped))
    }
  }

  # Convert the list of alleles to a data.frame
  alleles <- as.data.frame(t(data.frame(alleles, check.names=F)))
  keys <- strsplit(rownames(alleles), ".", fixed=TRUE)

  id <- c()
  parent <- c()
  for(i in 1:length(keys)){
    key <- keys[[i]]
    id <- c(id, key[1])
    parent <- c(parent, key[2])
  }
  id <- gsub("X", "", id)

  alleles$id <- id
  alleles$parent <- parent
  rownames(alleles) <- 1:nrow(alleles)
  return(alleles)
}

chooseAlleles <- function(a1, a2){
  # Combines two vectors of alleles by randomly selecting one allele
  # or the other at each position.
  # Parameters
  # ----------
  # a1, a2 : vector <int>
  #   Equal length vectors of alleles for one individual
  #
  # Return
  # ------
  # int
  #   A new set of alleles created by combining the two provided sets
  #   according to Mendelian inheritance.

  s1 = sample(c(0, 1), length(a1), replace=TRUE)
  s2 = 1 - s1

  return((a1 * s1) + (a2 * s2))
}

calc.a <- function(alleles, threshold=1, by.id=FALSE){
  # 'a' is the number of an individual's alleles that are rare in
  # each simulation.
  # Parameters
  # ----------
  # alleles : matrix {id, parent, V1 ... Vn}
  #   A matrix providing the alleles an animal recieved during
  #   each simulation. The first 2 columns provide the animal ID
  #   and the parent the allele came from. Remaining columns provide
  #   alleles.
  # threshold : int
  #   Maximum number of copies of an allele that can be present in the
  #   population for it to be considered rare. Default is 1.
  # by.id : bool
  #
  # Return
  # ------
  # matrix
  #   A matrix with named rows indicating the number of unique alleles
  #   an animal had during each round of simulation (indicated in columns).

  ids <- alleles$id
  parents <- alleles$parent
  alleles <- alleles[, !(names(alleles) %in% c("id", "parent"))]

  count.rare <- function(a){
    if(by.id){
      f <- freq(a, ids)
    } else{
      f <- freq(a)
    }
    rare.alleles <- f$allele[f$freq <= threshold]
    a <- (a %in% rare.alleles)
    return(tapply(a, ids, sum))
  }

  return(apply(alleles, 2, count.rare))
}

freq <- function(alleles, ids=NULL){
  # Calculates the count of each allele in the provided vector. If
  # ids are provided, the function will only count the unique alleles
  # for an individual (homozygous alleles will be counted as 1).
  #
  # Parameters
  # ----------
  # alleles : vector <int>
  #   Alleles in the population
  # ids : vector <char>
  #   IDs indicating to which animal each allele in 'alleles' belongs.
  #
  # Return
  # ------
  # data.frame {allele, freq}
  #   Table of allele counts within the population.

  if(!is.null(ids)){
    alleles <- unlist(tapply(alleles, as.factor(ids), unique))
  }

  a <- as.data.frame(table(alleles))
  colnames(a) <- c("allele", "freq")
  return(a)
}

###############################################################################
# Additional Statistics:

# Founder Equivalents
calc.fe <- function(ped){
  # Pedigree (req. fields: id, sire, dam, gen, population)
  # ASSUME: Pedigree has no partial parentage
  founders <- ped$id[is.na(ped$sire) & is.na(ped$dam)]
  UID.founders <- founders[grepl("^U", founders, ignore.case=T)]
  descendants <- ped$id[!(ped$id %in% founders)]

  d <- matrix(0, nrow=length(descendants), ncol=length(founders))
  colnames(d) <- founders
  rownames(d) <- descendants

  f <- diag(length(founders))
  colnames(f) <- rownames(f) <- founders

  d <- rbind(f, d)

  for(i in 1:max(ped$gen)){
    gen <- ped[(ped$gen == i), ]

    for(j in 1:nrow(gen)){
      ego <- gen$id[j]
      sire <- gen$sire[j]
      dam <- gen$dam[j]
      d[ego, ] <- (d[sire, ] + d[dam, ]) / 2
    }
  }

  current_desc <- ped$id[ped$population & !(ped$id %in% founders)]
  d <- d[current_desc, ]
  p <- colMeans(d)

  return(1/sum(p^2))
}

# Founder Genome Equivalents
calc.fg <- function(ped, alleles){
  # Pedigree (req. fields: id, sire, dam, gen, population)
  # ASSUME: Pedigree has no partial parentage
  founders <- ped$id[is.na(ped$sire) & is.na(ped$dam)]
  UID.founders <- founders[grepl("^U", founders, ignore.case=T)]
  descendants <- ped$id[!(ped$id %in% founders)]

  d <- matrix(0, nrow=length(descendants), ncol=length(founders))
  colnames(d) <- founders
  rownames(d) <- descendants

  f <- diag(length(founders))
  colnames(f) <- rownames(f) <- founders

  d <- rbind(f, d)

  for(i in 1:max(ped$gen)){
    gen <- ped[(ped$gen == i), ]

    for(j in 1:nrow(gen)){
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
  return(1/sum((p^2)/r, na.rm=TRUE))
}

# Allelic Retention
calc.retention <- function(ped, alleles){
  # ASSUME: Pedigree has no partial parentage
  founders <- ped$id[is.na(ped$sire) & is.na(ped$dam)]
  descendants <- ped$id[ped$population & !(ped$id %in% founders)]

  founders <- alleles[(alleles$id %in% founders), c("id", "V1")]
  colnames(founders) <- c("id", "allele")

  alleles <- alleles[(alleles$id %in% descendants),
                     !(colnames(alleles) %in% c("id", "parent"))]

  retained <- apply(alleles, 2, function(a){founders$allele %in% a})
  retained <- rowSums(retained, na.rm=TRUE)/ncol(retained)
  founders <- cbind(founders, retained)

  founders <- tapply(founders$retained, founders$id, mean)
  return(founders)
}



###############################################################################
# Additional Report Information:

offspringCounts <- function(probands, ped, consider.pop=FALSE){
  # Finds the number of total offspring for an animal in the provided pedigree;
  # optionally find the number that are part of the population of interest.
  #
  # Parameters
  # ----------
  # ids : vector <char>
  #   Egos for which offspring should be counted.
  # ped : `Pedigree`
  #   The complete pedigree.
  # consider.pop : bool
  #   Should the number of offspring that are part of the focal population
  #   be counted? Default is false.
  #
  # Return
  # ------
  # data.frame {cols: 'id' (req),
  #                   'total.offspring' (req),
  #                   'living.offspring' (opt)}
  #   Returns a data.frame containing the total offspring for the provided ids,
  #   and optionally, the number that are part of the focal population.

  total.offspring <- findOffspring(probands, ped)
  results <- as.data.frame(total.offspring)

  if(consider.pop && !is.null(ped$population)){
    pop <- ped[ped$population, ]
    living.offspring <- findOffspring(probands, pop)
    results <- cbind(results, living.offspring)
  }
  return(results)
}

findOffspring <- function(probands, ped){
  # Finds the number of total offspring for an animal in the provided pedigree
  # Parameters
  # ----------
  # probands : vector <char>
  #   List of animals for which offspring should be counted and returned.
  # ped : `Pedigree`
  #   The complete pedigree information
  #
  # Returns
  # -------
  # named vector
  #   Offpsring counts for each animal in 'probands'. Rownames are set to the
  #   IDs from 'probands'.

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

orderReport <- function(rpt, ped){
  # Takes in the results from a genetic value analysis, and orders the report
  # according to the ranking scheme we have developed.
  # Parameters
  # ----------
  # rpt : data.frame {req colnames: id, gu, z.scores, import, total.offspring}
  #   A data.frame of results from a genetic value analysis
  # ped : `Pedigree`
  #   The complete pedigree for the animals in the report.
  #
  # Return
  # ------
  # data.frame
  #   The rpt data.frame, sorted according to the ranking scheme:
  #   1. imported animals with no offspring
  #   2. animals with genome uniqueness above 10%, ranked by descending gu
  #   3. animals with mean kinship less than 0.25, ranked by ascending mk
  #   4. all remaining animals, ranked by ascending mk

  finalRpt <- list()

  founders <- ped$id[is.na(ped$sire) & is.na(ped$dam)]

  if("origin" %in% names(rpt)){
    # imports with no offspring
    i <- (!is.na(rpt$origin) & (rpt$total.offspring == 0) &
            (rpt$id %in% founders))

    imports <- rpt[i, ]
    rpt <- rpt[!i, ]
    if("age" %in% names(rpt)){
      finalRpt$imports <- imports[with(imports, order(age)), ]
    }
    else{
      finalRpt$imports <- imports[with(imports, order(id)), ]
    }

    # ONPRC-born animals with no parentage
    i <- (is.na(rpt$origin) & (rpt$total.offspring == 0) &
            (rpt$id %in% founders))

    no.parentage <- rpt[i, ]
    rpt <- rpt[!i, ]
    if("age" %in% names(rpt)){
      finalRpt$no.parentage <- no.parentage[with(no.parentage, order(age)), ]
    }
    else{
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

rankSubjects <- function(rpt){
  # Adds a column to 'rpt' containing integers from 1 to nrow, and provides
  # a value designation for each animal of "high value" or "low value"
  #
  # Parameters
  # ----------
  # rpt : list of data.frame {req. colnames: value}
  #   A list of dataframes containing genetic value data for the population.
  #   Dataframes separate out those animals that are imports, those that have
  #   high genome uniqueness (gu > 10%), those that have low mean kinship
  #   (mk < 0.25), and the remainder.
  #
  # Return
  # ------
  # list of data.frame
  #   Updated list of data.frames, with value and ranking information added.

  rnk <- 1

  for(i in 1:length(rpt)){
    if(nrow(rpt[[i]]) == 0){
      next
    }

    if(names(rpt[i]) == "low.val"){
      rpt[[i]][, "value"] <- "Low Value"
    } else if(names(rpt[i]) == "no.parentage"){
      rpt[[i]][, "value"] <- "Undetermined"
    } else{
      rpt[[i]][, "value"] <- "High Value"
    }

    if(names(rpt[i]) == "no.parentage"){
      rpt[[i]][, "rank"] <- NA
    } else{
      rpt[[i]][, "rank"] <- rnk:(rnk + nrow(rpt[[i]]) - 1)
      rnk <- rnk + nrow(rpt[[i]])
    }

  }
  return(rpt)
}
###############################################################################