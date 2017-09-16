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
calcA <- function(alleles, threshold = 1, by.id = FALSE) {
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
calcFE <- function(ped) {
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
#' table of allele information produced by \code{geneDrop()}.
#' @export
calcFG <- function(ped, alleles) {
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
#' table of allele information produced by \code{geneDrop()}.
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
#' @return A dataframe with at least \code{id} and \code{totalOffspring}
#' required and \code{living.offspring} optional.
#'
#' @export
offspringCounts <- function(probands, ped, consider.pop = FALSE) {
  totalOffspring <- findOffspring(probands, ped)
  results <- as.data.frame(totalOffspring)

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
#' \code{z.scores}, \code{import}, \code{totalOffspring}, which is
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
    i <- (!is.na(rpt$origin) & (rpt$totalOffspring == 0) &
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
    i <- (is.na(rpt$origin) & (rpt$totalOffspring == 0) &
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
