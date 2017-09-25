#' Calculates Founder Equivalents and Founder Genome Equivalents
#'
#' Part of the Genetic Value Analysis
#'
#' @param ped the pedigree information in datatable format.  Pedigree
#' (req. fields: id, sire, dam, gen, population).
#'
#' It is assumed that the pedigree has no partial parentage
#' @param alleles dataframe contains an \code{AlleleTable}. This is a
#' table of allele information produced by \code{geneDrop()}.
#' @export
calcFEFG <- function(ped, alleles) {
  ped <- toCharacter(ped, headers = c("id", "sire", "dam"))
  founders <- ped$id[is.na(ped$sire) & is.na(ped$dam)]
  ## UID.founders <- founders[grepl("^U", founders, ignore.case = TRUE)]
  ## UID.founders is not used; It may be a mistake, but it could be vestiges of
  ## something planned that was not done.
  descendants <- ped$id[!(ped$id %in% founders)]

  d <- matrix(0, nrow = length(descendants), ncol = length(founders))
  colnames(d) <- founders
  rownames(d) <- descendants

  founderMatrix <- diag(length(founders))
  colnames(founderMatrix) <- rownames(founderMatrix) <- founders

  d <- rbind(founderMatrix, d)
  founderMatrix <- NULL
  ## Note: skips generation 0.
  ## The references inside matrix d do not work if ped$sire and ped$dam and
  ## thus gen$sire and gen$dam are factors. See test_calcFE.R
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

  r <- calcRetention(ped, alleles)
  return(list(FE = 1 / sum(p^2), FG = 1 / sum((p^2) / r, na.rm = TRUE)))
}
