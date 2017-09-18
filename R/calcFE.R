#' Calculates founder Equivalents
#'
#' Part of the Genetic Value Analysis
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
