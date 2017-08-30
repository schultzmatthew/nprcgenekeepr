#' Trim pedigree to ancestors of provided group removing uninformative
#' ancestors.
#'
#' Filters a pedigree down to only the ancestors of the provided group.
#' removing unnecessary individuals from the studbook. This version builds
#' the pedigree back in time starting from a group of probands, then moves
#' back down the tree trimming off uninformative ancestors.
#'
#' @param probands a character vector with the list of animals whose ancestors
#' should be included in the final pedigree.
#' @param ped datatable that is the `Pedigree`. It contains pedigree
#' information. The fields \code{sire} and \code{dam} are required.
#'
#' @return A reduced pedigree.
#' @export
trimPedigree2 <- function(probands, ped) {
  animals <- probands

  while (TRUE) {
    sires <- ped$sire[ped$id %in% animals]
    dams <- ped$dam[ped$id %in% animals]

    parents <- unique(union(sires, dams))
    parents <- parents[!is.na(parents)]
    added <- setdiff(parents, animals)

    if (identical(added, character(0))) {
      break
    }
    if (identical(added, numeric(0))) {
      break
    }
    if (identical(added, integer(0))) {
      break
    }
    animals <- union(animals, parents)
  }

  ped <- ped[ped$id %in% animals, ]
  p <- ped

  while (TRUE) {
    founders <- p$id[is.na(p$sire) & is.na(p$dam)]

    sires <- as.data.frame(table(p$sire[p$sire %in% founders]))
    dams <- as.data.frame(table(p$dam[p$dam %in% founders]))
    sires$Var1 <- as.character(sires$Var1)
    dams$Var1 <- as.character(dams$Var1)

    rmv <- c(sires$Var1[sires$Freq == 1], dams$Var1[dams$Freq == 1])
    if (isEmpty(rmv)) {
      break
    }

    p$sire[p$sire %in% rmv] <- NA
    p$dam[p$dam %in% rmv] <- NA
    p <- p[!(p$id %in% rmv), ]

  }

  # Adding back second parents where one is known
  single.parents <- p$id[(is.na(p$sire) & !is.na(p$dam)) |
                           (!is.na(p$sire) & is.na(p$dam))]

  add.back <- c()
  for (id in single.parents) {
    if (!is.na(ped$sire[ped$id == id]) & !is.na(ped$dam[ped$id == id])) {

      if (is.na(p$sire[p$id == id])) {
        add.back <- c(add.back, ped$sire[ped$id == id])
        p[(p$id == id), "sire"] <- ped$sire[ped$id == id]

      } else{
        add.back <- c(add.back, ped$dam[ped$id == id])
        p[(p$id == id), "dam"] <- ped$dam[ped$id == id]
      }
    }
  }
  add.back <- ped[(ped$id %in% add.back), ]
  add.back$sire <- NA
  add.back$dam <- NA

  p <- rbind(p, add.back)

  return(p)
}
