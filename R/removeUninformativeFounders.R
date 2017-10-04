#' Remove uninformative founders.
#'
#' Founders (having unknown sire and dam) that appear only one time in a
#' pedigree are uninformative and can be removed from a pedigree without loss
#' of information.
#'
#' @param ped datatable that is the `Pedigree`. It contains pedigree
#' information. The fields \code{sire} and \code{dam} are required.
#'
#' @return A reduced pedigree.
#' @export
removeUninformativeFounders <- function(ped) {

  while (TRUE) {
    founders <- ped$id[is.na(ped$sire) & is.na(ped$dam)]

    sires <- as.data.frame(table(ped$sire[ped$sire %in% founders]))
    dams <- as.data.frame(table(ped$dam[ped$dam %in% founders]))
    sires$Var1 <- as.character(sires$Var1)
    dams$Var1 <- as.character(dams$Var1)

    rmv <- c(sires$Var1[sires$Freq == 1], dams$Var1[dams$Freq == 1])
    if (isEmpty(rmv)) {
      break
    }

    ped$sire[ped$sire %in% rmv] <- NA
    ped$dam[ped$dam %in% rmv] <- NA
    ped <- ped[!(ped$id %in% rmv), ]

  }
  ped
}
