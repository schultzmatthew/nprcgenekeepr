#' Convenience function to make the initial groupMembers animal list
#'
#' @return initial groupMembers list
#'
#' @param numGp integer value indicating the number of groups that should be
#' formed from the list of IDs. Default is 1.
#' @param currentGroup character vector of IDs of animals currently assigned
#' to the group. Defaults to NULL assuming no groups are existant.
#' @param candidates character vector of IDs of the animals available for
#' use in the group.
#' @param ped dataframe that is the `Pedigree`. It contains pedigree
#' information including the IDs listed in \code{candidates}.
#' @param harem logical variable when set to \code{TRUE}, the formed groups
#' have a single male at least \code{minAge} old.
#' @param minAge integer value indicating the minimum age to consider in group
#' formation. Pairwise kinships involving an animal of this age or younger will
#'  be ignored. Default is 1 year.
#' @export
makeGroupMembers <- function(numGp, currentGroup, candidates, ped, harem,
                             minAge) {
  groupMembers <- list()
  if (harem) {
    if (length(getPotentialSires(currentGroup, minAge, ped)) > 1)
      stop(paste0("User selected to form harems with more than one male, ",
                  "There are ",
                  length(getPotentialSires(currentGroup, minAge, ped)), " at ",
                  " least ", minAge, " years old in the current group."))
    if (length(currentGroup) > 0 & numGp > 1)
      stop(paste0("User cannot have more than one breeding group with ",
                  "animals in a current group"))
    if (length(getPotentialSires(candidates, minAge, ped)) < numGp &
        length(getPotentialSires(currentGroup, minAge, ped)) == 0)
      stop(paste0("User selected to form harems in ", numGp, " groups with ",
                  "only ", length(getPotentialSires(currentGroup, minAge, ped)),
                  " males at least ",
                  minAge, " years old in the list of candidates."))

    if (length(getPotentialSires(currentGroup, minAge, ped)) == 0) {
      ped <- ped[!is.na(ped$birth), ]
      sires <- sample(getPotentialSires(candidates, minAge, ped), numGp,
                      replace = FALSE)
      for (i in 1:numGp) {
        groupMembers[[i]] <- sires[i]
      }
    }
    if (length(currentGroup) > 0) {
      if (length(getPotentialSires(currentGroup, minAge, ped)) > 0) {
        groupMembers[[1]] <- currentGroup
      } else {
        groupMembers[[1]] <- c(groupMembers[[1]], currentGroup)
      }
    }
  } else {
    for (i in 1:numGp) {
      if (length(currentGroup) == 0) {
        groupMembers[[i]] <- vector()
      } else {
        groupMembers[[i]] <- currentGroup
      }
    }
  }
  groupMembers
}
