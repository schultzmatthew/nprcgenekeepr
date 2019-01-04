#' Forms breeding group(s) with an effort to match a specified sex ratio
#'
#' @description The sex ratio is the ratio of females to males.
#'
#' @param candidates character vector of IDs of the animals available for
#' use in the group.
#' @param numGp integer value indicating the number of groups that should be
#' formed from the list of IDs. Default is 1.
#' @export
#' @param minAge integer value indicating the minimum age to consider in group
#' formation. Pairwise kinships involving an animal of this age or younger will
#'  be ignored. Default is 1 year.
#' @param ped dataframe that is the `Pedigree`. It contains pedigree
#' information including the IDs listed in \code{candidates}.
fillGroupMembersWithSexRatio <-
  function(candidates, groupMembers, grpNum, kin, ped, minAge, numGp,
           sexRatio) {
  potentialSires <- getPotentialSires(candidates, minAge, ped)
  availableMales <- makeAvailable(potentialSires, numGp)
  availableFemales <- makeAvailable(setdiff(candidates, potentialSires), numGp)

  while (TRUE) {
    if (isEmpty(grpNum)) {
      break
    }

    # Select a group at random
    i <- sample(grpNum, 1)[[1]]

    # Select an animal that can be added to this group and add it
    ratio <- calculateSexRatio(groupMembers[[i]], ped)
    if (ratio < sexRatio) { ## need female
      id <- sample(availableFemales[[i]], 1)
      #if (length(availableFemales[[1]]) <= 1)
      #  browser()

      availableFemales <-
        removeSelectedAnimalFromAvailableAnimals(availableFemales, id, numGp)
    } else {
      #if (length(availableMales[[1]]) <= 1)
      #  browser()
      id <- sample(availableMales[[i]], 1)
      availableMales <-
        removeSelectedAnimalFromAvailableAnimals(availableMales, id, numGp)
    }
    groupMembers[[i]] <- c(groupMembers[[i]], id)
    # Remove all relatives from consideration for the group it was added to
    availableMales[[i]] <- setdiff(availableMales[[i]], kin[[id]])
    availableFemales[[i]] <- setdiff(availableFemales[[i]], kin[[id]])
    grpNum <- removeGroupIfNoAvailableAnimals(grpNum, availableMales)
    grpNum <- removeGroupIfNoAvailableAnimals(grpNum, availableFemales)
  }
  groupMembers
}
