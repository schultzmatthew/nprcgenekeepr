#' Forms and fills list of animals groups based on provided constraints
#'
#' @return list of animal groups and their member animals
#'
#' @param candidates character vector of IDs of the animals available for
#' use in the group.
#' @param currentGroup character vector of IDs of animals currently assigned
#' to the group. Defaults to NULL assuming no groups are existant.
#' @param kin list of animals and those animals who are related above a
#' threshold value.
#' @param numGp integer value indicating the number of groups that should be
#' formed from the list of IDs. Default is 1.
#'
#' @export
fillGroupMembers <- function(candidates, currentGroup, kin, numGp) {
  groupMembers <- makeGroupMembers(numGp, currentGroup)
  available <- makeAvailable(numGp, candidates)
  grpNum <- makeGrpNum(numGp)
  while (TRUE) {
    if (isEmpty(grpNum)) {
      break
    }

    # Select a group at random
    i <- sample(grpNum, 1)[[1]]

    # Select an animal that can be added to this group and add it
    id <- sample(available[[i]], 1)
    groupMembers[[i]] <- c(groupMembers[[i]], id)
    available <-
      removeSelectedAnimalFromAvailableAnimals(available, id, numGp)

    # Remove all relatives from consideration for the group it was added to
    available[[i]] <- setdiff(available[[i]], kin[[id]])
    grpNum <- removeGroupIfNoAvailableAnimals(grpNum, available)
  }
  groupMembers
}
