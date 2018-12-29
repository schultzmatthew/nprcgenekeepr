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
###
fillGroupMembers2 <- function(candidates, currentGroup, kin, numGp) {
  groupMembers <- makeGroupMembers(numGp, currentGroup)
  available <- makeAvailable(numGp, candidates)[[1]]
  #grpNum <- makeGrpNum(numGp)
  nAvailable <- length(available)
  while (TRUE) {
    if (isEmpty(grpNum)) {
      break
    }
    randomSample <- sample(available, numGp)
    available <-
      removeSelectedAnimalFromAvailableAnimals(available, randomSample, numGp)
    for (i in seq_along(randomSample)) {
      groupMembers[[i]] <- c(groupMembers[[i]], randomSample[i])
      # Remove all relatives from consideration for the group it was added to
      available[[i]] <- setdiff(available[[i]], kin[[randomSample[i]]])
    }

    grpNum <- removeGroupIfNoAvailableAnimals(grpNum, available)



      nPerGroup <- floor(nAvailable / numGp)
  leftOvers <- nAvailable - (numGp * numGp)
  for (i in 1:numGp) {
    first <- ((i - 1) * nPerGroup) + 1
    last <- i * nPerGroup
    groupMembers[[i]] <- setdiff(randomSample[first:last],
                                 unlist(sapply(randomSample[first:last],
                                        function(x){kin[x][[1]]})))
  }
  if (leftOvers < 0) {
    for (i in 1:leftOvers) {
      groupMembers[[i]] <- c(groupMembers[[i]], leftOvers[i])
    }
  }
  }
  groupMembers
}
