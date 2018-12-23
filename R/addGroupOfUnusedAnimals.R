#' addGroupOfUnusedAnimals adds a group to the saved groups if needed
#'
#' @return list of groups, which are each lists of animal Ids.
#'   unused animals at the end of the iteration.
#'
#' @param savedGroupMembers list of groups of animals in the form of a vector
#' of animal Ids.
#' @param candidates character vector of IDs of the animals available for
#' use in the group.
#'
#' @export
addGroupOfUnusedAnimals <- function(savedGroupMembers, candidates) {
  # Adding a group for the unused animals
  n <- length(savedGroupMembers) + 1
  savedGroupMembers[[n]] <-
    ifelse(isEmpty(setdiff(candidates, unlist(savedGroupMembers))),
           c(NA), list(setdiff(candidates, unlist(savedGroupMembers))))[[1]]
  savedGroupMembers
}
