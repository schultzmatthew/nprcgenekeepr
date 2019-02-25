#' Conveniece function to update the groupMembers list
#'
#' @return Updated savedGroupMembers list
#' @param savedGroupMembers list of animals in best group thus far. Initialized
#' with \code{list()}.
#' @param currentGroups list of character vectors of IDs of animals currently assigned
#' to the group. Defaults to character(0) assuming no groups are existant.
#' @param groupMembers list of animals in the group at the current iteration
updateSavedGroupMembers <- function(savedGroupMembers, currentGroups,
                                    groupMembers) {

  if (is.null(currentGroups) | length(currentGroups) < 1) {
    savedGroupMembers <- groupMembers
  } else {
    savedGroupMembers[[1]] <- groupMembers
  }
  savedGroupMembers
}
