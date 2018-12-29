#' Conveniece function to update the groupMembers list
#'
#' @return Updated savedGroupMembers list
#' @param savedGroupMembers list of animals in best group thus far. Initialized
#' with \code{list()}.
#' @param currentGroup character vector of IDs of animals currently assigned
#' to the group. Defaults to NULL assuming no groups are existant.
#' @param groupMembers list of animals in the group at the current iteration
updateSavedGroupMembers <- function(savedGroupMembers, currentGroup,
                                    groupMembers) {

  if (is.null(currentGroup) | length(currentGroup) < 1) {
    savedGroupMembers <- groupMembers
  } else {
    savedGroupMembers[[1]] <- groupMembers
  }
  savedGroupMembers
}
