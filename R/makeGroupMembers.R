#' Convenience function to make the initial groupMembers animal list
#'
#' @return initial groupMembers list
#'
#' @param numGp integer value indicating the number of groups that should be
#' formed from the list of IDs. Default is 1.
#' @param currentGroup character vector of IDs of animals currently assigned
#' to the group. Defaults to NULL assuming no groups are existant.
makeGroupMembers <- function(numGp, currentGroup) {
  groupMembers <- list()
  for (i in 1:numGp) {
    if (is.null(currentGroup)) {
      groupMembers[[i]] <- vector()
    } else {
      groupMembers[[i]] <- currentGroup
    }
  }
  groupMembers
}
