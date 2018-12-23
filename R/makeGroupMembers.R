#' Convenience function to make the initial groupMembers animal list
#'
#' @return initial groupMembers list
#'
#' @param candidates character vector of IDs of the animals available for
#' use in the group.
#' @param numGp integer value indicating the number of groups that should be
#' formed from the list of IDs. Default is 1.
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
