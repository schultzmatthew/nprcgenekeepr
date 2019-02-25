#' Takes list of currentGroups and removes all empty vectors
#'
#' Part of Group Formation
#'
#' @description{
#' \code{compactCurrentGroup} takes the list \code{currentGroups} and removes
#' all empty character vectors. This allows the user to place seed animals
#' in current groups without regard to order.
#' }
#' @return A version of \code{currentGroups} with empty character vectors
#' removed. If no character vectors remain \code{currentGroups} gets defined
#' as \code{character(0)}.

#' @param currentGroups list of character vectors of IDs of animals currently assigned
#' to the group. Defaults to character(0) assuming no groups are existant.
compactCurrentGroups <- function(currentGroups = character(0)) {
  if (length(currentGroups) > 0) {
    i <- 1
    for (currentGroup in currentGroups) {
      if (length(currentGroup) < 1) {
        currentGroups[[i]] <- NULL
      } else {
        i <- i + 1
      }
    }
  }
  if (length(currentGroups) == 0)
    currentGroups <- character(0)

  currentGroups
}
