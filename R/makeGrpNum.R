#' Convenience function to make the initial grpNum list
#'
#' @return initial grpNum list
#'
#' @param numGp integer value indicating the number of groups that should be
#' formed from the list of IDs. Default is 1.
makeGrpNum <- function(numGp) {
  grpNum <- list()
  grpNum[1:numGp] <- 1:numGp
  grpNum
}
