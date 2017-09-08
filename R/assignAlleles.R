#' Assign parent alleles randomly
#'
#' @param parentType character vector of length one with value of
#' \code{"sire"} or \code{"dam"}.
#' @param parent either \code{ped[id, "sire"]} or \code{ped[id, "dam"]}.
#' @param alleles list of list containing the alleles for each individual's
#'  sire and dam that have been assigned thus far
#' @param id character vector of length one containing the animal ID
#' @param a integer value that is a counter. This function updates \code{a} in
#' the calling environment.
#' @param n integer indicating the number of iterations to simulate.
#' Default is 5000.
#' @export
assignAlleles <- function(parentType, parent, alleles, id, a, n) {
  if (is.na(parent)) {
    # If the parent is unknown, create a unique set of alleles for him or her
    alleles[[id]][[parentType]] <- rep(a, n)
    a <<- a + 1L
  } else {
    # Otherwise get his two sets of alleles and randomly select one
    # for each iteration
    p1 <- alleles[[parent]][["sire"]]
    p2 <- alleles[[parent]][["dam"]]
    alleles[[id]][[parentType]] <- chooseAlleles(p1, p2)
  }
  alleles
}
