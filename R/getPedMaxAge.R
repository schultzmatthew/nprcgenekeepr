#' Get the maximum age of live animals in the pedigree.
#'
#' @param ped dataframe with pedigree
#' @export
getPedMaxAge <- function(ped) {
  max(ped$age, na.rm = TRUE)
}
