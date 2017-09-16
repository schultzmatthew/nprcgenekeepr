#' Calculates the averages kinship for each animal in a kinship matrix
#'
#' @param kmat a numeric matrix of pairwise kinship coefficients.
#' Animal IDs are the row and column names.
#'
#' @return A named numeric vecter of average kinship coefficients for each
#' animal ID. Elements are named with the IDs from the columns of kmat.
#' @export
avgKinship <- function(kmat) {
  return(colMeans(kmat, na.rm = TRUE))
}
