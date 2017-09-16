#' Filters a kinship matrix to include only the egos listed in 'ids'
#'
#' @param ids character vector containing the IDs of interest.
#' The kinship matrix should be reduced to only include these rows and columns.
#' @param kmat a numeric matrix of pairwise kinship coefficients.
#' Rows and columns should be named with IDs.
#'
#' @return A numeric matrix that is the reduced kinship matrix with named
#' rows and columns (row and col names are 'ids')
#' @export
filterKinMatrix <- function(ids, kmat) {
  return(kmat[(rownames(kmat) %in% ids), (colnames(kmat) %in% ids)])
}
