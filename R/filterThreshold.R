#' Filters kinship to remove rows with kinship values less than the specified
#' threshold
#'
#' Part of Group Formation
#' Filters kinship values less than the specified threshold from a long-format
#' table of kinship values.
#' @param kin a dataframe with columns \code{id1}, \code{id2}, and
#' \code{kinship}. This is the kinship data reformatted from a matrix,
#' to a long-format table.
#' @param threshold numeric value representing the minimum kinship level to be
#' considered in group formation. Pairwise kinship below this level will be
#' ignored.
#' @export
filterThreshold <- function(kin, threshold = 0.015625) {
  kin <- kin[kin$kinship >= threshold, ]
  if (nrow(kin) > 0)
    rownames(kin) <- 1:nrow(kin)
  return(kin)
}
