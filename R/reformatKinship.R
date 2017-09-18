#' Reformats a kinship matrix into a long-format table.
#'
#' Part of Group Formation
#'
#' @param kin_matrix numerical matrix of pairwise kinship values. The row and
#' column names correspond to animal IDs.
#' @param rm.dups locigal value indication whether or not reverse-order ID
#' pairs be filtered out? (i.e., "ID1 ID2 kin_val" and "ID2 ID1 kin_val" will
#' be collapsed into a single entry if rm.dups = TRUE)
#'
#' @return dataframe with columns \code{id1}, \code{id2}, and \code{kinship}.
#' This is the kinship data reformatted from a matrix, to a long-format table.
#' @importFrom utils stack
#' @export
reformatKinship <- function(kin_matrix, rm.dups = FALSE) {
  if (rm.dups) {
    kin_matrix[upper.tri(kin_matrix)] <- NA
  }

  kmat <- as.data.frame(kin_matrix)
  k <- stack(kmat)
  k["id2"] <- row.names(kmat)

  colnames(k) <- c("kinship", "id1", "id2")
  k$id1 <- as.character(k$id1)

  k <- k[!is.na(k$kinship), ]

  return(k[, c("id1", "id2", "kinship")])
}
