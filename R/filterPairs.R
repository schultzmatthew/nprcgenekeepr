#' Filters kinship values from a long-format kinship table based on the sexes
#'  of the two animals involved.
#'
#' Part of Group Formation
#'
#' @param kin a dataframe with columns \code{id1}, \code{id2}, and
#' \code{kinship}. This is the kinship data reformatted from a matrix,
#' to a long-format table.
#' @param ped Dataframe of pedigree information including the IDs listed in
#' \code{candidates}.
#' @param ignore a list coontaining zero or more character vectors of length 2
#' indicating which sex pairs should be ignored with regard to kinship.
#' Defaults to \code{list(c("F", "F"))}.
#'
#' @return a dataframe representing a filtered long-format kinship table.
#' @export
filterPairs <- function(kin, ped, ignore = list(c("F", "F"))) {

  if (length(ignore) == 0) {
    return(kin)
  }
  kin["sort.col"] <- 1:nrow(kin)

  g1 <- merge(kin, ped, by.x = "id1", by.y = "id", all.x = TRUE, all.y = FALSE)
  g2 <- merge(kin, ped, by.x = "id2", by.y = "id", all.x = TRUE, all.y = FALSE)

  g1 <- g1[with(g1, order(sort.col)), "sex"]
  g2 <- g2[with(g2, order(sort.col)), "sex"]

  keep <- rep(TRUE, length(g1))

  for (i in 1:length(ignore)) {
    rel <- ignore[[i]]
    k <- !( ( (g1 == rel[1]) & (g2 == rel[2])) | ( (g1 == rel[2]) &
                                                  (g2 == rel[1])))
    keep <- keep & k
  }
  kin$sort.col <- NULL
  kin <- kin[keep, ]
  if (nrow(kin) > 0)
    rownames(kin) <- 1:nrow(kin)
  return(kin[!is.na(kin[[1]]), ])
}
