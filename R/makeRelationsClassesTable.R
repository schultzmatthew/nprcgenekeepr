#' Make relation classes table from \code{kin} dataframe.
#'
#' From Relations
#'
#' kin : data.frame {fields: id1, id2, kinship, relation}
#'
#' @param kin a dataframe with columns \code{id1}, \code{id2}, \code{kinship},
#' and \code{relation}. It is a long-form table of pairwise kinships, with
#' relationship categories included for each pair.
#' @export
makeRelationClasseTable <- function(kin) {
  rel.class <- c("Self", "Parent-Offspring", "Full-Siblings", "Half-Siblings",
                 "Grandparent-Grandchild", "Full-Cousins", "Cousin - Other",
                 "Full-Avuncular", "Avuncular - Other", "Other", "No Relation")

  kin <- kin[kin$relation != "Self", ]
  r <- as.data.frame(table(kin$relation))
  colnames(r) <- c("Relationship Class", "Frequency")
  #r$Frequency <- r$Frequency/2

  rel.class <- rel.class[rel.class %in% r[, "Relationship Class"]]
  return(r[match(rel.class, r[, "Relationship Class"]),])
}
