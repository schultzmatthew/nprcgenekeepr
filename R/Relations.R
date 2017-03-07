# Relations.R
# 2015-08-31

# Contains functions to calculate the first-order relationships in a pedigree,
# and to convert pairwise kinships to the appropriate relationship category.

# Relationships categories:
# For each ID in the pair, find a CEPH-style pedigree and compare them
#   If one is the parent of the other:
#     Designate the relationship as 'parent-offspring'
#		Else if both parents are shared:
#     Designate the relationship as 'full-siblings'
#		Else if one parent is shared:
#     Designate the relationship as 'half-siblings'
#		Else if one is the grandparent of the other:
#     Designate the relationship as 'grandparent-grandchild'
#		Else if both grand parents are shared:
#     Designate the relationship as 'cousin'
#		Else if at least one grand parent is shared:
#     Designate the relationship as 'cousin - other'
#		Else if the parents of one are the grandparents of the other:
#     Designate the relationship as 'full-avuncular'
#   Else if a single parent of one is the grandparent of the other:
#     Designate the relationship as 'avuncular - other'
#		Else if the kinship is greater than 0, but the pair don't fall into the above categories:
#     Designate the relationship as 'other'
#   Else:
#     Designate the relationships as 'no relation'

###############################################################################
#' Make a CEPH-style pedigree for each id
#'
#' Creates a CEPH-style pedigree for each id, consisting of three generations:
#' the id, the parents, and the grandparents. Inserts NA for unknown pedigree
#' members.
#'
#' @param id character vector of IDs for a set of animals.
#' @param id character vector with unique identifier for an individual
#' @param sire character vector with unique identifier for an
#' individual's father (\code{NA} if unknown).
#' @param dam character vector with unique identifier for an
#' individual's mother (\code{NA} if unknown).
#'
#' @return list of lists {fields: id, {subfields: parents, pgp, mgp}}
#' Pedigree information converted into a CEPH-style list. The top level
#' list elements are the IDs from id. Below each ID is a list of three
#' elements: parents (sire, dam), paternal grandparents (pgp: sire, dam),
#' and maternal grandparents (mgp: sire, dam).


makeCEPH <- function(id, sire, dam) {
  ped <- data.frame(sire = sire, dam = dam, row.names = id,
                    stringsAsFactors = FALSE)

  ceph <- list()
  for (i in id) {
    sire <- ped[i, "sire"]
    dam <- ped[i, "dam"]
    parents <- c(sire, dam)

    if (is.na(sire)) {
      pgp <- c(NA, NA)
    } else{
      pgp <- c(ped[sire, "sire"], ped[sire, "dam"])
    }

    if (is.na(dam)) {
      mgp <- c(NA, NA)
    } else{
      mgp <- c(ped[dam, "sire"], ped[dam, "dam"])
    }

    ceph[[i]] <- list(parents = parents, pgp = pgp, mgp = mgp)
  }

  return(ceph)
}

countFirstOrder <- function(ped, ids = NULL) {
  # Tallies the number of first-order relatives for each member of the provided
  # pedigree. If 'ids' is provided, the analysis is restricted to only the
  # specified subset.
  #
  # Parameters
  # ----------
  # ped : `Pedigree`
  #   Standardized pedigree information in a table.
  # ids : vector <char> or NULL
  #   List of IDs to which the analysis should be restricted. First-order
  #   relationships will only be tallied for the listed IDs and will only
  #   consider relationships within the subset. If NULL, the analysis will
  #   include all IDs in the pedigree.
  #
  # Return
  # ------
  # data.frame {fields: id, parents, offspring, siblings, total}
  #   A table of first-order relationship counts, broken down to indicate
  #   the number of parents, offspring, and siblings that are part of the
  #   subset under consideration.

  if (!is.null(ids)) {
    ped <- ped[ped$id %in% ids, ]
  }
  rownames(ped) <- 1:nrow(ped)
  parents <- c()
  offspring <- c()
  siblings <- c()

  for (i in 1:nrow(ped)) {
    id <- ped[i, "id"]
    sire <- ped[i, "sire"]
    dam <- ped[i, "dam"]

    p <- sum(c((sire %in% ped$id), (dam %in% ped$id)))
    o <- sum((ped$sire %in% id) | (ped$dam %in% id))
    if (is.na(sire) | is.na(dam)) {
      s <- 0
    } else{
      s <- sum((ped$sire %in% sire) & (ped$dam %in% dam)) - 1
    }

    parents <- c(parents, p)
    offspring <- c(offspring, o)
    siblings <- c(siblings, s)
  }
  total <- parents + offspring + siblings
  ped["parents"] <- parents
  ped["offspring"] <- offspring
  ped["siblings"] <- siblings
  ped["total"] <- total

  return(ped[, c("id", "parents", "offspring", "siblings", "total")])
}

# Converts pairwise kinship values to a relationship category descriptor.
#
# Parameters
# ----------
# kmat : matrix
#   Matrix of pairwise kinship coefficients. Rows and columns
#   should be named with IDs.
# ped : `Pedigree`
#   Standardized pedigree information in a table.
# ids : vector <char> or NULL
#   List of IDs to which the analysis should be restricted. If provided,
#   only relationships between these IDs will be converted to categories.
# updateProgress : function or NULL
#   Function that can be called during each iteration to update a
#   shiny::Progress object.
#
# Return
# ------
# data.frame {fields: id1, id2, kinship, relation}
#   Long-form table of pairwise kinships, with relationship categories
#   included for each pair.

convertRelationships <- function(kmat, ped, ids = NULL, updateProgress = NULL) {
  if (!is.null(ids)) {
    kmat <- filterKinMatrix(ids, kmat)
  }
  kin <- reformatKinship(kmat, rm.dups = TRUE)
  ped <- makeCEPH(ped$id, ped$sire, ped$dam)
  r <- c()

  for (i in 1:nrow(kin)) {
    id1 <- kin$id1[i]
    id2 <- kin$id2[i]

    ceph1 <- ped[[id1]]
    ceph2 <- ped[[id2]]

    if (id1 == id2) {
      relation <- "Self"
    } else if (entire(ceph1$parents == ceph2$parents)) {
      relation <- "Full-Siblings"
    } else if (id1 %in% ceph2$parents | id2 %in% ceph1$parents) {
      # one animal is the parent of the other
      relation <- "Parent-Offspring"
    } else if (!isEmpty(intersect(ceph1$parents, ceph2$parents))) {
      # at least 1 parent is shared
      relation <- "Half-Siblings"
    } else if (id1 %in% c(ceph2$pgp, ceph2$mgp) |
               id2 %in% c(ceph1$pgp, ceph1$mgp)) {
      # one animals is the grandparent of the other
      relation <- "Grandparent-Grandchild"
    } else if (entire(ceph1$pgp == ceph2$pgp) |
               entire(ceph1$pgp == ceph2$mgp) |
               entire(ceph1$mgp == ceph2$pgp) |
               entire(ceph1$mgp == ceph2$mgp)) {
      # When a full set of grandparents are shared
      relation <- "Full-Cousins"
    } else if (!isEmpty(intersect(c(ceph1$pgp, ceph1$mgp),
                                 c(ceph2$pgp, ceph2$mgp)))) {
      # When at least one grandparent is in common
      relation <- "Cousin - Other"
    } else if (entire(ceph1$parents == ceph2$pgp) |
              entire(ceph1$parents == ceph2$mgp) |
              entire(ceph2$parents == ceph1$pgp) |
              entire(ceph2$parents == ceph1$mgp)) {
      # When parents of one proband are the grandparents of the other
      relation <- "Full-Avuncular"
    } else if (!isEmpty(intersect(ceph1$parents, c(ceph2$pgp, ceph2$mgp))) |
              !isEmpty(intersect(ceph2$parents, c(ceph1$pgp, ceph1$mgp)))) {
      # When at least one parent of a proband is the grandparent of the other
      relation <- "Avuncular - Other"
    } else if (kin$kinship[i] > 0) {
      relation <- "Other"
    } else{
      relation <- "No Relation"
    }

    r <- c(r, relation)

    if (!is.null(updateProgress)) {
      updateProgress()
    }
  }
  kin["relation"] <- r
  return(kin)
}
#' Returns \code{TRUE} if every member of the vector is \code{TRUE}.
#'
#' Considers NA values the same as false
#' @param v logical vector
#' @export
entire <- function(v) {
  v <- all(v)
  v <- if (is.na(v)) FALSE else v
  return(v)
}

relationClasses <- function(kin) {
  #
  # kin : data.frame {fields: id1, id2, kinship, relation}
  #
  #
  #

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

###############################################################################


# filterKinDuplicates <- function(kin) {
#   # data.frame {id1, id2, kinship}
#   pairs <- c()
#   keep <- c()
#
#   for (i in 1:nrow(kin)) {
#     pair <- paste(kin$id1[i], kin$id2[i], sep = "")
#     rev.pair <- paste(kin$id2[i], kin$id1[i], sep = "")
#
#     if (!(rev.pair %in% pairs)) {
#       pairs <- c(pairs, pair)
#       keep <- c(keep, i)
#     }
#   }
#
#   return(kin[keep, ])
# }



###############################################################################

