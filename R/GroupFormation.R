# GroupFormation.R
# 2015-08-13



#################################################################################
#' Generating 1 or more new groups from a list of candidates
#' Function to find the largest group of unrelated animals that can be formed
#' from a set of candidate IDs. If more than 1 group is desired, the function
#' finds the set of groups with the largest average size.
#
#' The function implements a maximal independent set algorithm to find groups
#' of unrelated animals. A set of animals may have many different MISs of
#' varying sizes, and finding the largest would require traversing all possible
#' combinations of animals. Since this could be very time consuming, this
#' algorithm produces a random sample of the possible MISs, and selects from
#' these. The size of the random sample is determined by the specified number
#' of iterations.
#
#' Parameters
#' ----------
#' @param candidates character vector of IDs of the animals available for
#' use in the group.
#' @param kmat numberic matrix {row and column names: animal IDs} of
#' pairwise kinship values. Rows and columns are named with animal IDs.
#' @param ped datatable that is the `Pedigree`. It contains pedigree
#' information including the IDs listed in \code{candidates}.
#' @param threshold : float
#'   Minimum kinship level to be considered in group formation. Pairwise
#'   kinship below this level will be ignored.
#' @param ignore : list <char>
#'   List of sex combinations to be ignored. If provided, the vectors in the
#'   list specify if pairwise kinship should be ignored between certain sexes.
#'   Default is to ignore all pairwise kinship between females.
#' @param min.age
#'   Minimum age to consider in group formation. Pairwise kinships involving
#' @param iter integer indicating the number of times to perform the random
#' group formation process. Default value is 1000 iterations.
#' @param numGp : int
#'   Number of groups that should be formed from the list of IDs. Default is 1.
#' @param updateProgress function or NULL. If this function is defined, it
#' will be called during each iteration to update a
#' \code{shiny::Progress} object.
#
#' Return
#' ------
#' list {fields: group, score}
#'   "group" contains a list of the best group(s) produced during the
#'   simulation, while "score" provides the score associated with the
#'   group(s).

#' @export
groupAssign <- function(candidates, kmat, ped, threshold = 0.015625,
                        ignore = list(c("F", "F")), min.age = 1, iter = 1000,
                        numGp = 1, updateProgress = NULL) {
  kmat <- filterKinMatrix(candidates, kmat)
  kin <- reformatKinship(kmat)

  kin <- filterThreshold(kin, threshold = threshold)
  kin <- filterPairs(kin, ped, ignore = ignore)
  kin <- filterAge(kin, ped, min.age = min.age)

  # Filter out self kinships
  kin <- kin[(kin$id1 != kin$id2), ]

  # Converting the kinships to a list
  kin <- tapply(kin$id2, kin$id1, c)

  # adding animals with no relatives
  for (cand in setdiff(candidates, names(kin))) {
    kin[[cand]] <- c(NA)
  }

  # Starting the group assignment simulation:
  saved.score <- -1
  saved.gp <- list()

  for (k in 1:iter) {
    gp <- list()
    d <- list()
    for (i in 1:numGp) {
      gp[[i]] <- vector()
      d[[i]] <- candidates
    }

    g <- list()
    g[1:numGp] <- 1:numGp
    while (TRUE) {
      if (isEmpty(g)) {
        break
      }

      # Select a group at random
      i <- sample(g, 1)[[1]]

      # Select an animal that can be added to this group and add it
      id <- sample(d[[i]], 1)
      gp[[i]] <- c(gp[[i]], id)

      # Remove the selected animal from consideration
      for (j in 1:numGp) {
        d[[j]] <- setdiff(d[[j]], id)
      }

      # Remove all relatives from consideration for the group it was added to
      # need to modify "kin" to include blank entries for animals with no relatives
      d[[i]] <- setdiff(d[[i]], kin[[id]])

      remaining.g <- g
      for (i in remaining.g) {
        if (isEmpty(d[[i]])) {
          g <- setdiff(g, i)
        }
      }
    }

    # Score the resulting groups
    score <- min(sapply(gp, length))

    if (score > saved.score) {
      saved.gp <- gp
      saved.score <- score
    }

    # Updating the progress bar, if applicable
    if (!is.null(updateProgress)) {
      updateProgress()
    }
  }

  # Adding a group for the unused animals
  n <- length(saved.gp) + 1
  saved.gp[[n]] <- ifelse(isEmpty(setdiff(candidates, unlist(saved.gp))),
                          c(NA),
                          setdiff(candidates, unlist(saved.gp)))

  return(list(group = saved.gp, score = saved.score))
}
#' Adding animals to an existing group:
#'
#' Function to find the largest group that can be formed by adding unrelated
#' animals from a set of candidate IDs to an existing group.
#'
#' The function implements a maximal independent set algorithm to find groups
#' of unrelated animals. A set of animals may have many different MISs of
#' varying sizes, and finding the largest would require traversing all possible
#' combinations of animals. Since this could be very time consuming, this
#' algorithm produces a random sample of the possible MISs, and selects from
#' these. The size of the random sample is determined by the specified number
#' of iterations.
#'
#' Parameters
#' ----------
#' @param candidates character vector of IDs of the animals available for use
#'  in the group.
#' @param current.group character vector of IDs of animals currently assigned to the group.
#' @param kmat : matrix {row and column names: animal IDs}
#'   Matrix of pairwise kinship values. Rows and columns are named with
#'   animal IDs.
#' @param ped Dataframe of pedigree information including the IDs listed in
#' \code{candidates}.
#' @param threshold numeric value indicating the minimum kinship level to be
#' considered in group formation. Pairwise kinship below this level will be
#' ignored.
#' @param ignore list of character vectors representing the sex combinations
#' to be ignored. If provided, the vectors in the list specify if pairwise
#'  kinship should be ignored between certain sexes. Default is to ignore all
#'  pairwise kinship between females.
#' @param min.age integer value indicating the minimum age to consider in group
#' formation. Pairwise kinships involving an animal of this age or younger will
#'  be ignored. Default is 1 year.
#' @param iter integer value indicating number of times to perform the random
#' group formation process. Default is 1000 iterations.
#' @param updateProgress optional function (may be NULL) that is called
#' during each iteration to update a \code{shiny::Progress object}.
#'
#' @return a list with \code{group}, which contains a list of the best
#' group(s) produced during the simulation and \code{score}, which provides
#' the score associated with the group(s).
#' @export
groupAddition <- function(candidates, current.group, kmat, ped,
                          threshold = 0.015625, ignore = list(c("F", "F")),
                          min.age = 1, iter = 1000, updateProgress = NULL) {
  kmat <- filterKinMatrix(union(candidates, current.group), kmat)
  kin <- reformatKinship(kmat)

  kin <- filterThreshold(kin, threshold = threshold)
  kin <- filterPairs(kin, ped, ignore = ignore)
  kin <- filterAge(kin, ped, min.age = min.age)

  # Filter out self kinships
  kin <- kin[(kin$id1 != kin$id2), ]

  # Ignore kinship between current group members
  kin <- kin[!((kin$id1 %in% current.group) & (kin$id2 %in% current.group)), ]

  # Converting the kinships to a list
  kin <- tapply(kin$id2, kin$id1, c)

  # Filtering out candidates related to current group members
  conflicts <- unlist(kin[current.group])
  candidates <- setdiff(candidates, conflicts)

  # adding animals with no relatives
  for (cand in setdiff(candidates, names(kin))) {
    kin[[cand]] <- c(NA)
  }

  # Starting the group assignment simulation
  saved.score <- -1
  saved.gp <- list()

  for (k in 1:iter) {
    gp <- current.group
    d <- candidates

    while (TRUE) {
      if (isEmpty(d)) {
        break
      }

      # Select an animal that can be added to this group and add it
      id <- sample(d, 1)
      gp <- c(gp, id)

      # Remove the selected animal from consideration
      d <- setdiff(d, id)

      # Remove all relatives from consideration for the group it was added to
      d <- setdiff(d, kin[[id]])
    }

    # Score the resulting group
    score <- length(gp)

    if (score > saved.score) {
      saved.gp[[1]] <- gp
      saved.score <- score
    }

    # Updating the progress bar, if applicable
    if (!is.null(updateProgress)) {
      updateProgress()
    }
  }

  # Adding a group for the unused animals
  n <- length(saved.gp) + 1
  saved.gp[[n]] <- setdiff(candidates, unlist(saved.gp))

  return(list(group = saved.gp, score = saved.score))
}

#################################################################################
# Helper Functions:
#' Reformats a kinship matrix into a long-format table.
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
#' Filters kinship values less than the specified threshold from a long-format
#' table of kinship values.
#'
#' @param kin a dataframe with columns \code{id1}, \code{id2}, and
#' \code{kinship}. This is the kinship data reformatted from a matrix,
#' to a long-format table.
#' @param threshold numeric value representing the minimum kinship level to be
#' considered in group formation. Pairwise kinship below this level will be
#' ignored.
#' @export
filterThreshold <- function(kin, threshold = 0.015625) {
  kin <- kin[kin$kinship >= threshold, ]
  rownames(kin) <- 1:nrow(kin)
  return(kin)
}
#' Filters kinship values from a long-format kinship table based on the sexes
#'  of the two animals involved.
#'
#' @param kin a dataframe with columns \code{id1}, \code{id2}, and
#' \code{kinship}. This is the kinship data reformatted from a matrix,
#' to a long-format table.
#'
#' @export
filterPairs <- function(kin, ped, ignore = list(c("F", "F"))) {
  # Parameters
  # ----------
  # kin : data.frame {fields: id1, id2, kinship}
  #   Long-format kinship table.
  # ped : `Pedigree`
  #   Table of pedigree information.
  # ignore : list <char>
  #   List of sex pairs for which kinship should be ignored.
  #
  # Return
  # ------
  # data.frame
  #   Filtered long-format kinship table.

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
    k <- !(((g1 == rel[1]) & (g2 == rel[2])) | ((g1 == rel[2]) & (g2 == rel[1])))
    keep <- keep & k
  }
  kin$sort.col <- NULL
  kin <- kin[keep, ]
  rownames(kin) <- 1:nrow(kin)
  return(kin)
}

#' Removes kinship values where one animal is less than the min.age
#'
#' @param kin
#' @param ped : `Pedigree`
#'   Dataframe of pedigree information including the IDs listed in "candidates".
#' @param min.age numeric value representing minimum years of age of
#' animals to retain.
#' @export
filterAge <- function(kin, ped, min.age = 1) {
  kin$sort.col <- 1:nrow(kin)

  a1 <- merge(kin, ped, by.x = "id1", by.y = "id", all.x = TRUE, all.y = FALSE)
  a2 <- merge(kin, ped, by.x = "id2", by.y = "id", all.x = TRUE, all.y = FALSE)

  a1 <- a1[with(a1, order(sort.col)), "age"]
  a2 <- a2[with(a2, order(sort.col)), "age"]

  keep <- (((a1 >= min.age) | is.na(a1)) & ((a2 >= min.age) | is.na(a2)))

  kin$sort.col <- NULL
  kin <- kin[keep, ]
  rownames(kin) <- 1:nrow(kin)
  return(kin)
}

