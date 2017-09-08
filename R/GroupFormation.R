# GroupFormation.R
# 2015-08-13



#################################################################################
#' Assign breeding groups
#'
#' Generating  from a list of candidates.
#' \code{groupAssign} finds either the largest group of unrelated animals
#' that can be formed from a set of candidate IDs or if more than 1 group is
#' desired, it finds the set of groups with the largest average size.
#'
#' The function implements a maximal independent set (MIS) algorithm to find
#' groups of unrelated animals. A set of animals may have many different MISs of
#' varying sizes, and finding the largest would require traversing all possible
#' combinations of animals. Since this could be very time consuming, this
#' algorithm produces a random sample of the possible MISs, and selects from
#' these. The size of the random sample is determined by the specified number
#' of iterations.
#'
#' @param candidates character vector of IDs of the animals available for
#' use in the group.
#' @param kmat numeric matrix {row and column names: animal IDs} of
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
#' @param withKin logical varialbe when set to \code{TRUE} the kinship
#' matrix for the group is returned along with the group and score.
#' Defaults to not return the kinship matrix. This maintains compatability with
#' earlier versions.
#
#' @return A list with fields \code{group} and \code{score}.
#'   The field \code{group} contains a list of the best group(s) produced
#'   during the simulation, while the field \code{score} provides the score
#'   associated with the group(s).
#' @export
groupAssign <- function(candidates, kmat, ped, threshold = 0.015625,
                        ignore = list(c("F", "F")), min.age = 1, iter = 1000,
                        numGp = 1, updateProgress = NULL, withKin = FALSE) {
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
  saved.groupMembers <- list()

  for (k in 1:iter) {
    groupMembers <- list()
    available <- list()
    for (i in 1:numGp) {
      groupMembers[[i]] <- vector()
      available[[i]] <- candidates
    }

    grpNum <- list()
    grpNum[1:numGp] <- 1:numGp
    while (TRUE) {
      if (isEmpty(grpNum)) {
        break
      }

      # Select a group at random
      i <- sample(grpNum, 1)[[1]]

      # Select an animal that can be added to this group and add it
      id <- sample(available[[i]], 1)
      groupMembers[[i]] <- c(groupMembers[[i]], id)

      # Remove the selected animal from consideration
      for (j in 1:numGp) {
        available[[j]] <- setdiff(available[[j]], id)
      }

      # Remove all relatives from consideration for the group it was added to
      # need to modify "kin" to include blank entries for animals with no
      # relatives
      available[[i]] <- setdiff(available[[i]], kin[[id]])

      remainingGrpNum <- grpNum
      for (i in remainingGrpNum) {
        if (isEmpty(available[[i]])) {
          grpNum <- setdiff(grpNum, i)
        }
      }
    }

    # Score the resulting groups
    score <- min(sapply(groupMembers, length))

    if (score > saved.score) {
      saved.groupMembers <- groupMembers
      saved.score <- score
    }

    # Updating the progress bar, if applicable
    if (!is.null(updateProgress)) {
      updateProgress()
    }
  }

  # Adding a group for the unused animals
  n <- length(saved.groupMembers) + 1
  saved.groupMembers[[n]] <- ifelse(isEmpty(setdiff(candidates, unlist(saved.groupMembers))),
                          c(NA),
                          setdiff(candidates, unlist(saved.groupMembers)))
  if (withKin) {
    groupKin <- list()
    for (i in seq_along(saved.groupMembers)) {
      groupKin[[i]] <-   filterKinMatrix(saved.groupMembers[[i]], kmat)
    }
    return(list(group = saved.groupMembers, score = saved.score, groupKin = groupKin))
  } else {
    return(list(group = saved.groupMembers, score = saved.score))
  }
}
#' Add animals to an existing breeding group:
#'
#' Function to find the largest group that can be formed by adding unrelated
#' animals from a set of candidate IDs to an existing group.
#'
#' The function implements a maximal independent set (MIS) algorithm to find
#' groups of unrelated animals. A set of animals may have many different MISs of
#' varying sizes, and finding the largest would require traversing all possible
#' combinations of animals. Since this could be very time consuming, this
#' algorithm produces a random sample of the possible MISs, and selects from
#' these. The size of the random sample is determined by the specified number
#' of iterations.
#'
#' @param candidates character vector of IDs of the animals available for use
#'  in the group.
#' @param currentGroup character vector of IDs of animals currently assigned to the group.
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
#' @param withKin logical varialbe when set to \code{TRUE} the kinship
#' matrix for the group is returned along with the group and score.
#' Defaults to not return the kinship matrix. This maintains compatability with
#' earlier versions.
#' @return a list with \code{group}, which contains a list of the best
#' group(s) produced during the simulation and \code{score}, which provides
#' the score associated with the group(s).
#' @export
groupAddition <- function(candidates, currentGroup, kmat, ped,
                          threshold = 0.015625, ignore = list(c("F", "F")),
                          min.age = 1, iter = 1000, updateProgress = NULL,
                          withKin = FALSE) {
  kmat <- filterKinMatrix(union(candidates, currentGroup), kmat)
  kin <- reformatKinship(kmat)

  kin <- filterThreshold(kin, threshold = threshold)
  kin <- filterPairs(kin, ped, ignore = ignore)
  kin <- filterAge(kin, ped, min.age = min.age)

  # Filter out self kinships
  kin <- kin[(kin$id1 != kin$id2), ]

  # Ignore kinship between current group members
  kin <- kin[!((kin$id1 %in% currentGroup) & (kin$id2 %in% currentGroup)), ]

  # Converting the kinships to a list
  kin <- tapply(kin$id2, kin$id1, c)

  # Filtering out candidates related to current group members
  conflicts <- unlist(kin[currentGroup])
  candidates <- setdiff(candidates, conflicts)

  # adding animals with no relatives
  for (cand in setdiff(candidates, names(kin))) {
    kin[[cand]] <- c(NA)
  }

  # Starting the group assignment simulation
  saved.score <- -1
  saved.groupMembers <- list()

  for (k in 1:iter) {
    groupMembers <- currentGroup
    d <- candidates

    while (TRUE) {
      if (isEmpty(d)) {
        break
      }

      # Select an animal that can be added to this group and add it
      id <- sample(d, 1)
      groupMembers <- c(groupMembers, id)

      # Remove the selected animal from consideration
      d <- setdiff(d, id)

      # Remove all relatives from consideration for the group it was added to
      d <- setdiff(d, kin[[id]])
    }

    # Score the resulting group
    score <- length(groupMembers)

    if (score > saved.score) {
      saved.groupMembers[[1]] <- groupMembers
      saved.score <- score
    }

    # Updating the progress bar, if applicable
    if (!is.null(updateProgress)) {
      updateProgress()
    }
  }

  # Adding a group for the unused animals
  n <- length(saved.groupMembers) + 1
  saved.groupMembers[[n]] <- setdiff(candidates, unlist(saved.groupMembers))
  if (withKin) {
    groupKin <- list()
    for (i in seq_along(saved.groupMembers)) {
      groupKin[[i]] <-   filterKinMatrix(saved.groupMembers[[i]], kmat)
    }
    return(list(group = saved.groupMembers, score = saved.score, groupKin = groupKin))
  } else {
    return(list(group = saved.groupMembers, score = saved.score))
  }
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
  if (nrow(kin) > 0)
    rownames(kin) <- 1:nrow(kin)
  return(kin)
}
#' Filters kinship values from a long-format kinship table based on the sexes
#'  of the two animals involved.
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
    k <- !(((g1 == rel[1]) & (g2 == rel[2])) | ((g1 == rel[2]) & (g2 == rel[1])))
    keep <- keep & k
  }
  kin$sort.col <- NULL
  kin <- kin[keep, ]
  if (nrow(kin) > 0)
    rownames(kin) <- 1:nrow(kin)
  return(kin)
}
#' Removes kinship values where one animal is less than the min.age
#'
#' @param kin a dataframe with columns \code{id1}, \code{id2}, and
#' \code{kinship}. This is the kinship data reformatted from a matrix,
#' to a long-format table.
#' @param ped dataframe of pedigree information including the IDs listed
#' in "candidates".
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
  if (nrow(kin) > 0)
    rownames(kin) <- 1:nrow(kin)
  return(kin)
}

