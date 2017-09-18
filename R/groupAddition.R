#' Add animals to an existing breeding group:
#'
#' Part of Group Formation
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
#' @param currentGroup character vector of IDs of animals currently assigned
#' to the group.
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
