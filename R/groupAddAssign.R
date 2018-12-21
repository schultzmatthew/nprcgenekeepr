#' Add animals to an existing breeding group or form groups:
#'
#' Part of Group Formation
#'
#' @description{
#' \code{groupAddAssign} finds the largest group that can be formed by adding
#' unrelated animals from a set of candidate IDs to an existing group, to a new
#' group it has formed from a set of candidate IDs or if more than 1 group
#' is desired, it finds the set of groups with the largest average size.
#'
#' The function implements a maximal independent set (MIS) algorithm to find
#' groups of unrelated animals. A set of animals may have many different MISs of
#' varying sizes, and finding the largest would require traversing all possible
#' combinations of animals. Since this could be very time consuming, this
#' algorithm produces a random sample of the possible MISs, and selects from
#' these. The size of the random sample is determined by the specified number
#' of iterations.
#'
#' }
#' @return A list with list items \code{group}, \code{score} and optionally
#' \code{groupKin}.
#' The list item \code{group} contains a list of the best group(s) produced
#' during the simulation.
#' The list item \code{score} provides the score associated with the group(s).
#' The list item \code{groupKin} contains the subset of the kinship matrix
#' that is specific for each group formed.
#'
#' @param candidates character vector of IDs of the animals available for
#' use in the group.
#' @param currentGroup character vector of IDs of animals currently assigned
#' to the group. Defaults to NULL assuming no groups are existant.
#' @param kmat numeric matrix of pairwise kinship values. Rows and columns
#' are named with animal IDs.
#' @param ped dataframe that is the `Pedigree`. It contains pedigree
#' information including the IDs listed in \code{candidates}.
#' @param threshold numeric value indicating the minimum kinship level to be
#' considered in group formation. Pairwise kinship below this level will be
#' ignored.
#' @param ignore list of character vectors representing the sex combinations
#' to be ignored. If provided, the vectors in the list specify if pairwise
#' kinship should be ignored between certain sexes.
#' Default is to ignore all pairwise kinship between females.
#' @param minAge integer value indicating the minimum age to consider in group
#' formation. Pairwise kinships involving an animal of this age or younger will
#'  be ignored. Default is 1 year.
#' @param iter integer indicating the number of times to perform the random
#' group formation process. Default value is 1000 iterations.
#' @param numGp integer value indicating the number of groups that should be
#' formed from the list of IDs. Default is 1.
#' @param updateProgress function or NULL. If this function is defined, it
#' will be called during each iteration to update a
#' \code{shiny::Progress} object.
#' @param withKin logical variable when set to \code{TRUE} the kinship
#' matrix for the group is returned along with the group and score.
#' Defaults to not return the kinship matrix. This maintains compatability with
#' earlier versions.
#'
#' @export
groupAddAssign <- function(candidates, currentGroup = NULL, kmat, ped,
                            threshold = 0.015625, ignore = list(c("F", "F")),
                            minAge = 1, iter = 1000,
                            numGp = 1, updateProgress = NULL,
                            withKin = FALSE) {
  if (!is.null(currentGroup) && numGp > 1)
    stop("Cannot have more than one group formed when adding to a single group")
  kmat <- kinMatrix2LongForm(union(candidates, currentGroup), kmat)
  kin <- kinMatrix2LongForm(kmat)

  kin <- filterThreshold(kin, threshold = threshold)
  kin <- filterPairs(kin, ped, ignore = ignore)
  kin <- filterAge(kin, ped, minAge = minAge)

  # Filter out self kinships
  kin <- kin[(kin$id1 != kin$id2), ]

  # Ignore kinship between current group members
  kin <- kin[!((kin$id1 %in% currentGroup) & (kin$id2 %in% currentGroup)), ]

  # Converting the kinships to a list
  kin <- tapply(kin$id2, kin$id1, c)

  # Filtering out candidates related to current group members
  conflicts <- unique(c(unlist(kin[currentGroup]), currentGroup))
  candidates <- setdiff(candidates, conflicts)

  # adding animals with no relatives
  for (cand in setdiff(candidates, names(kin))) {
    kin[[cand]] <- c(NA)
  }

  # Starting the group assignment simulation
  saved.score <- -1
  saved.groupMembers <- list()

  for (k in 1:iter) {
    groupMembers <- list()
    available <- list()
    for (i in 1:numGp) {
      if (is.null(currentGroup)) {
        groupMembers[[i]] <- vector()
      } else {
        groupMembers[[i]] <- currentGroup
      }
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
      if (is.null(currentGroup)) {
        saved.groupMembers <- groupMembers
      } else {
        saved.groupMembers[[1]] <- groupMembers
      }
      saved.score <- score
    }

    # Updating the progress bar, if applicable
    if (!is.null(updateProgress)) {
      updateProgress()
    }
  }

  # Adding a group for the unused animals
  n <- length(saved.groupMembers) + 1
  saved.groupMembers[[n]] <-
    ifelse(isEmpty(setdiff(candidates, unlist(saved.groupMembers))),
           c(NA), list(setdiff(candidates, unlist(saved.groupMembers))))[[1]]

  if (withKin) {
    groupKin <- list()
    for (i in seq_along(saved.groupMembers)) {
      groupKin[[i]] <-   filterKinMatrix(saved.groupMembers[[i]], kmat)
    }
    return(list(group = saved.groupMembers, score = saved.score,
                groupKin = groupKin))
  } else {
    return(list(group = saved.groupMembers, score = saved.score))
  }
}
