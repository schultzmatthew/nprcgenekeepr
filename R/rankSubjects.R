#' Ranks animals based on genetic value.
#'
#' Part of Genetic Value Analysis
#' Adds a column to \code{rpt} containing integers from 1 to nrow, and provides
#' a value designation for each animal of "high value" or "low value"
#'
#' @param rpt a list of data.frame {req. colnames: value} containing genetic
#' value data for the population. Dataframes separate out those animals that
#' are imports, those that have high genome uniqueness (gu > 10%), those that
#' have low mean kinship (mk < 0.25), and the remainder.
#'
#' @return A list of dataframes with value and ranking information added.
#' @export
rankSubjects <- function(rpt) {
  rnk <- 1

  for (i in 1:length(rpt)) {
    if (nrow(rpt[[i]]) == 0) {
      next
    }

    if (names(rpt[i]) == "low.val") {
      rpt[[i]][, "value"] <- "Low Value"
    } else if (names(rpt[i]) == "noParentage") {
      rpt[[i]][, "value"] <- "Undetermined"
    } else {
      rpt[[i]][, "value"] <- "High Value"
    }

    if (names(rpt[i]) == "noParentage") {
      rpt[[i]][, "rank"] <- NA
    } else {
      rpt[[i]][, "rank"] <- rnk:(rnk + nrow(rpt[[i]]) - 1)
      rnk <- rnk + nrow(rpt[[i]])
    }

  }
  return(rpt)
}
