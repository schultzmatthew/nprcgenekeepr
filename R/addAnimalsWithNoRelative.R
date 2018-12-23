#' addAnimalsWithNoRelative adds an NA value for all animals without a relative
#'
#' @return dataframe with kinships in long form after adding a row for each
#' animal without a relative.
#' @param kin dataframe with kinship values
#' @param candidates character vector of IDs of the animals available for
#' use in the group.
addAnimalsWithNoRelative <- function(kin, candidates) {

  # adding animals with no relatives
  for (cand in setdiff(candidates, names(kin))) {
    kin[[cand]] <- c(NA)
  }
  kin
}
