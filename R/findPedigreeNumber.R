#' Determines the generation number for each id.
#'
#' One of Pedigree Curation functions
#' @param id character vector with unique identifier for an individual
#' @param sire character vector with unique identifier for an
#' individual's father (\code{NA} if unknown).
#' @param dam character vector with unique identifier for an
#' individual's mother (\code{NA} if unknown).
#' @return Integer vector indicating generation numbers for each id,
#' starting at 0 for individuals lacking IDs for both parents.
#' @export
findPedigreeNumber <- function(id, sire, dam) {
  founders <- id[is.na(sire) & is.na(dam)]
  ped.num <- rep(NA, length(id))
  n <- 1

  while (!isEmpty(founders)) {
    population <- founders[1]

    while (TRUE) {
      parents <- union(sire[id %in% population],
                       dam[id %in% population])
      parents <- parents[!is.na(parents)]

      offspring <- id[(sire %in% population)  | (dam %in% population)]

      added <- setdiff(union(offspring, parents), population)

      if (isEmpty(added)) {
        break
      }

      population <- union(population, union(parents, offspring))
    }
    ped.num[id %in% population] <- n
    n <- n + 1

    founders <- setdiff(founders, population)
  }
  return(ped.num)
}
