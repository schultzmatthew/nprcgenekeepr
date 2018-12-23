
#' Updates list of available animals by removing the selected animal
#'
#' @return list of available animals
#'
#' @param available list of available animals for each group
#' @param id character vector of length one having the selected animals Id
#' @param numGp integer indicating the number of groups being formed.
removeSelectedAnimalFromAvailableAnimals <- function(available, id, numGp) {
  # Remove the selected animal from consideration
  for (j in 1:numGp) {
    available[[j]] <- setdiff(available[[j]], id)
  }
  available
}
