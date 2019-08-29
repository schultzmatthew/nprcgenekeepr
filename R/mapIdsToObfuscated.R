#' Map IDs to Obfuscated IDs
#'
#' @return A dataframe or vector with original IDs replaced by their obfuscated
#' counterparts.
#'
#' This is not robust as it fails if all IDs are found within \code{map}.
#'
#' @param ids character vector with original IDs
#' @param map named character vector where the values are the obfuscated IDs
#' and the vector of names (\code{names(map)}) is the vector of original names.
#' @export
mapIdsToObfuscated <- function(ids, map) {
  if (!all(ids %in% names(map)))
    stop("Some IDs are not in map.")
  as.character(sapply(ids, function(id) {
    map[names(map) == as.character(id)]}))
}
