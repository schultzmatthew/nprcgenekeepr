#' Vector of date column names
#'
#' @return Vector of column names in a standardized pedigree object that are
#' dates.
#'
getDateColNames <- function() {
  c("birth", "death", "departure", "exit")
}
