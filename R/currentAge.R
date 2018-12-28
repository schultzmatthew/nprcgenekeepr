#' Returns age in years using the provided birthdate.
#'
#' @return Age in years using the provided birthdate.
#'
#' Assumes current date for calculating age.
#' @param birth birth date(s)
#' @import lubridate
#' @export
currentAge <- function(birth) {
  interval(start = birth, end = today()) /
    duration(num = 1, units = "years")
}
