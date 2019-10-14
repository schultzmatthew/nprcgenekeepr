#' Returns age in years using the provided birthdate.
#'
## Copyright(c) 2017-2019 R. Mark Sharp
## This file is part of nprcmanager
#' @return Age in years using the provided birthdate.
#'
#' Assumes current date for calculating age.
#' @param birth birth date(s)
#' @import lubridate
#' @export
getCurrentAge <- function(birth) {
  interval(start = birth, end = today()) /
    duration(num = 1, units = "years")
}
