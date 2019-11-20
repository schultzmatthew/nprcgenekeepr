#' removeEarlyDates removes dates before a specified year
#'
## Copyright(c) 2017-2019 R. Mark Sharp
## This file is part of nprcmanager
#' Dates before a specified year are set to NA. This is often used for dates
#' formed from malformed character representations such as a date in
#' %m-%d-%Y format being read by %Y-%m-%d format
#'
#' NA values are ignored and not changed.
#' @param dates vector of dates
#' @param firstYear integer value of first (earliest) year in the allowed
#' date range.
#' @importFrom lubridate year
#' @export
removeEarlyDates <- function(dates, firstYear) {
  dates[year(dates) < firstYear & !is.na(dates)] <- NA
  dates
}
