#' getVersion Get the version number of nprcgenekeepr
#'
## Copyright(c) 2017-2020 R. Mark Sharp
## This file is part of nprcgenekeepr
#' @return Current Version
#' @examples
#' \donttest{
#' library(nprcgenekeepr)
#' getVersion()
#' }
#' @param date A logical value when TRUE (default) a date in YYYYMMDD format
#' within parentheses is appended.
#' @export
getVersion <- function(date = TRUE) {
  version = "1.0.3"
  version_date = "20200526"
  if (date) {
    paste0(version, " (", version_date, ")")
  } else {
    version
  }
}
