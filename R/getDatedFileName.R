#' Returns a character vector with an file name having the date prepended.
#'
## Copyright(c) 2017-2019 R. Mark Sharp
## This file is part of nprcgenekeepr
#' @param filename character vector with name to use in file name
#' @import lubridate
#' @export
getDatedFilename <- function(filename) {
  dateStamp <- stri_replace_all_fixed(
    stri_replace_all_fixed(as.character(now()), " ", "_"), ":", "_")
  stri_c(dateStamp, "_", filename)
}
