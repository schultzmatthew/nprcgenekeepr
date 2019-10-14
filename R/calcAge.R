#' Calculate animal ages.
#'
## Copyright(c) 2017-2019 R. Mark Sharp
## This file is part of nprcmanager
#' Part of Pedigree Curation
#'
#' Given vectors of birth and exit dates, calculate an individuals age. If no
#' exit date is provided, the calculation is based on the current date.
#'
#' @param birth Date vector of birth dates
#' @param exit Date vector of exit dates.
#'
#' @return A numeric vector (\code{NA} allowed) indicating age in decimal years
#' from "birth" to "exit" or the current date if "exit" is NA.
#' @export
calcAge <- function(birth, exit) {
  if (length(birth) == 0)
    return(birth)
  exit[is.na(exit)] <- Sys.Date()
  return(round( (as.double(exit - birth) / 365.25), 1))
}
