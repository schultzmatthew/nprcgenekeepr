#' Sets the exit date, if there is no exit column in the table
#'
#'
#' Part of Pedigree Curation
#'
#' @param ped dataframe of pedigree and demographic information potentially
#' containing columns indicating the birth and death dates of an individual.
#' The table may also contain dates of sale (departure). Optional columns
#' are \code{birth}, \code{death}, and \code{departure}.
#' @param time.origin date object used by \code{as.Date} to set \code{origin}.
#'
#' @return A dataframe with an updated table with exit dates specified based
#' on date information that was available.
#' @export
setExit <- function(ped, time.origin = as.Date("1970-01-01")) {
  headers <- tolower(names(ped))

  if (("birth" %in% headers) && !("exit" %in% headers)) {
    if (("death" %in% headers) && ("departure" %in% headers)) {
      # mapply simplifies results by default
      # mapply would return a list, but simplification coerces this to a vector
      # consequently, the simplification also coerces Date columns to Numeric
      # as.Date("1970-01-01") is used to counter this and maintain Dates
      # properly
      ped$exit <- as.Date(mapply(chooseDate, ped$death, ped$departure),
                          origin = time.origin)
    } else if ("death" %in% headers) {
      ped$exit <- ped$death
    } else if ("departure" %in% headers) {
      ped$exit <- ped$departure
    } else {
      ped$exit <- as.Date(NA, origin = time.origin)
    }
  }
  return(ped)
}
