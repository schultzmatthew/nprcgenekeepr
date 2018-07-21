#' Converts date columns formatted as characters to be of type datetime
#'
#' Part of Pedigree Curation
#'
#' @param ped a dataframe of pedigree information that may contain birth,
#' death, departure, or exit dates. The fields are optional, but will be used
#' if present.(optional fields: birth, death, departure, and exit).
#' @param time.origin date object used by \code{as.Date} to set \code{origin}.
#' @return A dataframe with an updated table with date columns converted from
#' \code{character} data type to \code{Date} data type.
#' @export
convertDates <- function(ped, time.origin = as.Date("1970-01-01")) {
  headers <-  tolower(names(ped))
  headers <- headers[headers %in% c("birth", "death", "departure", "exit")]
  for (header in headers) {
    if (!all(is_valid_date_str(ped[[header]], format = "%Y-%m-%d"))) {
      rowNums <- get_and_or_list(seq_along(ped[[header]])[
        !is_valid_date_str(ped[[header]], format = "%Y-%m-%d")], "and")
      stop(paste0("Column '", header, "' has invalid dates on row(s) ",
                  rowNums, "."))
    }

    ped[[header]] <- as.Date(ped[[header]], format = format, origin = time.origin)
  }
  return(ped)
}
