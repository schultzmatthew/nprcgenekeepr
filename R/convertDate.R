#' Converts date columns formatted as characters to be of type datetime
#'
#' Part of Pedigree Curation
#'
#' @param ped a dataframe of pedigree information that may contain birth,
#' death, departure, or exit dates. The fields are optional, but will be used
#' if present.(optional fields: birth, death, departure, and exit).
#' @param time.origin date object used by \code{as.Date} to set \code{origin}.
#' @param errors logical value if TRUE will scan the entire file and
#' make a list of all errors found. The errors will be returned in a
#' list of list where each sublist is a type of error found.
#' @return A dataframe with an updated table with date columns converted from
#' \code{character} data type to \code{Date} data type. Values that do not
#' conform to the format %Y%m%d are set to NA. NA values are left as NA.
#' @importFrom rmsutilityr get_and_or_list
#' @importFrom rmsutilityr is_valid_date_str
#' @export
convertDate <- function(ped, time.origin = as.Date("1970-01-01"), errors = FALSE) {
  headers <-  tolower(names(ped))
  headers <- headers[headers %in% c("birth", "death", "departure", "exit")]
  format = "%Y-%m-%d"
  invalid_date_rows <- NULL
  for (header in headers) {
    dates <- ped[[header]]
    if (class(dates) == "factor") {
      dates <- as.character(dates)
    }
    if (class(dates) == "Date")
      dates <- as.character(dates, format = "%Y-%m-%d")
    if (class(dates) == "character") {
      dates[dates == ""] <- NA
      dates <- insertSeparators(dates)
      dates <- as.Date(dates, format = format, origin = time.origin,
                      optional = TRUE)
      dates <- removeEarlyDates(dates, 1000)
    }
    if (any(is.na(dates))) {
      if (errors) {
        invalid_date_rows <- c(invalid_date_rows,
                               seq_along(dates)[is.na(dates)])
        next
      }
      rowNums <- get_and_or_list(seq_along(dates)[is.na(dates)], "and")
      stop(paste0("Column '", header, "' has invalid dates on row(s) ",
                  rowNums, "."))
    }

    ped[[header]] <- dates
  }
  if (errors) {
    if (!is.null(invalid_date_rows))
      invalid_date_rows <- as.character(sort(invalid_date_rows))
    return(invalid_date_rows)
  } else {
    return(ped)
  }
}
