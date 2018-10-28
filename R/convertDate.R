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
#' \code{character} data type to \code{Date} data type.
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
    if (class(dates) == "character") {
      dates[dates == ""] <- NA
    }
    if (!all(is.na(dates))) {
      if (!any(stri_detect_regex(dates[!is.na(dates)], pattern = "[-/]"))) {
        if (all(suppressWarnings(as.integer(dates[!is.na(dates)]) &
                                 !is.na(as.integer(dates[!is.na(dates)]))))) {
          dates <- sapply(dates, function(x) {
            stri_c(stri_sub(x, from = 1, to = 4), "-",
                   stri_sub(x, from = 5, to = 6), "-",
                   stri_sub(x, from = 7, to = 8))
          })
        }
      }
      dates[year(as.Date(dates, format = format,
                        origin = time.origin, optional = TRUE)) < 1000 |
              is.na(dates)] <- NA
      if (!all(is_valid_date_str(dates[!is.na(dates)],
                                 format = format, optional = FALSE))) {
        if (errors) {
          t_invalid_date_rows <- as.character(seq_along(dates)[
            !is_valid_date_str(dates, format = format, optional = FALSE)])
          invalid_date_rows <- c(invalid_date_rows, t_invalid_date_rows)
          next
        }
        rowNums <- get_and_or_list(seq_along(dates)[
          !is_valid_date_str(dates[!is.na(dates)],
                             format = format)], "and")
        stop(paste0("Column '", header, "' has invalid dates on row(s) ",
                    rowNums, "."))
      }
    }

    ## Check for bad dates detected by anytime
    #dates <- anytime(dates, oldHeuristic = TRUE)
    ped[[header]] <- as.Date(dates, format = format,
                             origin = time.origin, optional = TRUE)
  }
  if (errors) {
    return(invalid_date_rows)
  } else {
    return(ped)
  }
}
