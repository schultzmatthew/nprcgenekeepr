#' Converts date columns formatted as characters to be of type datetime
#'
#' Part of Pedigree Curation
#'
#' @param ped a dataframe of pedigree information that may contain birth,
#' death, departure, or exit dates. The fields are optional, but will be used
#' if present.(optional fields: birth, death, departure, and exit).
#' @param time.origin date object used by \code{as.Date} to set \code{origin}.
#' @param reportErrors logical value if TRUE will scan the entire file and
#' make a list of all errors found. The errors will be returned in a
#' list of list where each sublist is a type of error found.
#' @return A dataframe with an updated table with date columns converted from
#' \code{character} data type to \code{Date} data type. Values that do not
#' conform to the format %Y%m%d are set to NA. NA values are left as NA.
#' @importFrom rmsutilityr get_and_or_list
#' @importFrom rmsutilityr is_valid_date_str
#' @importFrom stringi stri_trim_both
#' @export
convertDate <- function(ped, time.origin = as.Date("1970-01-01"), reportErrors = FALSE) {
  ## Ignore records added because of unknown parents
  if (any("record_status" %in% names(ped))) {
    addedPed <- ped[ped$record_status == "added", ]
    ped <- ped[ped$record_status == "original", ]
    if (nrow(ped) == 0)
      return(rbind(ped, addedPed))
  }

  headers <-  tolower(names(ped))
  headers <- headers[headers %in% c("birth", "death", "departure", "exit")]
  format = "%Y-%m-%d"
  invalid_date_rows <- NULL
  for (header in headers) {
    dates <- ped[[header]]
    if (class(dates) == "factor" | class(dates) == "logical") {
      dates <- as.character(dates)
    }
    if (class(dates) == "Date") {
      originalNAs <- is.na(dates)
      dates <- dates[!originalNAs]
    } else if (class(dates) == "character") {
      dates[stri_trim_both(dates) == ""] <- NA
      ped[[header]] <- dates
      originalNAs <- is.na(dates)
      dates <- dates[!originalNAs]
      if (length(dates) > 0) {
        dates <- insertSeparators(dates)
        dates <- as.Date(dates, format = format, origin = time.origin,
                         optional = TRUE)
        dates <- removeEarlyDates(dates, 1000)
      }
    } else {
      stop(stri_c("class(dates) is not 'character', 'factor', 'integer', or ",
                  "'Date' it is == ", class(dates)))
    }

    if (any(is.na(dates))) {
      goodAndBadDates <- ifelse(is.na(dates), "bad", "good")
      originalDates <- ped[[header]]
      originalDates[originalNAs] <- "good"
      originalDates[!originalNAs] <- goodAndBadDates
      if (reportErrors) {
        invalid_date_rows <- c(invalid_date_rows,
                               seq_along(originalDates)[originalDates == "bad"])
        next
      }
      rowNums <- get_and_or_list(
        seq_along(originalDates)[originalDates == "bad"], "and")
      stop(paste0("Column '", header, "' has invalid dates on row(s) ",
                  rowNums, "."))
    }
    ped[!originalNAs, header] <- dates
    ped[[header]] <- as.Date(as.integer(ped[[header]]), origin = time.origin)
  }
  if (reportErrors) {
    if (!is.null(invalid_date_rows))
      invalid_date_rows <- as.character(sort(invalid_date_rows))
    return(invalid_date_rows)
  } else {
    ## Add back records of unknown parents
    if (any("record_status" %in% names(ped)))
      ped <- rbind(ped, addedPed)
    return(ped)
  }
}
