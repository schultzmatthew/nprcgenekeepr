# PedigreeCuration.R
# 2014-10-02
#
# Contains functions to load, and quality control pedigree information


###############################################################################
# Helper Functions:
#' Append the rows of one dataframe to another.
#'
#' Appends the rows of df2 to df1, can handle cases where df2
#' has a subset of the columns of df1
#'
#' @param df1 the target dataframe to append to.
#' @param df2 the the donor dataframe information should be appended from
#'
#' @return The appended dataframe with \code{NA} inserted into columns as
#' needed.
#' @export
rebindFill <- function(df1, df2) {

  # Find columns in df1 not in df2
  add.headers <- setdiff(names(df1), names(df2))

  # Add the missing columns to df2 (containing NA values)
  if (!isEmpty(add.headers)) {
    for (i in 1:length(add.headers)) {
      c <- df1[, add.headers[i]]
      col.type <- mode(c)
      if (col.type == "numeric") {
        if (class(c) == "Date") {
          df2[, add.headers[i]] <- as.Date(NA, origin = as.Date("1970-01-01"))
        } else{
          df2[, add.headers[i]] <- NaN
        }
      }
      else if (col.type == "character") {
        df2[, add.headers[i]] <- NA
      }
      else if (col.type == "logical") {
        df2[, add.headers[i]] <- NA
      }
      else{
        stop(col.type, " : unknown column type")
      }
    }
  }
  return(rbind(df1, df2))
}
#' Updates sex for animals listed as either a sire or dam.
#'
#' @param id character vector with unique identifier for an individual
#' @param sire character vector with unique identifier for an
#' individual's father (\code{NA} if unknown).
#' @param dam character vector with unique identifier for an
#' individual's mother (\code{NA} if unknown).
#' @param sex factor with levels: "M", "F", "U". Sex specifier for an
#' individual.
#' @return A factor with levels: "M", "F", "H", and "U"
#' representing the sex codes for the ids provided
#' @export
checkParentSex <- function(id, sire, dam, sex) {
  # Get all sires and dams
  sires <- unique(sire)
  sires <- sires[!is.na(sires)]
  dams <- unique(dam)
  dams <- dams[!is.na(dams)]

  # Check if any ids are listed in both the sire and dam columns (error)
  err <- intersect(sires, dams)
  if (length(err > 0)) {
    stop(err, " : Subject(s) listed as both sire and dam")
  }

  # Update gender for sires and dams
  sex[((id %in% sires) & (sex != "M"))] <- "M"
  sex[((id %in% dams) & (sex != "F"))] <- "F"
  return(sex)
}
#' Converts sex indicator for an individual to a standardized codes.
#'
#' Standard sex codes are
#' \itemize{
#' \item{F} {-- replacing "FEMALE" or "2"}
#' \item{M} {-- replacing "MALE" or "1"}
#' \item{H} {-- replacing "HERMAPHRODITE" or "4", if igore.herm == FALSE}
#' \item{U} {-- replacing "HERMAPHRODITE" or "4", if igore.herm == TRUE}
#' \item{U} {-- replacing "UNKNOWN" or "3"}}
#'
#' @param sex factor indicating sex of a set of individuals
#' @param ignore.herm logical flag indicating if hermaphrodites should be
#' treated as unknown sex ("U"), default is \code{TRUE}.
#' @export
convertSexCodes <- function(sex, ignore.herm = TRUE) {
  sex <- toupper(sex)
  sex[is.na(sex)] <- "U"

  sex[sex %in% c("MALE", "M", "1")] <- "M"
  sex[sex %in% c("FEMALE", "F", "2")] <- "F"
  sex[sex %in% c("UNKNOWN", "U", "3")] <- "U"

  if (ignore.herm) {
    sex[sex %in% c("HERMAPHRODITE", "H", "4")] <- "U"
  } else {
    sex[sex %in% c("HERMAPHRODITE", "H", "4")] <- "H"
  }
  sex <- factor(sex, levels = c("F", "M", "H", "U"))
  return(sex)
}
#' Converts status indicators to a Standardized code
#'
#' @param status character vector or NA. Flag indicating an individual's
#' status as alive, dead, sold, etc.
#'
#' @return factor {levels: ALIVE, DECEASED, SHIPPED, UNKNOWN}. Vector of
#' standardized status codes with the possible values
#' ALIVE, DECEASED, SHIPPED, or UNKNOWN
#' @export
convertStatusCodes <- function(status) {
  status <- toupper(status)
  status[is.na(status)] <- "UNKNOWN"
  status[status %in% c("ALIVE", "A", "1")] <- "ALIVE"
  status[status %in% c("DECEASED", "DEAD", "D", "2")] <- "DECEASED"
  status[status %in% c("SHIPPED", "SOLD", "SALE", "S", "3")] <- "SHIPPED"
  status[status %in% c("UNKNOWN", "U", "4")] <- "UNKNOWN"

  status <- factor(status, levels = c("ALIVE", "DECEASED", "SHIPPED", "UNKNOWN"))
  return(status)
}

#' Converts the ancestry information to a standardized code
#'
#' @param ancestry character vector or NA with free-form text providing
#' information about the geographic population of origin.
#'
#' @return factor vector of standardized designators specifying if an animal is
#' a Chinese rhesus, Indian rhesus, Chinese-Indian hybrid rhesus, or
#' Japanese macaque. Levels: CHINESE, INDIAN, HYBRID, JAPANESE, OTHER, UNKNOWN.
#' @export
convertAncestry <- function(ancestry) {
  ancestry <- tolower(ancestry)

  # Find entries containing non-standardized indications of population
  chinese <- grepl("chin", ancestry) & !grepl("ind", ancestry)
  indian <- !grepl("chin", ancestry) & grepl("ind", ancestry)
  hybrid <- ((grepl("chin", ancestry) & grepl("ind", ancestry)) |
               grepl("hyb", ancestry))
  japanese <- grepl("jap", ancestry)
  unknown <- is.na(ancestry)

  other <- !(chinese | indian | hybrid | japanese) & !unknown

  ancestry[chinese] <- "CHINESE"
  ancestry[indian] <- "INDIAN"
  ancestry[hybrid] <- "HYBRID"
  ancestry[japanese] <- "JAPANESE"
  ancestry[unknown] <- "UNKNOWN"
  ancestry[other] <- "OTHER"

  ancestry <- factor(ancestry, levels = c("INDIAN", "CHINESE", "HYBRID",
                                        "JAPANESE", "OTHER", "UNKNOWN"))
  return(ancestry)
}
#' Converts date columns formatted as characters to be of type datetime
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
    ped[[header]] <- as.Date(ped[[header]], origin = time.origin)
  }
  return(ped)
}
#' Sets the exit date, if there is no exit column in the table
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
	  # as.Date("1970-01-01") is used to counter this and maintain Dates properly
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
#' Choose date based on \code{earlier} flag.
#'
#' Given two dates, one is selected to be returned based on whether
#' it occurred earlier or later than the other. \code{NAs} are ignored if
#' possible.
#'
#' @param d1 \code{Date} vector with the first of two dates to compare.
#' @param d2 \code{Date} vector with the second of two dates to compare.
#' @param earlier logical variable with \code{TRUE} if the earlier of the two
#' dates is to be returned, otherwise the later is returned. Default is
#' \code{TRUE}.
#'
#' @return \code{Date} vector of chosen dates or \code{NA} where neither
#' is provided
#' @export
chooseDate <- function(d1, d2, earlier = TRUE) {
  if (is.na(d1)) {
    return(d2)
  }
  else if (is.na(d2)) {
    return(d1)
  }
  else if ((d1 < d2) & earlier) {
    return(d1)
  }
  else if ((d1 > d2) & !earlier) {
    return(d1)
  }
  else{
    return(d2)
  }
}
#' Calculate animal ages.
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
  exit[is.na(exit)] <- Sys.Date()
  return(round((as.double(exit - birth) / 365.25), 1))
}
#' Returns \code{TRUE} if x is a zero-length vector.
#'
#' @param x vector of any type.
#' @export
isEmpty <- function(x) {
  x <- x[!is.na(x)]
  return(length(x) == 0)
}
#' Returns an updated dataframe with duplicate rows removed.
#'
#' Returns an error if the table has duplicate IDs with differing data.
#'
#' @param ped datatable that is the `Pedigree`. It contains pedigree
#' information. The \code{id} column is required.
#' @export
removeDuplicates <- function(ped) {
  p <- unique(ped)

  if (sum(duplicated(p$id)) == 0) {
    return(p)
  }
  else{
    stop("Duplicate IDs with mismatched information present")
  }
}

###############################################################################
# Pedigree Filtering:
#
