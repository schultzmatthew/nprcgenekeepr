# PedigreeCuration.R
# 2014-10-02
#
# Contains functions to load, and quality control pedigree information

###############################################################################
# Data Definition:
#

# Constants:
TIME.ORIGIN <- as.Date("1970-01-01")

#' Get possible column names for a studbook.
#'
#' @return A character vector of the possible columns that can be in a studbook.
#' The possible columns are as follows:
#' \itemize{
#' \item{id} {-- character vector with unique identifier for an individual}
#' \item{sire} {-- character vector with unique identifier for an
#' individual's father (\code{NA} if unknown).}
#' \item{dam} {-- character vector with unique identifier for an
#' individual's mother (\code{NA} if unknown).}
#' \item{sex} {-- factor {levels: "M", "F", "U"} Sex specifier for an
#' individual}
#' \item{gen} {-- integer vector with the generation number of the individual}
#' \item{birth} {-- Date or \code{NA} (optional) with the individual's birth
#' date}
#' \item{exit} {-- Date or \code{NA} (optional) with the individual's exit date
#' (death, or departure if applicable)}
#' \item{ancestry} {-- character vector or \code{NA} (optional) that indicates
#' the geographic population to which the individual belongs.}
#' \item{age} {-- numeric or \code{NA} (optional) indicating the individual's
#' current age or age at exit.}
#' \item{population} {-- an optional logical argument indicating wheter or not
#' the \code{id} is part of the extant population.}
#' \item{origin} {-- character vector or \code{NA} (optional) that indicates
#' the name of the facility that the individual was imported from.
#' \code{NA} indicates the invidual was not imported.}
#' \item{status} {-- an optional factor indicating the status of an individual
#' with levels \code{ALIVE}, \code{DEAD}, and \code{SHIPPED}.}
#' \item{condition} {--  character vector or \code{NA} (optional) that indicates
#' the restricted status of an animal. "Nonrestricted" animals
#' are generally assumed to be naive.}
#' \item{spf} {-- character vector or \code{NA} (optional) indicating the
#' specific pathogen-free status of an individual.}
#' \item{vasx.ovx} {-- character vector indicating the vasectomy/overiectomy
#' status of an animal where \code{NA} indicates an intact animal and all other
#' values inicate surgical alteration.}
#' \item{ped.num} {-- integer vector indicating generation numbers for each id,
#' starting at 0 for individuals lacking IDs for both parents.}}
#'
#' @export
getPossibleCols <- function() {
  c("id", "sire", "dam", "sex", "gen", "birth", "exit", "age",
    "ancestry", "population", "origin", "status", "condition",
    "spf", "vasx.ovx", "ped.num")
}
#' Main pedigree curation function that performs basic quality control on
#' pedigree information
#'
#' @param sb A dataframe containing a table of pedigree and demographic
#' information.
#'
#' The function recognizes the following columns (optional columns
#' will be used if present, but are not required):
#'
#' \itemize{
#' \item{id} {--- Character vector with Unique identifier for all individuals}
#' \item{sire} {--- Character vector with unique identifier for the father of the
#' current id}
#' \item{dam} {--- Character vector with unique identifier for the mother of the
#' current id}
#' \item{sex} {--- Factor {levels: "M", "F", "U"} Sex specifier for an
#' individual}
#' \item{birth} {--- Date or \code{NA} (optional) with the individual's birth
#' date}
#' \item{departure} {--- Date or \code{NA} (optional) an individual was sold
#' or shipped from the colony}
#' \item{death} {--- date or \code{NA} (optional)
#'  Date of death, if applicable}
#' \item{status} {--- Factor {levels: ALIVE, DEAD, SHIPPED} (optional)
#'  Status of an individual}
#' \item{origin} {--- Character or \code{NA} (optional)
#'  Facility an individual originated from, if other than ONPRC}
#' \item{ancestry} {--- Character or \code{NA} (optional)
#'  Geographic population to which the individual belongs}
#' \item{spf} {--- Character or \code{NA} (optional)
#'  Specific pathogen-free status of an individual}
#' \item{vasx.ovx} {--- Character or \code{NA} (optional)
#'  Indicator of the vasectomy/ovariectomy status of an animal; \code{NA} if
#'  animal is intact, assume all other values indicate surgical alteration}
#' \item{condition} {--- Character or \code{NA} (optional)
#'  Indicator of the restricted status of an animal. "Nonrestricted" animals
#'  are generally assumed to be naive.}
#' }
#' @return A datatable with standardized and quality controlled pedigree
#' information.
#'
#' The following changes are made to the headers.
#'
#' \itemize{
#' \item {Column headers are converted to all lower case}
#' \item {Periods (".") within column headers are collapsed to no space ""}
#' \item {\code{egoid} is converted to \code{id}}
#' \item {\code{sireid} is convert to \code{sire}}
#' \item {\code{damid} is converted to \code{dam}}}
#'
#' If the dataframe (\code{sb} does not contain the five required columns
#' (\code{id}, \code{sire}, \code{dam}, \code{sex}), and
#' \code{birth} the function throws an error by calling \code{stop()}.
#'
#' If the \code{id} field has the string \emph{UNKNOWN} (any case) or both
#' the fields \code{sire} or \code{dam} have \code{NA} or \emph{UNKNOWN}
#' (any case), the record is removed.
#' If either of the fields \code{sire} or \code{dam} have the
#' string \emph{UNKNOWN} (any case), they are replaced with a unique identifier
#' with the form \code{Unnnn}, where \code{nnnn} represents one of a series
#' of sequential integers representing the number of missing sires and
#' dams right justified in a pattern of \code{0000}. See \code{add.uIds}
#' function.
#'
#' The function \code{addParents} is used to add records for parents missing
#' their own record in the pedigree.
#'
#' The function \code{convertSexCodes} is used with \code{ignore.herm == TRUE}
#' to convert sex codes according to the following factors of standardized
#' codes:
#'
#' \itemize{
#' \item{F} {-- replacing "FEMALE" or "2"}
#' \item{M} {-- replacing "MALE" or "1"}
#' \item{H} {-- replacing "HERMAPHRODITE" or "4", if igore.herm == FALSE}
#' \item{U} {-- replacing "HERMAPHRODITE" or "4", if igore.herm == TRUE}
#' \item{U} {-- replacing "UNKNOWN" or "3"}}
#'
#' The function \code{checkParentSex} is used to ensure no parent is both
#' a sire and a dam. If this error is detected, the function throws an error
#' and halts the program.
#'
#' The function \code{convertStatusCodes} converts status indicators to the
#' following factors of standardized codes. Case of the original status value
#' is ignored.
#'
#' \itemize{
#' \item{"ALIVE"} {--- replacing "alive", "A" and "1"}
#' \item {"DECEASED"} {--- replacing "deceased", "DEAD", "D", "2"}
#' \item {"SHIPPED"} {--- replacing "shipped", "sold", "sale", "s", "3"}
#' \item{"UNKNOWN"} {--- replacing is.na(status)}
#' \item {"UNKNOWN"} {--- replacing "unknown", "U", "4"}}
#'
#' The function \code{convertAncestry} coverts ancestry indicators using
#' regular experessions such that the following conversions are made from
#' character strings that match selected substrings to the following factors.
#'
#' \itemize{
#' \item{"INDIAN"} {--- replacing "ind" and not "chin"}
#' \item{"CHINESE"} {--- replacing "chin" and not "ind"}
#' \item{"HYBRID"} {--- replacing "hyb" or "chin" and "ind"}
#' \item{"JAPANESE"} {--- replacing "jap"}
#' \item{"UNKNOWN"} {--- replacing \code{NA}}
#' \item{"OTHER"} {--- replacing not matching any of the above}}
#'
#' The function \code{convertDates} converts character representations of
#' dates in the columns \code{birth}, \code{death}, \code{departure}, and
#' \code{exit} to dates using the \code{as.Date} function.
#'
#' The function \code{setExit} uses huristics and the columns \code{death}
#' and \code{departure} to set \code{exit} if it is not already defined.
#'
#' The function \code{calcAge} uses the \code{birth} and the \code{exit}
#' columns to define the \code{age} columnn. The numerical values is rounded
#' to the nearest 0.1 of a year. If \code{exit} is not defined, the
#' current system date (\code{Sys.Date()}) is used.
#'
#' The function \code{findGeneration} is used to define the generation number
#' for each animal in the pedigree.
#'
#' The function \code{removeDuplicates} checks for any duplicated records and
#' removeds the duplicates. I also throws an error and stops the program if an
#' ID appears in more
#' than one record where one or more of the other columns have a difference.
#'
#' Columns that cannot be used subsequently are removed and the rows are
#' ordered by generation number and then ID.
#'
#' Finally the columns \code{id} \code{sire}, and \code{dam} are coerce to
#' character.
#'
#' @export
qc.Studbook <- function(sb) {
  headers <- tolower(names(sb))
  headers <- gsub(" ", "", headers)
  headers <- gsub("_", "", headers)
  headers <- gsub("egoid", "id", headers)
  headers <- gsub("sireid", "sire", headers)
  headers <- gsub("damid", "dam", headers)
  headers <- gsub("birthdate", "birth", headers)

  # Checking for the 4 required fields (id, sire, dam, sex)
  if (is.na(match("id", headers))) {
    stop("No valid headers found")
  }

  names(sb) <- headers
  ## An age column was required, however, code below creates it if it does
  ## not exists. Thus, it is not required as a prerequisit.
  required_cols <- c("id", "sire", "dam", "sex", "birth")
  required <- required_cols %in% headers

  if (!all(required)) {
    stop(paste0("Required field missing (", paste0(required_cols[!required],
                                                   collapse = ", "), ")."))
  }

  # Removing erroneous IDs (someone started entering "unknown" for unknown
  # parents instead of leaving the field blank in PRIMe)
  sb <- sb[toupper(sb$id) != "UNKNOWN", ]
  sb$sire[toupper(sb$sire) == "UNKNOWN"] <- NA
  sb$dam[toupper(sb$dam) == "UNKNOWN"] <- NA

  # Adding UIDs
  sb <- add.uIds(sb)

  # Find any parents that don't have their own line entry
  sb <- addParents(sb)

  # Add and standardize needed fields
  sb$sex <- convertSexCodes(sb$sex)
  sb$sex <- checkParentSex(sb$id, sb$sire, sb$dam, sb$sex)

  if ("status" %in% headers) {
    sb$status <- convertStatusCodes(sb$status)
  }
  if ("ancestry" %in% headers) {
    sb$ancestry <- convertAncestry(sb$ancestry)
  }

  # converting date column entries from strings to date
  sb <- convertDates(sb, time.origin = TIME.ORIGIN)
  sb <- setExit(sb, time.origin = TIME.ORIGIN)

  # setting age
  # uses current date as the end point if no exit date is available
  if (("birth" %in% headers) && !("age" %in% headers)) {
    sb["age"] <- calcAge(sb$birth, sb$exit)
  }

  # Adding generation numbers
  sb["gen"] <- findGeneration(sb$id, sb$sire, sb$dam)

  # Cleaning-up the data.frame
  # Filtering unnecessary columns and ordering the data
  sb <- removeDuplicates(sb)
  cols <- intersect(getPossibleCols(), colnames(sb))
  sb <- sb[, cols]
  sb <- sb[with(sb, order(gen, id)), ]
  rownames(sb) <- seq(length.out = nrow(sb))

  # Ensuring the IDs are stored as characters
  sb$id <- as.character(sb$id)
  sb$sire <- as.character(sb$sire)
  sb$dam <- as.character(sb$dam)

  return(sb)
}

###############################################################################
# Helper Functions:
#' Add parents
#'
#' Given a pedigree, find any IDs listed in the "sire" or "dam" columns
#' that lack their own line entry and generate one.
#'
#' @param ped datatable that is the `Pedigree`. It contains pedigree
#' information including the IDs listed in \code{candidates}.
#'
#' @return An updated pedigree with entries added as necessary.
#' Entries have the id and sex specified; all remaining columns are filled
#' with \code{NA}.
#' @export
addParents <- function(ped) {
  sires <- ped$sire
  dams <- ped$dam

  # Finding sires and dams not in the id column
  a1 <- sires[!(sires %in% ped$id) & !is.na(sires)]
  a1 <- a1[!duplicated(a1)]
  a2 <- dams[!(dams %in% ped$id) & !is.na(dams)]
  a2 <- a2[!duplicated(a2)]

  a1 <- data.frame(id = a1, stringsAsFactors = FALSE)
  a2 <- data.frame(id = a2, stringsAsFactors = FALSE)

  # Adding line entries for these parents
  if (nrow(a1) > 0) {
    a1$sire <- NA
    a1$dam <- NA
    a1$sex <- "M"
    ped <- rbind.fill(ped, a1)
  }

  if (nrow(a2) > 0) {
    a2$sire <- NA
    a2$dam <- NA
    a2$sex <- "F"
    ped <- rbind.fill(ped, a2)
  }
  return(ped)
}
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
rbind.fill <- function(df1, df2) {

  # Find columns in df1 not in df2
  add.headers <- setdiff(names(df1), names(df2))

  # Add the missing columns to df2 (containing NA values)
  if (!isEmpty(add.headers)) {
    for (i in 1:length(add.headers)) {
      c <- df1[, add.headers[i]]
      col.type <- mode(c)
      if (col.type == "numeric") {
        if (class(c) == "Date") {
          df2[, add.headers[i]] <- as.Date(NA, origin = TIME.ORIGIN)
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
#'
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
	  # TIME.ORIGIN is used to counter this and maintain Dates properly
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
#' Determines the generation number for each id.
#'
#' @param id character vector with unique identifier for an individual
#' @param sire character vector with unique identifier for an
#' individual's father (\code{NA} if unknown).
#' @param dam character vector with unique identifier for an
#' individual's mother (\code{NA} if unknown).
#'
#' @return An integer vector indication the generation numbers for each id,
#' starting at 0 for individuals lacking IDs for both parents.
#'
#' @export
findGeneration <- function(id, sire, dam) {
  parents <- c()
  gen <- rep(NA, length(id))
  i <- 0

#' @description{The first time through this loop no sire or dam is in parents.
#' This means that the animals without a sire and without a dam are
#' assigned to generation 0 and become the first parental genereation.
#' The second time through this loop finds all of the animals that do
#' not have a sire or do not have a dam and at least one parent
#' is in the vector of parents defined the first time through.
#' The ids that were not assigned as parents in the previous loop
#' are given the incremented generation number.}

#' Subsequent trips in the loop repeat what was done the second time
#' through until no further animals can be added to the \code{next.gen}
#' vector.
  while (TRUE) {
    cumulative.parents <- id[(is.na(sire) | (sire %in% parents)) &
                               (is.na(dam) | (dam %in% parents))]
    next.gen <- setdiff(cumulative.parents, parents)

    if (isEmpty(next.gen)) {
      break
    }

    gen[id %in% next.gen] <- i
    i <- i + 1

    parents <- cumulative.parents
  }
  return(gen)
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
#' Eliminates partial parentage situations by adding unique placeholder
#' IDs for the unknown parent.
#'
#' @param ped datatable that is the `Pedigree`. It contains pedigree
#' information. The fields \code{sire} and \code{dam} are required.
#' @return The updated pedigree with partial parentage removed.
#' @export
add.uIds <- function(ped) {
  s <- which(is.na(ped$sire) & !is.na(ped$dam))
  d <- which(!is.na(ped$sire) & is.na(ped$dam))

  if (!identical(s, integer(0))) {
    k <- length(s)
    sire_ids <- paste("U", sprintf("%04d", 1:k), sep = "")
    ped[s, "sire"] <- sire_ids
  }
  else{
    k <- 0
  }

  if (!identical(d, integer(0))) {
    m <- k + 1
    n <- k + length(d)
    dam_ids <- paste("U", sprintf("%04d", m:n), sep = "")
    ped[d, "dam"] <- dam_ids
  }

  return(ped)
}
#' Determines the generation number for each id.
#'
#' @param id character vector with unique identifier for an individual
#' @param sire character vector with unique identifier for an
#' individual's father (\code{NA} if unknown).
#' @param dam character vector with unique identifier for an
#' individual's mother (\code{NA} if unknown).
#' @return Integer vector indicating generation numbers for each id,
#' starting at 0 for individuals lacking IDs for both parents.
#' @export
findPedigreeNumber <- function(id, sire, dam) {
  founders <- id[is.na(sire) & is.na(dam)]
  ped.num <- rep(NA, length(id))
  n <- 1

  while (!isEmpty(founders)) {
    population <- founders[1]

    while (TRUE) {
      parents <- union(sire[id %in% population],
                       dam[id %in% population])
      parents <- parents[!is.na(parents)]

      offspring <- id[(sire %in% population)  | (dam %in% population)]

      added <- setdiff(union(offspring, parents), population)

      if (isEmpty(added)) {
        break
      }

      population <- union(population, union(parents, offspring))
    }
    ped.num[id %in% population] <- n
    n <- n + 1

    founders <- setdiff(founders, population)
  }
  return(ped.num)
}

###############################################################################
# Pedigree Filtering:
#
#' Population designation function
#'
#' @param ids character vector of IDs to be flagged as part of the population
#' under consideration.
#' @param ped datatable that is the `Pedigree`. It contains pedigree
#' information. The \code{id} column is required.
#'
#' @return An updated pedigree with the \code{population} column added or
#' updated.
#' @export
resetPopulation <- function(ids, ped) {
  ped$population <- FALSE

  if (length(ids) == 0) {
    ped$population <- TRUE
  } else {
    ped$population[ped$id %in% ids] <- TRUE
  }
  return(ped)
}
#' Update or add the "group" field of a Pedigree.
#'
#' @param ids character vector of IDs to be flagged as part of the group under
#' consideration.
#' @param ped datatable that is the `Pedigree`. It contains pedigree
#' information. The \code{id} column is required.
#'
#' @return An updated pedigree with the \code{group} column added or updated.
#' @export
resetGroup <- function(ids, ped) {
  ped$group <- FALSE
  ped$group[ped$id %in% ids] <- TRUE
  return(ped)
}
