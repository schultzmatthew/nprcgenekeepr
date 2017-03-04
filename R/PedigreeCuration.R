# PedigreeCuration.R
# 2014-10-02
#
# Contains functions to load, and quality control pedigree information

###############################################################################
# Data Definition:
#

# Constants:
TIME.ORIGIN <- as.Date("1970-01-01")
#' Get possible column names for a studbood.
#'
#' @return a character vector of the possible columns that can be in a studbook.
#' The possible columns are as follows:
#' \itemize{
#' \item{id} {-- character vector with unique identifier for an individual}
#' \item{sire} {-- character vector with unique identifier for an
#' individual's father (\code{NA} if unknown).}
#' \item{dam} {-- character vector with unique identifier for an
#' individual's mother (\code{NA} if unknown).}
#' \item{sex} {-- factor {levels: "M", "F", "U"} Sex specifier for an individual}
#' \item{gen} {-- integer vector with the generation number of the individual}
#' \item{birth} {-- Date or NA (optional) with the individual's birth date}
#' \item{exit} {-- Date or NA (optional) with the individual's exit date (death,
#'  or departure if applicable)}
#' \item{ancestry} {-- character vector or \code{NA} (optional) that indicates
#' the geographic population to which the individual belongs.}
#' \item{age} {-- numeric or \code{NA} (optional) indicating the individual's
#' current age or age at exit.}
#' \item{population} {-- logical (optional)
#' Is the id part of the extant population?}
#' \item{origin} {-- character vector or \code{NA} (optional) that indicates
#' the name of the facility that the individual was imported from.
#' \code{NA} indicates the invidual was not imported.}
#' \item{status} {-- definition is missing.}
#' \item{condition} {--  character vector or \code{NA} (optional) that indicates
#' the restricted status of an animal. "Nonrestricted" animals
#   are generally assumed to be naive.}
#' \item{spf} {-- character vector or \code{NA} (optional) indicating the
#' specific pathogen-free status of an individual.}
#' \item{vasx.ovx} {-- character vector indicating the vasectomy/overiectomy
#' status of an animal where \code{NA} indicates an intact animal and all other
#' values inicate surgical alteration.}
#' \item{ped.num} {-- definition is missing.}}
#' @export
get_possible_cols <- function() {
  c("id", "sire", "dam", "sex", "gen", "birth", "exit", "age",
    "ancestry", "population", "origin", "status", "condition",
    "spf", "vasx.ovx", "ped.num")
}
#' Main pedigree curation function that performs basic quality control on
#' pedigree information
#'
#' @param sb : data.frame
#   Table of pedigree and demographic information
#   The function recognizes the following columns (optional columns
#   will be used if present, but are not required):
#
# id : char
#   Unique identifier for all individuals
# sire : char or NA
#   Identifier for the father of the current id
# dam : char or NA
#   Identifier for the mother of the current id
# sex : factor {levels: M, F, U}
#   Sex of the individual
# birth : date or NA (optional)
#   Date of birth
# departure : date or NA (optional)
#   Date an individual was sold or shipped from the colony
# death : date or NA (optional)
#   Date of death, if applicable
# status : factor {levels: ALIVE, DEAD, SHIPPED} (optional)
#   Status of an individual
# origin : char or NA (optional)
#   Facility an individual originated from, if other than ONPRC
# ancestry : char or NA (optional)
#   Geographic population to which the individual belongs
# spf : char or NA (optional)
#   Specific pathogen-free status of an individual
# vasx.ovx : char or NA (optional)
#   Indicator of the vasectomy/ovariectomy status of an animal; NA if
#   animal is intact, assume all other values indicate surgical alteration
# condition : char or NA (optional)
#   Indicator of the restricted status of an animal. "Nonrestricted" animals
#   are generally assumed to be naive.
#
#' @return a datatable with standardized and quality controlled pedigree
#' information.

#' @export
qc.Studbook <- function(sb) {
  headers <- tolower(names(sb))
  headers <- gsub(" ", "", headers)
  headers <- gsub("egoid", "id", headers)
  headers <- gsub("sireid", "sire", headers)
  headers <- gsub("damid", "dam", headers)

  # Checking for the 4 required fields (id, sire, dam, sex)
  if (is.na(match("id", headers))) {
    stop("No valid headers found")
  }

  names(sb) <- headers
  required_cols <- c("id", "sire", "dam", "sex")
  required <- required_cols %in% headers

  if (!all(required)) {
    stop(paste0("Required field missing (", paste0(required_cols[!required],
                                                   collapse = ", "), ")."))
  }

  required_cols <- c("age", "birth")
  required <- required_cols %in% headers
  if (!any(required)) {
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

  # converting date column entries from strings to datetime
  sb <- convertDates(sb)
  sb <- setExit(sb)

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
  cols <- intersect(get_possible_cols(), colnames(sb))
  sb <- sb[, cols]
  sb <- sb[with(sb, order(gen, id)), ]
  rownames(sb) <- seq(length = nrow(sb))

  # Ensuring the IDs are stored as characters
  sb$id <- as.character(sb$id)
  sb$sire <- as.character(sb$sire)
  sb$dam <- as.character(sb$dam)

  return(sb)
}

###############################################################################
# Helper Functions:
#' @export
addParents <- function(ped) {
  # Given a pedigree, find any IDs listed in the "sire" or "dam" columns
  # that lack their own line entry and generate one.
  #
  # Parameters
  # ----------
  # ped : Pedigree
  #   Pedigree data.frame of information for a group of animals
  #
  # Return
  # ------
  # Pedigree
  #   Updated Pedigree with entries added as necessary. Entries have the id
  #   and sex specified; all remaining columns are filled with NA.

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
#' @export
rbind.fill <- function(df1, df2) {
  # Appends the rows of df2 to df1, can handle cases where df2
  # has a subset of the columns of df1
  #
  # Parameters
  # ----------
  # df1 : data.frame
  #   The target data.frame to append to
  # df2 : data.frame
  #   The donor data.frame information should be appended from
  #
  # Return
  # ------
  # data.frame
  #   The appended data, with NA inserted into columns as needed

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
#' @export
checkParentSex <- function(id, sire, dam, sex) {
  # Updates sex for animals listed as either a sire or dam
  # Parameters
  # ----------
  # id : char
  #   Identifier of an animal
  # sire : char or NA
  #   Identifier of id's father
  # dam : char or NA
  #   Identifier of id's mother
  # sex : char, int, or NA
  #   Indicator of id's sex
  #
  # Return
  # ------
  # factor {levels: M, F, H, U}
  #   A vector of sex codes for the ids provided

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
#' @export
convertSexCodes <- function(sex, ignore.herm = TRUE) {
  # Converts sex indicator for an individual to a standardized code
  # {M, F, H, U}
  #
  # Parameters
  # ----------
  # sex : vector <char, int, or NA>
  #   Codes indicating sex of a set of individuals
  # ignore.herm : bool
  #   Flag indicating if hermaphrodites should be treated as
  #   unknown sex, default is true
  #
  # Return
  # ------
  # factor {levels: M, F, H, U}
  #   Modified vector containing standardized sex codes with the
  #   possible values M, F, U, & optionally, H

  sex <- toupper(sex)
  sex[is.na(sex)] <- "U"

  sex[sex %in% c("MALE", "M", "1")] <- "M"
  sex[sex %in% c("FEMALE", "F", "2")] <- "F"
  sex[sex %in% c("UNKNOWN", "U", "3")] <- "U"
  if (ignore.herm) {
    sex[sex %in% c("HERMAPHRODITE", "H", "4")] <- "U"
  }
  else{
    sex[sex %in% c("HERMAPHRODITE", "H", "4")] <- "H"
  }
  sex <- factor(sex, levels = c("F", "M", "H", "U"))

  return(sex)
}

#' @export
convertStatusCodes <- function(status) {
  # Converts status indicators to a Standardized code
  # Parameters
  # ----------
  # status : vector <char or NA>
  #   Flag indicating an individual's status as alive, dead,
  #   sold, etc.
  #
  # Return
  # ------
  # factor {levels: ALIVE, DECEASED, SHIPPED, UNKNOWN}
  #   Vector of standardized status codes with the possible values
  #   ALIVE, DECEASED, SHIPPED, or UNKNOWN

  status <- toupper(status)
  status[is.na(status)] <- "UNKNOWN"
  status[status %in% c("ALIVE", "A", "1")] <- "ALIVE"
  status[status %in% c("DECEASED", "DEAD", "D", "2")] <- "DECEASED"
  status[status %in% c("SHIPPED", "SOLD", "SALE", "S", "3")] <- "SHIPPED"
  status[status %in% c("UNKNOWN", "U", "4")] <- "UNKNOWN"

  status <- factor(status, levels = c("ALIVE", "DECEASED", "SHIPPED", "UNKNOWN"))
  return(status)
}

#' @export
convertAncestry <- function(ancestry) {
  # Converts the ancestry information to a standardized code
  # Parameters
  # ----------
  # ancestry : vector <char or NA>
  #   Free-form text providing information about the geographic
  #   population of origin
  #
  # Returns
  # -------
  # factor {levels: CHINESE, INDIAN, HYBRID, JAPANESE, OTHER, UNKNOWN}
  #   Vector of standardized designators specifying if an animal is a Chinese
  #   rhesus, Indian rhesus, Chinese-Indian hybrid rhesus, or Japanese macaque

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

#' @export
convertDates <- function(ped) {
  # Converts date columns formatted as characters to be of type datetime
  #
  # Parameters
  # ----------
  # ped : data.frame {opt. fields: birth, death, departure, exit}
  #   Table of pedigree information that may contain birth, death, departure
  #   or exit dates. The fields are optional, but will be used if present.
  #
  # Return
  # ------
  # data.frame
  #   Updated table with date columns converted from <char> to <Date>

  headers <- tolower(names(ped))

  if ("birth" %in% headers) {
    ped$birth <- as.Date(ped$birth, origin = TIME.ORIGIN)
  }
  if ("death" %in% headers) {
    ped$death <- as.Date(ped$death, origin = TIME.ORIGIN)
  }
  if ("departure" %in% headers) {
    ped$departure <- as.Date(ped$departure, origin = TIME.ORIGIN)
  }
  if ("exit" %in% headers) {
    ped$exit <- as.Date(ped$exit, origin = TIME.ORIGIN)
  }
  return(ped)
}

#' @export
setExit <- function(ped) {
  # Sets the exit date, if there is no exit column in the table
  #
  # Parameters
  # ----------
  # ped : data.frame {opt. columns: birth, death, departure}
  #   Table of pedigree and demographic information potentially containing
  #   columns indicating the birth and death dates of an individual. The
  #   table may also contain dates of sale (departure).
  #
  # Return
  # ------
  # data.frame
  #   An updated table with exit dates specified based on date information
  #   that was available.

  headers <- tolower(names(ped))

  if (("birth" %in% headers) && !("exit" %in% headers)) {
    if (("death" %in% headers) && ("departure" %in% headers)) {
	  # mapply simplifies results by default
	  # mapply would return a list, but simplification coerces this to a vector
	  # consequently, the simplification also coerces Date columns to Numeric
	  # TIME.ORIGIN is used to counter this and maintain Dates properly
      ped$exit <- as.Date(mapply(chooseDate, ped$death, ped$departure), origin = TIME.ORIGIN)
    }
    else if ("death" %in% headers) {
      ped$exit <- ped$death
    }
    else if ("departure" %in% headers) {
      ped$exit <- ped$departure
    }
    else{
      ped$exit <- as.Date(NA, origin = TIME.ORIGIN)
    }
  }
  return(ped)
}

#' @export
chooseDate <- function(d1, d2, earlier = TRUE) {
  # Given two dates, one is selected to be returned based on whether
  # it occurred earlier or later than the other. NAs are ignored if
  # possible.
  #
  # Parameters
  # ----------
  # d1, d2 : Date or NA
  #   The dates to compare
  # earlier : bool
  #   If TRUE, the earlier of the two dates is returned, otherwise
  #   the later is returned. Default is TRUE.
  #
  # Return
  # ------
  # Date or NA
  #   The chosen date, or NA if neither is provided

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

#' @export
calcAge <- function(birth, exit) {
  # Given vectors of birth and exit dates, calculate an individuals age;
  # calculates based on the current date, if no exit date is available
  #
  # Returns
  # -------
  # vector <float or NA>
  #   Age in decimal years from "birth" to "exit" or the current date
  #   if "exit" is NA

  exit[is.na(exit)] <- Sys.Date()
  return(round((as.double(exit - birth)/365.25), 1))
}

#' @export
findGeneration <- function(id, sire, dam) {
  # Determines the generation number for each id.
  #
  # Parameters
  # ----------
  # id : vector <char>
  #   IDs for a set of individuals
  # sire : vector <char or NA>
  #   IDs of the sires for the individuals in "id"
  # dam : vector <char or NA>
  #   IDs of the dams for the individuals in "id"
  #
  # Return
  # ------
  # vector <int>
  #   Generation numbers for each id, starting at 0 for
  #   individuals lacking IDs both parents.

  parents <- c()
  gen <- rep(NA, length(id))
  i <- 0

  while(TRUE) {
    cumulative.parents <- id[(is.na(sire) | (sire %in% parents)) &
                               (is.na(dam) | (dam %in% parents))]
    next.gen <- setdiff(cumulative.parents, parents)

    if (isEmpty(next.gen)) {
      break
    }

    gen[id %in% next.gen] <- i
    i <- i+1

    parents <- cumulative.parents
  }
  return(gen)
}

#' @export
isEmpty <- function(x) {
  # Returns true if x is a zero-length vector
  x <- x[!is.na(x)]
  return(length(x) == 0)
}

#' @export
removeDuplicates <- function(ped) {
  # Returns an updated data.frame with duplicate rows removed. Returns
  # an error if the table has duplicate IDs with differing data.
  # Parameters
  # ----------
  # ped : data.frame {req. col: id}
  #   Table of pedigree information
  #

  p <- unique(ped)

  if (sum(duplicated(p$id)) == 0) {
    return(p)
  }
  else{
    stop("Duplicate IDs with mismatched information present")
  }
}

#' @export
add.uIds <- function(ped) {
  # Eliminates partial parentage situations by adding unique placeholder
  # IDs for the unknown parent.
  # Parameters
  # ----------
  # ped : data.frame (req. fields: sire, dam)
  #   Data.frame of pedigree information
  #
  # Return
  # ------
  # data.frame
  #   The updated pedigree with partial parentage removed.

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

#' @export
findPedigreeNumber <- function(id, sire, dam) {
  # Determines the generation number for each id.
  #
  # Parameters
  # ----------
  # id : vector <char>
  #   IDs for a set of individuals
  # sire : vector <char or NA>
  #   IDs of the sires for the individuals in "id"
  # dam : vector <char or NA>
  #   IDs of the dams for the individuals in "id"
  #
  # Return
  # ------
  # vector <int>
  #   Generation numbers for each id, starting at 0 for
  #   individuals lacking IDs both parents.
  founders <- id[is.na(sire) & is.na(dam)]
  ped.num <- rep(NA, length(id))
  n <- 1

  while(!isEmpty(founders)) {
    population <- founders[1]

    while(TRUE) {
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
#' @export
trimPedigree <- function(probands, ped) {
  # Filters a pedigree down to only the ancestors of the provided group,
  # removing unnecessary individuals from the studbook. This version builds
  # the pedigree back in time starting from a group of probands. This will
  # include all ancestors of the probands, even ones that might be
  # uninformative.
  # Parameters
  # ----------
  # probands : vector <char>
  #   List of animals whose ancestors should be included in the final pedigree
  # ped : `Pedigree`
  #   Table of pedigree and demographic information
  #
  # Return
  # ------
  # `Pedigree`
  #   The reduced pedigree

  animals <- probands

  while(TRUE) {
    sires <- ped$sire[ped$id %in% animals]
    dams <- ped$dam[ped$id %in% animals]

    parents <- unique(union(sires, dams))
    parents <- parents[!is.na(parents)]
    added <- setdiff(parents, animals)

    if (identical(added, character(0))) {
      break
    }
    if (identical(added, numeric(0))) {
      break
    }
    if (identical(added, integer(0))) {
      break
    }
    animals <- union(animals, parents)
  }

  p <- ped[ped$id %in% animals, ]
  return(p)
}


#' @export
trimPedigree2 <- function(probands, ped) {
  # Filters a pedigree down to only the ancestors of the provided group,
  # removing unnecessary individuals from the studbook. This version builds
  # the pedigree back in time starting from a group of probands, then moves
  # back down the tree trimming off uninformative ancestors.
  # Parameters
  # ----------
  # probands : vector <char>
  #   List of animals whose ancestors should be included in the final pedigree
  # ped : `Pedigree`
  #   Table of pedigree and demographic information
  #
  # Return
  # ------
  # `Pedigree`
  #   The reduced pedigree

  animals <- probands

  while(TRUE) {
    sires <- ped$sire[ped$id %in% animals]
    dams <- ped$dam[ped$id %in% animals]

    parents <- unique(union(sires, dams))
    parents <- parents[!is.na(parents)]
    added <- setdiff(parents, animals)

    if (identical(added, character(0))) {
      break
    }
    if (identical(added, numeric(0))) {
      break
    }
    if (identical(added, integer(0))) {
      break
    }
    animals <- union(animals, parents)
  }

  ped <- ped[ped$id %in% animals, ]
  p <- ped

  while(TRUE) {
    founders <- p$id[is.na(p$sire) & is.na(p$dam)]

    sires <- as.data.frame(table(p$sire[p$sire %in% founders]))
    dams <- as.data.frame(table(p$dam[p$dam %in% founders]))
    sires$Var1 <- as.character(sires$Var1)
    dams$Var1 <- as.character(dams$Var1)

    rmv <- c(sires$Var1[sires$Freq == 1], dams$Var1[dams$Freq == 1])
    if (isEmpty(rmv)) {
      break
    }

    p$sire[p$sire %in% rmv] <- NA
    p$dam[p$dam %in% rmv] <- NA
    p <- p[!(p$id %in% rmv), ]

  }

  # Adding back second parents where one is known
  single.parents <- p$id[(is.na(p$sire) & !is.na(p$dam)) |
                           (!is.na(p$sire) & is.na(p$dam))]

  add.back <- c()
  for (id in single.parents) {
    if (!is.na(ped$sire[ped$id==id]) & !is.na(ped$dam[ped$id==id])) {

      if (is.na(p$sire[p$id==id])) {
        add.back <- c(add.back, ped$sire[ped$id==id])
        p[(p$id==id), "sire"] <- ped$sire[ped$id==id]

      } else{
        add.back <- c(add.back, ped$dam[ped$id==id])
        p[(p$id==id), "dam"] <- ped$dam[ped$id==id]
      }
    }
  }
  add.back <- ped[(ped$id %in% add.back), ]
  add.back$sire <- NA
  add.back$dam <- NA

  p <- rbind(p, add.back)

  return(p)
}

###############################################################################
# Population Designation Functions:
#' @export
resetPopulation <- function(ids, ped) {
  # Update or add the "population" field of a Pedigree
  # @type   ids: vector (string)
  # @param  ids: List of IDs to be flagged as part of the population
  # @type   ped: Pedigree
  # @param  ped: The complete pedigree provided
  #
  # @rtype:      Pedigree
  # @return:     Updated pedigree

  ped$population <- FALSE

  if (length(ids) == 0) {
    ped$population <- TRUE
  } else{
    ped$population[ped$id %in% ids] <- TRUE
  }
  return(ped)
}

#' @export
resetGroup <- function(ids, ped) {
  # Update or add the "group" field of a Pedigree
  # @type   ids: vector (string)
  # @param  ids: List of IDs to be flagged as part of the group under
  #              consideration
  # @type   ped: Pedigree
  # @param  ped: The complete pedigree provided
  #
  # @rtype:      Pedigree
  # @return:     Updated pedigree

  ped$group <- FALSE
  ped$group[ped$id %in% ids] <- TRUE
  return(ped)
}

