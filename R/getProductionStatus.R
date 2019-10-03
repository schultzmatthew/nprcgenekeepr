#' Get production status of group
#'
#' @return \code{production} -- Ratio of the number of births that live >30 days to
#' the number of females >= 3 years of age.
#'
#' @details Description of how Production and Production Status (color) is
#' calculated.
#' \enumerate{
#' \item  The Production Status is calculated on September 09, 2019.
#' \item  Births = count of all animals in group born since January 1,
#'     2017 through August 10, 2019, that lived at least 30 days.
#'     Animals born after August 10, 2019, are not counted since they have not
#'     lived 30 days.
#' \item  Dams = count of all females in group that have a birth date on or
#'        prior to September 09, 2016.
#' \item  Production = Births / Dams
#' \item  Production Status (color)
#'     \enumerate{
#'     \item  Shelter and pens
#'         \enumerate{
#'         \item   Production < 0.6; Red
#'         \item   Production >= 0.6 and Production <= 0.63; Yellow
#'         \item   Production > 0.63; Green
#'     }
#'     \item  Corrals
#'         \enumerate{
#'         \item   Production < 0.5; Red
#'         \item   Production >= 0.5 and Production <= 0.53; Yellow
#'         \item   Production > 0.53; Green
#'         }
#'     }
#'  }
#'
#' @param ped Dataframe that is the `Pedigree`. It contains pedigree
#' information. The \code{id}, \code{dam}, \code{sex} and \code{age}
#' (in years) columns are required.
#' @param minParentAge Numeric values to set the minimum age in years for
#' an animal to have an offspring. Defaults to 2 years. The check is not
#' performed for animals with missing birth dates.
#' @param maxOffspringAge Numeric values to set the maximum age in years for
#' an animal to be counted as birth in calculation of production status
#' ratio.
#' @param housing character vector of length 1 having the housing type, which
#' is either \emph{"shelter_pens"} or \emph{"corral"}.
#' @param currentDate Date to be used for calculating age. Defaults to
#'        \code{Sys.Date()}.
#' @export
getProductionStatus <- function(ped, minParentAge = 3, maxOffspringAge = NULL,
                                housing = "shelter_pens",
                                currentDate = Sys.Date()) {
  expectedCols <- c("id", "dam", "sex", "age")
  if (!all(expectedCols %in%
           names(ped))) {
    missingCol <- expectedCols[!expectedCols %in% names(ped)]
    stop(paste0("ped is missing: ", missingCol))
  }
  nDam <- nrow(ped[ped$sex == "F" & ped$age >= minParentAge, ])
  if (is.null(maxOffspringAge)) {
    maxOffspringAge <- mdy(paste0("1/1/", year(currentDate) - 2))
    startDate <- mdy(paste0("1/1/", year(Sys.Date()) - 2))
    dur <- as.duration(interval(startDate, currentDate))
    maxOffspringAge <- as.numeric(dur, "years")
  }
  nOffspring <- nrow(ped[ped$age <= maxOffspringAge, ])

  if (nDam > 0) {
    production <- nOffspring / nDam
  } else {
    production <- NA
  }

  if (housing == "shelter_pens") {
    if (production < 0.6) {
      color <- "red"
    } else if (production >= 0.6 & production <= 0.63) {
      color <- "yellow"
    } else if (production > 0.63) {
      color <- "green"
    }
  } else if (housing == "corral") {
    if (production < 0.5) {
      color <- "red"
    } else if (production >= 0.5 & production <= 0.53) {
      color <- "yellow"
    } else if (production > 0.53) {
      color <- "green"
    }
  } else {
    stop(paste0("Undefined housing type in getProduction status is: ",
                housing))
  }
  list(production = production, color = color)
}
