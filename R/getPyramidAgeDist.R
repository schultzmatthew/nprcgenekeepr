#' Get the age distribution for the pedigree
#'
#' Forms a dataframe with columns \code{id}, \code{birth}, \code{sex},
#' and {age} for those animals with a status of \code{Alive} in the pedigree.
#'
#' The lubridate package is used here because of the way the modern Gregorian
#' calendar is constructed, there is no straightforward arithmetic method
#' that produces a person’s age, stated according to common usage — common
#' usage meaning that a person’s age should always be an integer that
#' increases exactly on a birthday.
#' @param ped dataframe with pedigree
#' @import lubridate
#' @importFrom utils read.csv
#' @export
getPyramidAgeDist <- function(ped = "null") {
  if (ped == "null") {
    ped <- tryCatch(read.csv(file =
                               "../extdata/snprc_baboon_ped.csv",
                             stringsAsFactors = FALSE),
                    warning = function(cond) {
                      stop(paste0(cond, "\nWorkding directory: ", getwd()))
                    },
                    error = function(cond) {
                      stop(paste0(cond, "\nWorkding directory: ", getwd()))
                    })
    #stop(paste0(getwd()))
  }
  # ped <- ped[tolower(ped$EXIT) == "", c("EGO.ID", "SIRE.SIRE.ID",
  #                                              "DAM.ID", "SEX", "BIRTH",
  #                                              "EXIT")]
  names(ped) <- c("id", "sire", "dam", "sex", "birth", "exit_date")
  ped$birth <- ymd(ped$birth)
  ped$age <- NA
  ped$status <- NA
  ped$status[ped$exit_date == "9999999999"] <- "DECEASED"
  ped$status[ped$exit_date == ""] <- "ALIVE"
  ped$exit_date[ped$exit_date == "" | ped$exit_date == "9999999999"] <- NA
  ped$status[!is.na(ped$exit_date)] <- "DECEASED"
  #ped$exit_date[!is.na(ped$exit_date)] <-
  #  ymd(ped$exit_date[!is.na(ped$exit_date)])
  ped$age[is.na(ped$exit_date) & !is.na(ped$birth)] <-
    interval(start = ped$birth[is.na(ped$exit_date) &
                                 !is.na(ped$birth)],
             end = now()) / duration(num = 1, units = "years")
  ped$age[!is.na(ped$exit_date) & !is.na(ped$birth)] <-
    interval(start = ped$birth[!is.na(ped$exit_date) &
                                 !is.na(ped$birth)],
             end = ymd(ped$exit_date[!is.na(ped$exit_date) &
                                       !is.na(ped$birth)])) /
    duration(num = 1, units = "years")
  names(ped)[names(ped) == "exit_date"] <- "exit"
  ped
}
