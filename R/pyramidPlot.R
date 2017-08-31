#' Form age pyramid plot
#'
#' @param males integer vector with the number of males in age groups
#' corresponding to the position in the vector
#' @param females integer vector with the number of females in age groups
#' corresponding to the position in the vector
#' @param age_labels character vector of labels for the categories represented
#' by each pair of bars. There should be a label for each lx or rx value,
#' even if empty. If labels is a matrix or data frame, the first two columns
#' will be used for the left and right category labels respectively.
#' @param mcol color for the male (left) side of the plot
#' @param fcol color for the female (right) side of the plot
#' @param laxlab label for the male (left) side of the plot
#' @param raxlab label for the female (right) side of the plot
#' @param gap numeric value for one half of the space between the two sets
#' of bars for the \code{age_labels} in user units
#' @param current_date POSIXct date object indicating the date corresponding to
#' the date the pedigree census occurred.
#' @import lubridate
#' @import stringi
#' @import plotrix
#' @export
agePyramidPlot <- function(males, females, age_labels, mcol, fcol, laxlab,
                             raxlab, gap, current_date) {
  pyramid.plot(lx = males, rx = females, labels = age_labels,
               main = stri_c("Total on ",
                           stri_c(year(current_date), "-",
                                 month(current_date, label = TRUE), "-",
                                 stri_sub(stri_c(current_date), -2)),
                           ": ", sum(c(males, females))),
               top.labels = c(
                 stri_c('Male = ', sum(males)),
                 'Age',
                 stri_c('Female = ', sum(females))),
               lxcol = mcol,rxcol = fcol,
               laxlab = laxlab,
               raxlab = raxlab,
               gap = gap,
               # use for PT species
               # gap=40,
               # laxlab = seq(0, 100, by = 10),
               # raxlab = seq(0, 100, by = 10),
               unit = "Number of Animals",
               show.values = TRUE)
}
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
#' Get the maximum age of live animals in the pedigree.
#'
#' @param ped dataframe with pedigree
#' @export
getPedMaxAge <- function(ped) {
  max(ped$age, na.rm = TRUE)
}
#' Round up the provided integer vector \code{int} according to the
#' \code{modulas}.
#'
#' @param int integer vector
#' @param modulas integer value to use as the divisor.
#' @export
makeRoundUp <- function(int, modulas) {
  int + modulas - int %% modulas
}
#' Fill bins represented by list of two lists \code{males} and \code{females}.
#'
#'
#' @param age_dist dataframe with \code{sex} and \code{age} columns
#' @param lower_ages integer vector of lower age boundaries; must be the same
#' length as \code{upper_ages}
#' @param upper_ages integer vector of upper age boundaries; must be the same
#' length as \code{lower_ages}
#' @export
fillBins <- function(
  age_dist, lower_ages,
  upper_ages = lower_ages + c(lower_ages[2:length(lower_ages)], 100)) {
  male_bins <- c()
  female_bins <- c()
  for (bin in seq_along(lower_ages)) {
    male_bins <- c(male_bins,
                   nrow(age_dist[age_dist$sex == 'M' &
                                   age_dist$age >= lower_ages[bin] &
                                   age_dist$age < upper_ages[bin], ]))
    female_bins <- c(female_bins,
                     nrow(age_dist[age_dist$sex == 'F' &
                                     age_dist$age >= lower_ages[bin] &
                                     age_dist$age < upper_ages[bin], ]))
  }
  list(males = male_bins, females = female_bins)
}
getMaxAx <- function(bins, ax_modulas) {
  makeRoundUp(max(max(bins$male), max(bins$female)), ax_modulas)
}
#' Creates a pyramid plot of the pedigree provided.
#'
#' The pedigree provided must have the following columns: \code{sex} and
#' \code{age}. This needs to be augmented to allow pedigrees structures that
#' are provided by the nprcmanager package.
#' @param ped dataframe with pedigree data.
#' @import plotrix
#' @import stringi
#' @importFrom graphics par
#' @export
getPyramidPlot <- function(ped = NULL) {

  if (is.null(ped))
    ped <- getPyramidAgeDist()
  par(bg = "#FFF8DC")
  bin_width <- 2
  ax_modulas <- 5
  upper_ages <- seq(bin_width,
                    makeRoundUp(getPedMaxAge(ped), bin_width), bin_width)
  lower_ages <- upper_ages - bin_width

  bins <- fillBins(ped, lower_ages, upper_ages)
  max_ax <- max(getMaxAx(bins, ax_modulas))
  age_labels <- stri_c(lower_ages, " - ", upper_ages - 1)
  mcol <- color.gradient(0, 0,   0.5)
  fcol <- color.gradient(1, 0.5, 0.5)
  current_date <- now()
  ax_by <- max_ax / ax_modulas
  ax_gap <- ax_by * 0.6
  gap <- ax_gap
  laxlab <- seq(0, max_ax, by = ax_by)
  raxlab <- seq(0, max_ax, by = ax_by)
  agePyramidPlot(bins$males, bins$females, age_labels, mcol, fcol,
                  laxlab, raxlab, gap, current_date)

  par(bg = "transparent")

}
# ids <- c("31879", "31882", "31900", "31902", "31938", "31940", "31951",
#          "31952", "31966", "31970", "31980", "32043", "32130", "32132",
#          "32303", "31648", "31659", "31660", "31738", "31740", "31764",
#          "31786", "31792", "31800", "31817", "31844", "31852", "31993",
#          "32040")
# base_url <- "https://vger.txbiomed.org/labkey"
# max_rows <- 0
#' Get parents to corresponding animal IDs provided
#'
#' @param ped_source_df dataframe with pedigree structure having at least the
#' columns id, sire, and dam.
#' @param ids character vector of animal IDs
#' @export
getParents <- function(ped_source_df, ids) {
  unique(c(ped_source_df$sire[(ped_source_df$id %in% ids &
                             !is.na(ped_source_df$sire))],
           ped_source_df$dam[(ped_source_df$id %in% ids &
                             !is.na(ped_source_df$dam))]))
}
#' Get direct ancestors from labkey \code{study} schema and \code{demographics}
#' table.
#'
#' @param base_url character vector of length 1 having the base URL
#' (baseUrl) of the LabKey server
#' @param folder_path character vector of length 1 having the LabKey folder path
#' (folderPath)
#' @param ids character vector with an animal ID in each cell.
#' @importFrom Rlabkey labkey.selectRows
#' @export
getLkDirectAncestors <- function(base_url, folder_path, ids) {
  ped_source_df <- labkey.selectRows(
    baseUrl = base_url,
    folderPath = folder_path,
    schemaName = "study",
    queryName = "demographics",
    viewName = "",
    #colSort = "-Date",
    colFilter = NULL,
    containerFilter = NULL,
    colNameOpt = "fieldname",
    maxRows = NULL,
    colSelect = c("Id", "date", "gender", "species", "birth", "death",
                  "lastDayAtCenter", "calculated_status", "dam", "sire",
                  "origin", "parentid" , "species/arc_species_code"),
    showHidden = TRUE)
  names(ped_source_df)[names(ped_source_df) == "Id"] <- "id"
  names(ped_source_df)[names(ped_source_df) == "gender"] <- "sex"
  parents <- ids
  len <- length(parents)
  ancestors_df <- ped_source_df[ped_source_df$id %in% ids, ]
  while (len > 0) {
    parents <- getParents(ped_source_df, parents)
    len <- length(parents)
    if (len > 0) {
      ancestors_df <- rbind(ancestors_df,
                            ped_source_df[ped_source_df$id %in% parents, ])
    }
  }
  ancestors_df
}
# test <- getLkDirectAncestors(base_url = base_url, folder_path = "/SNPRC",
#                                 ids = ids)
# test[!test$id %in% deb_df$EGO.ID, ]
# deb_df[!deb_df$EGO.ID %in% test$id, ]
