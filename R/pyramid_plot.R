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
age_pyramid.plot <- function(males, females, age_labels, mcol, fcol, laxlab,
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
#' Forms a dataframe with columns \code{id}, \code{birth_date}, \code{sex},
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
get_pyramide_age_dist <- function() {
  ped <- read.csv(file = "/Users/msharp/Desktop/2cage_bab_brdrs_ped.csv",
                  stringsAsFactors = FALSE)
  # ped <- ped[tolower(ped$EXIT) == "", c("EGO.ID", "SIRE.SIRE.ID",
  #                                              "DAM.ID", "SEX", "BIRTH",
  #                                              "EXIT")]
  names(ped) <- c("id", "sire_id", "dam_id", "sex", "birth_date", "exit_date")
  ped$birth_date <- ymd(ped$birth_date)
  ped$age <- NA
  ped$exit_date[ped$exit_date == "" | ped$exit_date == "9999999999"] <- NA
  #ped$exit_date[!is.na(ped$exit_date)] <-
  #  ymd(ped$exit_date[!is.na(ped$exit_date)])
  ped$age[is.na(ped$exit_date) & !is.na(ped$birth_date)] <-
    interval(start = ped$birth_date[is.na(ped$exit_date) &
                                      !is.na(ped$birth_date)],
             end = now()) / duration(num = 1, units = "years")
  ped
}
#' Get the maximum age of live animals in the pedigree.
#'
#' @param ped dataframe with pedigree
#' @export
get_ped_max_age <- function(ped) {
  max(ped$age, na.rm = TRUE)
}
#' Round up the provided integer vector \code{int} according to the
#' \code{modulas}.
#'
#' @param int integer vector
#' @param modulas integer value to use as the divisor.
#' @export
make_round_up <- function(int, modulas) {
  int + modulas - int %% modulas
}
#' Fill bins represented by list of two lists \code{males} and \code{females}.
#'
#'
#' @param age_dist
#' @param lower_ages
#' @param upper_ages
#' @export
fill_bins <- function(age_dist, lower_ages,
                      upper_ages = lower_ages + bin_width) {
  male_bins <- c()
  female_bins <- c()
  for (bin in seq_along(lower_ages)) {
    male_bins <- c(male_bins, nrow(age_dist[age_dist$sex == 'M' &
                                              age_dist$age >= lower_ages[bin] &
                                              age_dist$age < upper_ages[bin], ]))
    female_bins <- c(female_bins, nrow(age_dist[age_dist$sex == 'F' &
                                                  age_dist$age >= lower_ages[bin] &
                                                  age_dist$age < upper_ages[bin], ]))
  }
  list(males = male_bins, females = female_bins)
}
get_max_ax <- function(bins, ax_modulas) {
  make_round_up(max(max(bins$male), max(bins$female)), ax_modulas)
}
library(stringi)
library(plotrix)

par(bg = "#FFF8DC")
bin_width <- 2
ax_modulas <- 5
ped <- get_pyramide_age_dist()
upper_ages <- seq(bin_width,
                  make_round_up(get_ped_max_age(ped), bin_width), bin_width)
lower_ages <- upper_ages - bin_width

bins <- fill_bins(ped, lower_ages, upper_ages)
max_ax <- max(get_max_ax(bins, ax_modulas))
age_labels <- stri_c(lower_ages, " - ", upper_ages - 1)
mcol <- color.gradient(0, 0,   0.5)
fcol <- color.gradient(1, 0.5, 0.5)
current_date <- now()
ax_by <- max_ax / ax_modulas
ax_gap <- ax_by * 0.6
gap <- ax_gap
laxlab <- seq(0, max_ax, by = ax_by)
raxlab <- seq(0, max_ax, by = ax_by)
age_pyramid.plot(bins$males, bins$females, age_labels, mcol, fcol,
                laxlab, raxlab, gap, current_date)

par(bg = "transparent")
