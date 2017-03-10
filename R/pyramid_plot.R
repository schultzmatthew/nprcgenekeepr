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
#' @import lubridate
#' @export
get_age_dist <- function(ped = ped()) {
  ped <- ped[tolower(ped$Status) == "alive", c("Id", "Birth", "Sex", "Status")]
  ped <- dplyr::rename(ped, id = Id, birth_date = Birth, sex = Sex)
  ped$age <- interval(start = ped$birth_date, end = now()) /
    duration(num = 1, units = "years")
  ped
}
