#' Creates a pyramid plot of the pedigree provided.
#'
#' The pedigree provided must have the following columns: \code{sex} and
#' \code{age}. This needs to be augmented to allow pedigrees structures that
#' are provided by the nprcmanager package.
#' @param ped dataframe with pedigree data.
#' @import lubridate
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
