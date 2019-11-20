#' Creates a pyramid plot of the pedigree provided.
#'
## Copyright(c) 2017-2019 R. Mark Sharp
## This file is part of nprcmanager
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
  binWidth <- 2
  axModulas <- 5
  upperAges <- seq(binWidth,
                    makeRoundUp(getPedMaxAge(ped), binWidth), binWidth)
  lowerAges <- upperAges - binWidth

  bins <- fillBins(ped, lowerAges, upperAges)
  maxAx <- max(getMaxAx(bins, axModulas))
  ageLabels <- stri_c(lowerAges, " - ", upperAges - 1)
  mcol <- color.gradient(0, 0,   0.5)
  fcol <- color.gradient(1, 0.5, 0.5)
  currentDate <- now()
  axBy <- maxAx / axModulas
  axGap <- axBy * 0.6
  gap <- axGap
  laxlab <- seq(0, maxAx, by = axBy)
  raxlab <- seq(0, maxAx, by = axBy)
  agePyramidPlot(bins$males, bins$females, ageLabels, mcol, fcol,
                  laxlab, raxlab, gap, currentDate)

  par(bg = "transparent")

}
