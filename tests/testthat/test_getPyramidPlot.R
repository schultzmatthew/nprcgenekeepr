context("getPyramidPlot")
library(testthat)
data("baboonPed")
recPlot <- function(expr) {
  pdf(NULL)
  on.exit(dev.off())
  dev.control(displaylist="enable")
  expr
  recordPlot()
}
agePlot <- recPlot(getPyramidPlot(baboonPed))
test_that("getPyramidPlot generates a plot", {

  expect_equal(class(agePlot), "recordedplot")
})
