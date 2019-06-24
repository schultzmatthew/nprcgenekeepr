context("summary.nprcmanagGV")
library(testthat)

test_that("summary.gvReport provides expected output", {
  skip_on_cran()
  set_seed(10)
  ped <- nprcmanager::pedOne
  ped$birth_date[ped$ego_id == "d2"] <- "2000-04-13"
  ped$birth_date[ped$ego_id == "o4"] <- "2016-04-13"
  ped <- suppressWarnings(qcStudbook(ped, reportErrors = FALSE))
  gvReport <- reportGV(ped)
  summaryGV <- summary(gvReport, guIter = 10)
  expect_equal(names(summaryGV[1, ]), c("Length", "Class", "Mode"))
  expect_equal(as.character(summaryGV[1, ]), c("13", "data.frame", "list"))
})
