context("resetPopulation")
library(testthat)
data("smallPed")
ped <- smallPed
test_that("resetPopulation sets correct records to TRUE and FALSE", {
  ped1 <- resetPopulation(ped = ped, ids = NULL)
  expect_true(all(ped1$population))
  ped1 <- resetPopulation(ped = ped, ids = c("A", "B", "I"))
  expect_true(all(ped1$population[ped1$id %in%
                    c("A", "B", "I")]))
  expect_true(length(setdiff(ped1$id[ped1$population], c("A", "B", "I"))) == 0)
})
