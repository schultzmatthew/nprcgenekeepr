context("removeDuplicates")
library(testthat)
data("smallPed")
ped <- smallPed

test_that("removeDuplicates removes nothing with no duplicates", {
  ped1 <- removeDuplicates(ped)
  expect_equal(nrow(ped), nrow(ped1))
  ped <- rbind(ped, ped[1:3, ])
  ped1 <- removeDuplicates(ped)
  expect_equal(nrow(ped) - 3, nrow(ped1))
  ped <- smallPed
  ped2 <- ped[1:3, ]
  ped2$dam[[1]] <- "B"
  ped <- rbind(ped, ped2)
  expect_error(removeDuplicates(ped))
})
