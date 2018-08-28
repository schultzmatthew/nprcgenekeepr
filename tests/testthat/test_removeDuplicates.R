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
test_that("removeDuplicates returns NULL errors flag == TRUE when there are no duplicates", {
  expect_true(is.null(removeDuplicates(ped, errors = TRUE)))
  ped <- rbind(ped, ped[1:3, ])
  ped1 <- removeDuplicates(ped, errors = TRUE)
  expect_equal(ped1, c("A", "B", "C"))
  ped <- smallPed
  ped2 <- ped[1:3, ]
  ped2$dam[[1]] <- "B"
  ped <- rbind(ped, ped2)
  expect_equal(removeDuplicates(ped, errors = TRUE), c("A", "B", "C"))
})
