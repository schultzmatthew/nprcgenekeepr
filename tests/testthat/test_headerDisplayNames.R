context("headerDisplayNames")
library(testthat)
test_that("headerDisplayNames makes the correct mapping", {
  expect_equal(headerDisplayNames(c("id", "sire", "dam", "gen")),
               c("Ego ID", "Sire ID", "Dam ID", "Generation #"))
  expect_equal(headerDisplayNames(c("spf", "second_name", "value")),
               c("SPF", "Second Allele", "Value Designation"))
})

