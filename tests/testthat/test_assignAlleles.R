context("assignAlleles")
library(testthat)
pedOne <- data.frame(id = c("s1", "d1", "s2", "d2", "o1", "o2", "o3", "o4"),
                     sire = c(NA, "s0", "s4", NA, "s1", "s1", "s2", "s2"),
                     dam = c(NA, "d0", "d4", NA, "d1", "d2", "d2", "d2"),
                     sex = c("M", "F", "M", "F", "F", "F", "F", "M"),
                     stringsAsFactors = FALSE)
pedTwo <- data.frame(id = c("s1", "d1", "s2", "d2", "o1", "o2", "o3", "o4"),
                     sire = c(NA, "s0", "s4", NA, "s1", "s1", "s2", "s2"),
                     dam = c("d0", "d0", "d4", NA, "d1", "d2", "d2", "d2"),
                     sex = c("M", "F", "M", "F", "F", "F", "F", "M"),
                     stringsAsFactors = FALSE)

pedThree <- data.frame(id = c("s1", "d1", "s2", "d2", "o1", "o2", "o3", "o4"),
                       sire = c("s0", "s0", "s4", NA, "s1", "s1", "s2", "s2"),
                       dam = c(NA, "d0", "d4", NA, "d1", "d2", "d2", "d2"),
                       sex = c("M", "F", "M", "F", "F", "F", "F", "M"),
                       stringsAsFactors = FALSE)
alleles <- list(alleles = list(), counter = 1)
test_that("assignAlleles assigns alleles correctly", {
  expect_error(assignAlleles(alleles, parentType = "sire", parent = "s1",
                             id = "o1", n = 4))
  alleles_2 <- assignAlleles(alleles, parentType = "sire", parent = NA,
                             id = "o1", n = 4)
  expect_equal(alleles_2$alleles$o1$sire, rep(1, 4))
})
