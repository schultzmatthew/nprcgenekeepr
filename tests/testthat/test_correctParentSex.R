context("correctParentSex")
library(testthat)
pedOne <- data.frame(id = c("s1", "d1", "s2", "d2", "o1", "o2", "o3", "o4"),
                     sire = c(NA, "s0", "s4", NA, "s1", "s1", "s2", "s2"),
                     dam = c(NA, "d0", "d4", NA, "d1", "d2", "d2", "d2"),
                     sex = c("F", "F", "M", "F", "F", "F", "F", "M"),
                     stringsAsFactors = FALSE)
pedTwo <- data.frame(id = c("s1", "d1", "s2", "d2", "o1", "o2", "o3", "o4"),
                     sire = c(NA, "s0", "s4", NA, "s1", "s1", "s2", "s2"),
                     dam = c("d0", "d0", "d4", NA, "d1", "d2", "d2", "d2"),
                     sex = c("M", "M", "M", "F", "F", "F", "F", "M"),
                     stringsAsFactors = FALSE)
pedThree <- data.frame(id = c("s1", "d1", "s2", "d2", "o1", "o2", "o3", "o4"),
                     sire = c(NA, "s0", "s4", NA, "s1", "s1", "s2", "s2"),
                     dam = c("d0", "d0", "d4", NA, "d1", "d2", "s1", "d2"),
                     sex = c("M", "M", "M", "F", "F", "F", "F", "M"),
                     stringsAsFactors = FALSE)

pedOne$sex <- correctParentSex(pedOne$id, pedOne$sire, pedOne$dam, pedOne$sex)
pedTwo$sex <- correctParentSex(pedTwo$id, pedTwo$sire, pedTwo$dam, pedTwo$sex)
test_that("correctParentSex makes correct changes", {
  expect_true(pedOne$sex[1] == "M")
  expect_true(pedTwo$sex[2] == "F")
  expect_error(correctParentSex(pedThree$id, pedThree$sire, pedThree$dam,
                                pedThree$sex))
})
