context("checkParentAge")
library(testthat)
data("baboonPed")
test_that("checkParentAge identifies the over aged parents", {
  underAgeTwo <- checkParentAge(baboonPed, minParentAge = 2)
  underAgeThree <- checkParentAge(baboonPed, minParentAge = 3)
  underAgeFive <- checkParentAge(baboonPed, minParentAge = 5)
  underAgeSix <- checkParentAge(baboonPed, minParentAge = 6)
  underAgeTen <- checkParentAge(baboonPed, minParentAge = 10)
  expect_equal(nrow(underAgeTwo), 0)
  expect_equal(nrow(underAgeThree), 0)
  expect_equal(nrow(underAgeFive), 1)
  expect_equal(nrow(underAgeSix), 6)
  expect_true(all(underAgeSix$dam %in% c("15057", "16135", "1C0321", "8143",
                                         "8846", "9859")))
  expect_true(
    all(underAgeTen$sire[underAgeTen$sireAge < 10 &
                           !is.na(underAgeTen$sireAge)] %in%
          c("15797", "13644", "26318", "12656", "14991", "28279", "15010",
            "28729", "1X1672", "1X0832", "1X3162", "1X0839", "1X1126", "1X2816",
            "1X3818", "1X3026", "10173", "8780", "8653")))
})
test_that("checkParentAge requires birth column to be potential date", {
  ped <- baboonPed
  ped$birth <- ped$birth > "2000-01-01"
  expect_error(checkParentAge(ped, minParentAge = 3))
}
)
test_that("checkParentAge allows birth column to be character", {
  ped <- baboonPed
  ped$birth <- format(ped$birth, format = "%Y-%m-%d")
  expect_equal(nrow(checkParentAge(ped, minParentAge = 6)), 6)
  ped <- baboonPed
  ped$birth <- format(ped$birth, format = "%m-%d-%Y")
  expect_equal(nrow(checkParentAge(ped, minParentAge = 6)), 6)
})
test_that("checkParentAge returns unchanged dataframe is require column is missing", {
  ped <- baboonPed[ , !names(baboonPed) %in% "id"]
  expect_equal(ncol(ped), ncol(baboonPed[ , !names(baboonPed) %in% "id"]))
  expect_equal(ped, baboonPed[ , !names(baboonPed) %in% "id"])
})
