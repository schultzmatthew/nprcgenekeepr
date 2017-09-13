context("getColumnVec")
library(testthat)
lines <- c("center = \"SNPRC\"",
           " baseUrl = \"https://boomer.txbiomed.org/labkey\"",
           " schemaName = \"study\"", " folderPath = \"/SNPRC\"",
           " queryName = \"demographics\"",
           "lkPedColumns = (\"Id\", \"gender\", \"birth\", \"death\",",
           "              \"lastDayAtCenter\", \"dam\", \"sire\")",
           "mapPedColumns = (\"id\", \"sex\", \"birth\", \"death\", ",
           "  \"exit\", \"dam\", \"sire\")")
original <- lines
lkVec <- c("Id", "gender", "birth", "death",
           "lastDayAtCenter", "dam", "sire")
mapVec <- c("id", "sex", "birth", "death", "exit", "dam", "sire")

test_that("getColumnVec returns correct lines and vectors", {
  expect_equal(lines[7], original[7])
  lines_and_vec <- getColumnVec(lines, "lkPedColumns")
  lines <- lines_and_vec$lines
  lkPedColumns <- lines_and_vec$vec
  expect_equal(lines[1], "center = \"SNPRC\"")
  expect_equal(lines[5], original[7])
  expect_equal(lkPedColumns, lkVec)
  lines_and_vec <- getColumnVec(lines, "lkPedColumns")
  lines <- lines_and_vec$lines
  mapPedColumns <- lines_and_vec$vec
  expect_equal(lines[1], "center = \"SNPRC\"")
  expect_equal(length(lines), 4)
  expect_equal(mapPedColumns, mapVec)

})
