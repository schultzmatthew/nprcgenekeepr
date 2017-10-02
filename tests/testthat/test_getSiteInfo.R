context("getSiteInfo")
library(testthat)
test_that("getSiteInfo at least returns the right elements", {
  expect_equal(names(getSiteInfo()),
               c("center", "baseUrl", "schemaName", "folderPath", "queryName",
                 "lkPedColumns", "mapPedColumns", "sysname", "release",
                 "version", "nodename", "machine", "login", "user",
                 "effective_user", "homeDir", "configFile"))
})

