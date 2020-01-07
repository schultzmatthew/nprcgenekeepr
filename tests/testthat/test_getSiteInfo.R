#' Copyright(c) 2017-2019 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("getSiteInfo")
library(stringi)
test_that("getSiteInfo at least returns the right elements", {
  expect_equal(suppressWarnings(names(getSiteInfo())),
               c("center", "baseUrl", "schemaName", "folderPath", "queryName",
                 "lkPedColumns", "mapPedColumns", "sysname", "release",
                 "version", "nodename", "machine", "login", "user",
                 "effective_user", "homeDir", "configFile"))
})
test_that("getSiteInfo handled Windows and non-windows opperating systems", {
  siteInfo <- suppressWarnings(getSiteInfo())
  if (stri_detect_fixed(toupper(siteInfo$sysname), "WIND")) {
    expect_equal(siteInfo$homeDir, paste0("/Users/", siteInfo$user, "/"))
    expect_equal(siteInfo$configFile, paste0("/Users/", siteInfo$user,"/",
                                    "_nprcgenekeepr_config"))
  } else {
    expect_equal(siteInfo$homeDir, "~/")
    expect_equal(siteInfo$configFile, "~/.nprcgenekeepr_config")
  }
})
test_that("getSiteInfo handle expectConfigFile parameter", {
  expect_warning(test <- getSiteInfo())
  expect_warning(test <- getSiteInfo(expectConfigFile = TRUE))
  expect_silent(test <- getSiteInfo(expectConfigFile = FALSE))
})
