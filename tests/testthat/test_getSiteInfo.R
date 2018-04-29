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
  siteInfo <- getSiteInfo()
  if (stri_detect_fixed(toupper(siteInfo$sysname), "WIND")) {
    expect_equal(siteInfo$homeDir, paste0("/Users/", siteInfo$user))
    expect_equal(siteInfo$configFile, paste0(paste0("/Users/", siteInfo$user),
                                    "_nprcmanager_config"))
  } else {
    expect_equal(siteInfo$homeDir, "~/")
    expect_equal(siteInfo$configFile, "~/.nprcmanager_config")
  }
})

