#' getConfigFileName returns the configuration file name appropriate for
#' the system.
## Copyright(c) 2017-2019 R. Mark Sharp
## This file is part of nprcmanager
#'
#' @return character vector with expected configuration file
#'
#' @param sysInfo object returned by Sys.info()
#' @export
getConfigFileName <- function(sysInfo) {
  if (stri_detect_fixed(toupper(sysInfo[["sysname"]]), "WIND")) {
    homeDir <- paste0("/Users/", sysInfo[["user"]], "/")
    configFile <- paste0(homeDir, "_nprcmanager_config")
  } else {
    homeDir <- paste0("~/")
    configFile <- paste0(homeDir, ".nprcmanager_config")
  }
  c(homeDir = homeDir, configFile = configFile)
}
