#' Get site information
#'
#' @return{A list of site specific informatio used by the application.}
#'
#' Currently this returns the following character strings in a named list.
#' \enumerate{
#'   \item{center}{One of "SNPRC" or "ONPRC"}
#'   \item{baseUrl}{If \code{center} is "SNPRC", baseUrl is one of
#'   "https://boomer.txbiomed.org/labkey" or "https://vger.txbiomed.org/labkey".
#'   To allow testing, if \code{center} is "ONPRC" baseUrl is
#'   "https://boomer.txbiomed.org/labkey".}
#'   \item{schemaName}{If \code{center} is "SNPRC", schemaName is "study".
#'   If \code{center} is "ONPRC", schemaName is "study"}
#'   \item{folderPath} {If \code{center} is "SNPRC", folderPath is "/SNPRC".
#'   If \code{center} is "ONPRC", folderPath is "/ONPRC"}
#'   \item{queryName}{is "demographics"}
#'}
#' @import stringi
#' @export
getSiteInfo <- function() {
  sysInfo <- Sys.info()
  if (stri_detect_fixed(toupper(sysInfo[["sysname"]]), "WIND")) {
    homeDir <- paste0("/Users/", sysInfo[["user"]], "/")
    configFile <- paste0(homeDir, "_nprcmanager_config")
  } else {
    homeDir <- paste0("~/")
    configFile <- paste0(homeDir, ".nprcmanager_config")
  }
  if (file.exists(configFile)) {
    lines <- readLines(configFile, skipNul = TRUE)
    tokenList <- getTokenList(lines)
    list(
      center = getParamDef(tokenList, "center"),
      baseUrl = getParamDef(tokenList, "baseUrl"),
      schemaName = getParamDef(tokenList, "schemaName"),
      folderPath = getParamDef(tokenList, "folderPath"),
      queryName = getParamDef(tokenList, "queryName"),
      lkPedColumns = getParamDef(tokenList, "lkPedColumns"),
      mapPedColumns = getParamDef(tokenList, "mapPedColumns"),
      sysname  = sysInfo[["sysname"]],
      release = sysInfo[["release"]],
      version  = sysInfo[["version"]],
      nodename = sysInfo[["nodename"]],
      machine = sysInfo[["machine"]],
      login = sysInfo[["login"]],
      user = sysInfo[["user"]],
      effective_user = sysInfo[["effective_user"]],
      homeDir = homeDir,
      configFile = configFile)
  } else {
    warning(paste0("The nprcmananger configuration file is missing.\n",
                   "The file should be named: ", configFile, ".\n"))
    list(center = "ONPRC",
      baseUrl = "https://boomer.txbiomed.org/labkey",
      schemaName = "study",
      folderPath = "/SNPRC",
      queryName = "demographics",
      lkPedColumns = c("Id", "gender", "birth", "death", "lastDayAtCenter",
                       "dam", "sire"),
      mapPedColumns = c("id", "sex", "birth", "death", "exit", "dam", "sire"),
      sysname  = sysInfo[["sysname"]],
      release = sysInfo[["release"]],
      version  = sysInfo[["version"]],
      nodename = sysInfo[["nodename"]],
      machine = sysInfo[["machine"]],
      login = sysInfo[["login"]],
      user = sysInfo[["user"]],
      effective_user = sysInfo[["effective_user"]],
      homeDir = homeDir,
      configFile = configFile)
  }
}
