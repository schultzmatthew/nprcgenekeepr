#' Get site information
#'
#' @return{A list of site specific informatio used by the application.}
#'
#' This version is a stub in that it has to be modified by each site.
#' The plan is to either replace this routine or give it a dynamic way to
#' define the needed information.
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
  sys_info <- Sys.info()
  if (stri_detect_fixed(toupper(sys_info[["sysname"]]), "WIND")) {
    config_file <- paste0("Users/", sys_info[["user"]], "/_nprcmanager_config")
  } else {
    config_file <- paste0("~/.nprcmanager_config")
  }
  if (file.exists(config_file)) {
    config_df <- read.csv(config_file, header = TRUE, sep = ",",
                          stringsAsFactors = FALSE, na.strings = c("", "NA"),
                          check.names = FALSE)
    list(
      center = config_df[["center"]],
      baseUrl = config_df[["baseUrl"]],
      schemaName = config_df[["schemaName"]],
      folderPath = config_df[["folderPath"]],
      queryName = config_df[["queryName"]],
      sysname  = sys_info[["sysname"]],
      release = sys_info[["release"]],
      version  = sys_info[["version"]],
      nodename = sys_info[["nodename"]],
      machine = sys_info[["machine"]],
      login = sys_info[["login"]],
      user = sys_info[["user"]],
      effective_user = sys_info[["effective_user"]])
  } else {
    warning(paste0("The nprcmananger configuration file is missing.\n",
                   "The file should be named: ", config_file, ".\n",
                   "It should have two lines with the first being a header ",
                   "with the following:\n",
                   "center, baseUrl, schemaName, folderPath, queryName\n",
                   "Note character case and order is fixed.\n",
                   "The second line should have comma separated quoted fields ",
                   "such as:\n",
                   "\"SNPRC\", \"https://boomer.txbiomed.org/labkey\", ",
                   "\"study\", \"/SNPRC\", \"demographics\"\n"))
    list(center = "ONPRC",
      baseUrl = "https://boomer.txbiomed.org/labkey",
      schemaName = "study",
      folderPath = "/SNPRC",
      queryName = "demographics",
      sysname  = sys_info[["sysname"]],
      release = sys_info[["release"]],
      version  = sys_info[["version"]],
      nodename = sys_info[["nodename"]],
      machine = sys_info[["machine"]],
      login = sys_info[["login"]],
      user = sys_info[["user"]],
      effective_user = sys_info[["effective_user"]])
  }
}
