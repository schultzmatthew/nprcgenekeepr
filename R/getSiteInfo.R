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
    homeDir <- paste0("Users/", sys_info[["user"]], "/")
    configFile <- paste0(homeDir, "_nprcmanager_config")
  } else {
    homeDir <- paste0("~/")
    configFile <- paste0(homeDir, ".nprcmanager_config")
  }
  if (file.exists(configFile)) {
    lines <- readLines(configFile, skipNul = TRUE)
    inList <- FALSE
    for (line in lines) {
      tokens <- getTokens(line)
      if (length(tokens) > 0) {
        if (stri_sub(tokens[1], 1, 1) == "#") # comment found
          next
        if (length(tokens) > 1) {
          if (stri_detect_fixed(tokens[2], "(")) {
            inList <- TRUE
            if (tolower(tokens[1]) != "pedcolumns") {
              stop(paste0("Unexpected list following ", tokens[1], ".\n"))
            } else {
              pedColumns <- c(tokens[2:length(tokens)])
              if (any(stri_detect_fixed(tokens, ")"))) {
                inList <- FALSE
              }
              next
            }
          } else if (inList) {
            pedColumns <- c(pedColumns, tokens)
            if (any(stri_detect_fixed(tokens, ")"))) {
              inList <- FALSE
            }
          } else if (tolower(tokens[1]) == "center") {
            center <- tokens[2]
          } else if (tolower(tokens[1]) == "baseurl") {
            baseUrl <- tokens[2]
          } else if (tolower(tokens[1]) == "schemaname") {
            schemaName <- tokens[2]
          } else if (tolower(tokens[1]) == "folderpath") {
            folderPath <- tokens[2]
          } else if (tolower(tokens[1]) == "queryname") {
            queryName <- tokens[2]
          } else {
            stop(paste0("Cannot parse line: ", line,
                        "\n found in configuration file.\n"))
          }
        } else if (length(tokens) == 1 & inList) {
          pedColumns <- c(pedColumns, tokens)
          if (any(stri_detect_fixed(tokens, ")"))) {
            inList <- FALSE
          }
        } else {
          next
        }
      }
    }
    if (!is.null(pedColumns)) {
      pedColumns[2] <- stri_sub(pedColumns[2], 2)
      len <- length(pedColumns)
      pedColumns[len] <- stri_sub(pedColumns[len], 1,
                                  stri_length(pedColumns[len]) - 1)
    }
    list(
      center = center,
      baseUrl = baseUrl,
      schemaName = schemaName,
      folderPath = folderPath,
      queryName = queryName,
      pedColumns = pedColumns,
      sysname  = sys_info[["sysname"]],
      release = sys_info[["release"]],
      version  = sys_info[["version"]],
      nodename = sys_info[["nodename"]],
      machine = sys_info[["machine"]],
      login = sys_info[["login"]],
      user = sys_info[["user"]],
      effective_user = sys_info[["effective_user"]],
      homeDir = homeDir,
      configFile = configFile)
  } else {
    warning(paste0("The nprcmananger configuration file is missing.\n",
                   "The file should be named: ", configFile, ".\n",
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
      pedColumns = c("Id", "date", "gender", "species", "birth", "death",
                  "lastDayAtCenter", "calculated_status", "dam", "sire",
                  "origin", "parentid" , "species/arc_species_code"),
      sysname  = sys_info[["sysname"]],
      release = sys_info[["release"]],
      version  = sys_info[["version"]],
      nodename = sys_info[["nodename"]],
      machine = sys_info[["machine"]],
      login = sys_info[["login"]],
      user = sys_info[["user"]],
      effective_user = sys_info[["effective_user"]],
      homeDir = homeDir,
      configFile = configFile)
  }
}
