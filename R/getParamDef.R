#' Get parameter definitions from tokens found in configuration file.
#'
#' @param tokenList list of parameters and their definitions, which are
#' character vectors
#' @param param character vector representing the parameter being defined.
#' @export
getParamDef <- function(tokenList, param) {
  if (!any(tolower(tokenList$param) == tolower(param))) {
    stop(paste0("Could not find ", param, " in configuration file. ",
                "Check spelling carefully.\n"))
  } else {
    tokenList$tokenVec[tokenList$params == param]
  }
}
