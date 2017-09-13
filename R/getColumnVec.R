#' Get specified list of columns out of a set of lines.
#'
#' The set of lines comes from the nprcmanager configuration file as read by
#' getSiteInfo(). The list of column names is bound by opening and closing
#' parenthesis characters ("(", ")").
#'
#' @param lines a character vector containing the lines of text potentially
#' containing a named set of columns.
#' @param namedCol a character vector of length one having the value of the
#' first token in the first line in the list to be returned.
#' @export
getColumnVec <- function(lines, namedCol) {
  inList <- FALSE
  returnLines <- character(0)
  for (line in lines) {
    tokens <- getTokens(line)
    if (length(tokens) > 0) {
      if (stri_sub(tokens[1], 1, 1) == "#") { # comment found
        returnLines <- c(returnLines, line)
        next
      }
      if (length(tokens) > 1) {
        if (stri_detect_fixed(tokens[2], "(")) {
          if (tolower(tokens[1]) != namedCol) {
            returnLines <- c(returnLines, line)
            next
          } else {
            lkPedColumns <- c(tokens[2:length(tokens)])
            if (any(stri_detect_fixed(tokens, ")"))) {
              inList <- FALSE
            }
            next
          }
        } else if (inList) {
          lkPedColumns <- c(lkPedColumns, tokens)
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
        lkPedColumns <- c(lkPedColumns, tokens)
        if (any(stri_detect_fixed(tokens, ")"))) {
          inList <- FALSE
        }
      } else {
        next
      }
    }
  }
  if (!is.null(lkPedColumns)) {
    lkPedColumns[2] <- stri_sub(lkPedColumns[2], 2)
    len <- length(lkPedColumns)
    lkPedColumns[len] <- stri_sub(lkPedColumns[len], 1,
                                  stri_length(lkPedColumns[len]) - 1)
  }

  list(lines = lines, vec = "")
}
#
#