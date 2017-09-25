#' Returns first right and left space trimmed token from first character vector
#' element.
#'
#' @param lines character vector with text from configuration file
#' @import stringi
#' @export
getTokenList <- function(lines) {
  tokens <- character(0)
  line <- paste0(lines, collapse = " ")
  line <- stri_replace_all_fixed(stri_trim_both(line), pattern = "\"",
                               replacement = "")
  line <- stri_replace_all_fixed(line, pattern = "=",
                                 replacement = " = ")
  tokens <- c(tokens, stri_split_regex(
    line, pattern = "[[\\p{WHITE_SPACE},]]+")[[1]])
  tokens <- tokens[!tokens == ""]
  parLocations <- seq_along(tokens)[tokens == "="] - 1
  param <- tokens[parLocations]
  start <- parLocations + 2
  end <- parLocations - 1
  end <- c(end[-1], length(tokens))
  tokens <- stri_replace_all_regex(tokens, "[()]+", "")
  tokenVec <- list()
  for (i in seq_along(start)) {
    tokenVec[[i]] <- tokens[seq(from = start[i], to = end[i])]
  }
  list(param = param, tokenVec = tokenVec)
}
