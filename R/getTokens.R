#' Returns first right and left space trimmed token from first character vector
#' element.
#'
#' @param line character vector with text
#' @param pattern character vector with search pattern used to select the
#' text to be retrieved.
#' @import stringi
#' @export
getTokens <- function(line) {
  line <- stri_replace_all_fixed(stri_trim_both(line), pattern = "\"",
                                 replacement = "")
  tokens <- stri_split_regex(line, pattern = "[[\\p{WHITE_SPACE},]]+")[[1]]
  tokens[!tokens == ""]
}
