#' Returns first right and left space trimmed token from first character vector
#' element.
#'
#' @param line character vector with text
#' @param pattern character vector with search pattern used to select the
#' text to be retrieved.
#' @import stringi
#' @export
getToken <- function(line, pattern = "^") {
  line <- stri_sub(line, from = stri_locate_first_regex(tolower(line),
                                                        pattern)[[2]] + 1)
  line <- stri_sub(line, 1, to = stri_locate_first_regex(line, "[[\\n]]")[[1]])
  line <- stri_trim_both(line)
  if (!is.na(stri_locate_first_regex(line, "[[\\n]]")[[1]])) {
    if (stri_trim(stri_sub(
      line, 1, stri_locate_first_regex(line, "[[\\n]]")[[1]])) == "") {
      return("")
    }
  }
  return(stri_split_regex(line, pattern = "[[\\p{WHITE_SPACE}]]+")[[1]][1])
}
