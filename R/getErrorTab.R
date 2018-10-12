#' getErrorTab skeleton of list of errors
#'
#' @return HTML formated error list
#' @importFrom stringi stri_c
#' @export
getErrorTab <- function() {
  newText <- stri_c("<h4>Errors Detected and Changes to be Made to Pedigree ",
                    "File:</h4>\\n<p>\\n<ul style=\"list-style-type:disc\"\\n")
  newText <- stri_c(newText, "There is nothing to report \\n</ul>\\n</p>\\n")
  newText
}
