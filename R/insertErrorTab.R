#' insertErrorTab insert a list of errors found by \code{qcStudbook} in the pedigree file
#'
#' @return text of the error list formated as an HTML page
#' @param errorLst list of errors and changes made by \code{qcStudbook}
#' @importFrom stringi stri_c
#' @importFrom stringi stri_split_regex
#' @export
insertErrorTab <- function(errorLst) {
  text <- summary(errorLst)
  lines <- stri_split_regex(text$txt, pattern = "\\n")[[1]]
  newText <- stri_c("<h4>Errors Detected and Changes to be Made to Pedigree ",
                    "File:</h4>\\n<p>\\n<ul style=\"list-style-type:disc\"\\n")

    for (line in lines) {
    newText <- stri_c(newText, "	<li style=\"padding-bottom: 15px\">\\n",
                      line, "</li>\\n")
  }
  newText <- stri_c(newText, "</ul>\\n</p>\\n")
  function() {
    newText
  }

}
