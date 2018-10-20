#' insertErrorTab insert a list of errors found by \code{qcStudbook} in the pedigree file
#'
#' @return text of the error list formated as an HTML page
#' @param errorLst list of errors and changes made by \code{qcStudbook}
#' @param pedigreeFileName name of file provided by user on Input tab
#' @importFrom stringi stri_c
#' @importFrom stringi stri_split_regex
#' @export
insertErrorTab <- function(errorLst, pedigreeFileName) {
  text <- summary(errorLst)
  if (checkChangedColsLst(errorLst$changedCols)) {
    colsChangedTxt <- "and Changes to Pedigree Column Names "
  } else {
    colsChangedTxt <- ""
  }
  lines <- stri_split_regex(text$txt, pattern = "\n")[[1]]
  newText <- stri_c("<h3>Errors Detected ", colsChangedTxt,
                    "</h3>\nFile: '", pedigreeFileName,
                    "'\n<ul style=\"list-style-type:disc\">\n")

  for (line in lines) {
    if (stri_trim_both(line) == "")
      next
    newText <- stri_c(newText, "	<li style=\"padding-bottom: 15px\">\n",
                      line, "</li>\n")
  }
  if (nrow(text$sp) > 0) {
    lines <- dataframe2string(text$sp, row.names = FALSE, digits = 2)
    lines <- stri_split_lines(lines)[[1]]
    newText <- stri_c(newText, "\n<h3>Questionable Parents Because of ",
                      "Suspicious Dates</h3><pre>\n")
    for (line in lines)
      newText <- stri_c(newText, "<p>", line, "</p>")
  }
  newText <- stri_c(newText, "</ul>\n</p>\n</pre>")
  newText
}
