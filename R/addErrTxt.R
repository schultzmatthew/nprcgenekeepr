#' addErrTxt Concatenates any errors from nprcmanagErr into narrative form
#'
#' @return Error from nprcmaager
#' @param txt character string with initial error description value
#' @param err ve from errorLst
#' @param singularTxt character string with text used when the
#' length of err is 1
#' @param pluralTxt character string with text used when the
#' length of err is greater than 1.
#' @importFrom stringi stri_c
#' @importFrom stringi stri_detect_fixed
#' @export
addErrTxt <- function(txt, err, singularTxt, pluralTxt) {
  if (length(err) == 1) {
    if (stri_detect_fixed(err, "and")) {
      txt <- stri_c(txt, "The ", pluralTxt, ": ", err, ".\n")
    } else {
      txt <- stri_c(txt, "The ", singularTxt, ": ", err, ".\n")
    }
  } else if (length(err) > 1) {
    if (length(err) > 5)
      err <- err[1:5]
    txt <- stri_c(txt, "The ", pluralTxt, ": ",
                  get_and_or_list(err), ".\n")
  }
  txt
}
