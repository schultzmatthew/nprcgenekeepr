#' addErrTxt Concatenates any errors from nprcmanagerErr into narrative form
#'
#' @returns Error from nprcmaager
#' @param tex character string with initial error description value
#' @param err ve from errorLst
#' @param singularTxt character string with text used when the
#' length of err is 1
#' @param pluralTxt character string with text used when the
#' length of err is greater than 1.
#' @importFrom stringi stri_c
#' @export
addErrorTxt <- function(txt, err, singularTxt, pluralTxt) {
  if (length(err) == 1) {
    txt <- stri_c(txt, "The ", singularTxt, ": ", err, ".\n")
  } else if (length(err) > 1) {
    txt <- stri_c(txt, "The ", pluralTxt, ": ",
                  get_and_or_list(err), ".\n")
  }
  txt
}
