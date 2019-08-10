#' Returns a one element character string with correct punctuation for
#' a list made up of the elements of the character vector argument.
#'
#' @param c_vector Character vector containing the list of words
#' to be put in a list.
#' @param conjunction The conjunction to be used as the connector.
#' This is usually `and' or `or' with `and' being the default.
#' @examples
#' get_and_or_list(c("Bob", "John")) # "Bob and John"
#' get_and_or_list(c("Bob", "John"), "or") # "Bob or John"
#' get_and_or_list(c("Bob", "John", "Sam", "Bill"), "or")
#' # "Bob, John, Sam, or Bill"
#' @import stringi
#' @export
get_and_or_list <- function(c_vector, conjunction = "and") {
  len <- length(c_vector)
  c_str <- ""
  if (len == 1)
    c_str <- c_vector
  if (len == 2)
    c_str <- stri_c(c_vector[[1]], conjunction, c_vector[[2]], sep = " ")
  if (len > 2) {
    c_str <- stri_c(c(c_vector[1:(len - 1)], conjunction), sep = "",
                    collapse = ", ")
    c_str <- stri_c(c_str, " ", c_vector[[len]], sep = "", collapse = " ")
  }
  c_str
}
