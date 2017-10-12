#' Get the superset of columns that can be in a pedigree file.
#'
#' Part of Genetic Value Functions
#'
#' Replaces INCLUDE.COLUMNS data statement.
#' @return Superset of columns that can be in a pedigree file.
#' examples
#' get_inlcude_columns()
#'
#' @export
getIncludeColumns <- function() {
  c("id", "sex", "age", "birth", "exit", "population", "condition", "origin",
    "first_name", "second_name")
}
