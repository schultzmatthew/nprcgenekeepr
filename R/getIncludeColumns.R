#' Get the superset of columns that can be in a pedigree file.
#'
#' Part of Genetic Value Functions
#' @return Superset of columns that can be in a pedigree file.
#' examples
#' get_inlcude_columns()
#'
#' @export
getIncludeColumns <- function() { # Replaces INCLUDE.COLUMNS data statement.
  c("id", "sex", "age", "birth", "exit", "population", "condition", "origin",
    "first_name", "second_name")
}
