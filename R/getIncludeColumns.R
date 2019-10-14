#' Get the superset of columns that can be in a pedigree file.
#'
## Copyright(c) 2017-2019 R. Mark Sharp
## This file is part of nprcmanager
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
