#' Locates files with a particular string
#'
library(stringi)
path = "R"
pattern <- "@examples"
get_target_files <- function(path, pattern) {
  files <- list.files(path = path, full.names = TRUE)
  target_files <- character(0)
  for (file in files) {
     lines <- readLines(file)
     if (any(stri_detect_fixed(lines, pattern = pattern)))
        target_files <- c(target_files, file)
  }
  target_files
}
get_files_to_check <- function(path = ".") {
  list(
    all_files = list.files(path = path),
    examples_files = get_target_files(path, "@examples"),
    exported_files = get_target_files(path, "@export"),
    return_files = get_target_files(path, "@return"),
    exported_and_no_return = setdiff(exported_files, return_files),
    example_files_no_export = setdiff(examples_files, exported_files)
  )
}
files_to_check <- get_files_to_check("R")

#' @export
get_functions_from_examples <- function(path = ".") {
  files <- get_target_files(path = path, pattern = "@examples")
  functions_in_examples <- data.frame()
  for (file in files) {
    lines <- get_example_lines(file = file)
    function_names <- get_function_names(lines)
    if (length(function_names) > 0) {
      functions_in_examples <-
        rbind(functions_in_examples,
              data.frame(file = rep(file, nrow(function_names)),
                         function_name = function_names))
    }
  }
  function_names
}
#' @importFrom stringi stri_detect_fixed
#' @export
get_example_lines(file = stdin()) {
  lines <- readLines(file)
  len <- length(lines)
  if (len > 0) {
    counter <- 0
    for (line in lines) {
      counter <- counter + 1
      if (stri_detect_fixed(line, pattern = pattern))
        break
    }
    lines <- lines[counter + 1:len]
    example_lines <- character(0)
    for (line in lines) {
      if (stri_detect_fixed(line, pattern = "^#' @"))
        break
      example_lines <- c(example_lines, line)
    }
    example_lines
  }
}
