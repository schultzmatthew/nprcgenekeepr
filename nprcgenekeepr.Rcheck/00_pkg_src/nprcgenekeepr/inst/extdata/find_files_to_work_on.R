#' Locates files with a particular string
#'
library(stringi)
library(rmsutilityr)
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
  examples_files = get_target_files(path, "@examples")
  exported_files = get_target_files(path, "@export")
  return_files = get_target_files(path, "@return")
  list(
    all_files = list.files(path = path),
    examples_files = examples_files,
    exported_files = exported_files,
    return_files = return_files,
    exported_and_no_return = setdiff(exported_files, return_files),
    example_files_no_export = setdiff(examples_files, exported_files)
  )
}
files_to_check <- get_files_to_check("R")
pkg_functions_in_examples <- get_pkg_functions_in_examples("R")
check_these_functions_df <-
  pkg_functions_in_examples[
    !stri_detect_fixed(pkg_functions_in_examples$file,
                       pkg_functions_in_examples$function_name), ]
check_these_functions <- unique(check_these_functions_df$function_name)
get_exported_functions <- function(namespace) {
  exported <- character(0)
  lines <- readLines(namespace)
  for (line in lines) {
    words <- stringi::stri_extract_all_words(line)
    if (is.na(words[[1]][1]))
      next
    if (any(stri_detect_fixed(words[[1]], pattern = "export")))
      exported <- c(exported, words[[1]][2])
  }
  exported
}
exported_functions <- get_exported_functions("NAMESPACE")
unexported_functions_in_examples <-
  check_these_functions[!check_these_functions %in% exported_functions]
files_to_check$exported_and_no_return

