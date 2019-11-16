# makeFatalErrorTab <- function(fatalErrorText) {
#   errorLst <- getEmptyErrorLst()
#   errorLst$fatalError <- paste0(fatalErrorText, "\n")
#   insertTab(inputId = "tab_pages",
#           getErrorTab(errorLst, "no_file_name"), target = "Input",
#           position = "before", select = TRUE)
# }
