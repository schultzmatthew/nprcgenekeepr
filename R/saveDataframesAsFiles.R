#' Write copy of dataframes to either CSV or Excel file.
#'
## Copyright(c) 2017-2019 R. Mark Sharp
## This file is part of nprcgenekeepr
#' Uses \code{examplePedigree} data structure to create an example data file
#' @return full path name of file saved.
#' @param df_list list of dataframes to be stored as files.
#' \code{"txt"}, \code{"csv"}, or \code{"xlsx"}. Default value is \code{"csv"}.
#' @param fileType character vector of length one with possible values of
#' \code{"txt"}, \code{"csv"}, or \code{"xlsx"}. Default value is \code{"csv"}.
#'
#' @importFrom utils write.table write.csv
## ## rmsutilityr create_wkbk
#' @export
saveDataframesAsFiles <- function(df_list, fileType = "csv") {
  if (!(class(df_list) == "list" &
      all(sapply(df_list, function(df) {class(df) == "data.frame"}))))
      stop("df_list must be a list containing only dataframes.")
  stopifnot(any(fileType %in% c("txt", "csv", "excel")))
  filesWritten <- character(0)
  for (i in seq_along(df_list)) {
    filename <- paste0(names(df_list)[i], ".", fileType)
    if (fileType == "csv") {
      write.csv(df_list[[i]],
                file = filename,
                row.names = FALSE)
    } else if (fileType == "excel") {
      status <-
        create_wkbk(
          file = filename,
          df_list = df_list[i],
          sheetnames = names(df_list)[i],
          create = TRUE
        )
      if (!status)
        stop(paste0("Failed to write example data out to ", filename, "."))
    } else {
      write.table(
        df_list[[i]],
        file = filename,
        row.names = FALSE,
        sep = "\t"
      )
    }
    filesWritten <- c(filesWritten, filename)
  }
  filesWritten
}
