context("test_create_wkbk")
make_df_list <- function(size) {
  df_list <- list(size)
  if (size <= 0)
    return(df_list)
  for (i in 1:size) {
    n <- sample(2:10, 2, replace = TRUE)
    df <- data.frame(matrix(data = rnorm(n[1] * n[2]), ncol = n[1]))
    df_list[[i]] <- df
  }
  names(df_list) <- paste0("A", 1:size)
  df_list
}
test_that("create_wkbk recognizes wrong number of dataframes", {
  df_list <- make_df_list(3)
  file <- "filename"
  sheetnames <- names(df_list)[1:2]
  expect_error(create_wkbk(file = file, df_list = df_list,
                           sheetnames = sheetnames, create = TRUE), 
               "Number of dataframes does not match number of worksheet")
})
## cannot test creation of file.
