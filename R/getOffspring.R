#' Get offspring to corresponding animal IDs provided
#'
#' @param ped_source_df dataframe with pedigree structure having at least the
#' columns id, sire, and dam.
#' @param ids character vector of animal IDs
#' @export
getOffspring <- function(ped_source_df, ids) {
  unique(c(ped_source_df$id[(ped_source_df$sire %in% ids &
                                 !is.na(ped_source_df$sire))],
           ped_source_df$id[(ped_source_df$dam %in% ids &
                                !is.na(ped_source_df$dam))]))
}
