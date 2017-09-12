#' Get parents to corresponding animal IDs provided
#'
#' @param ped_source_df dataframe with pedigree structure having at least the
#' columns id, sire, and dam.
#' @param ids character vector of animal IDs
#' @export
getParents <- function(ped_source_df, ids) {
  unique(c(ped_source_df$sire[(ped_source_df$id %in% ids &
                                 !is.na(ped_source_df$sire))],
           ped_source_df$dam[(ped_source_df$id %in% ids &
                                !is.na(ped_source_df$dam))]))
}
