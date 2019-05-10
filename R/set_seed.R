#' Work around for unit tests using sample()
#'
#' @param seed argument to \code{set.seed}
set_seed <- function(seed = 1) {
  version <- as.integer(R.Version()$major) + (as.numeric(R.Version()$minor) / 10.0)
  # if (version >= 3.6) {
  #   args <- list(seed, sample.kind = "Rounding")
  # } else {
  #   args <- list(seed)
  # }
  # suppressWarnings(do.call(set.seed, args))
  if (version >= 3.6) {
    RNGkind(sample.kind="Rounding")
  }
  set.seed(seed)
}
