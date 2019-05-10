#' Work around for unit tests using sample()
#'
#' @param seed argument to \code{set.seed}
set_seed <- function(seed = 1) {
  #version <- as.integer(R.Version()$major) + (as.numeric(R.Version()$minor) / 10.0)
  tryCatch(suppressWarnings(set.seed(seed, sample.kind = "Rounding")),
           warning = function() set.seed(seed),
           error = function() set.seed(seed))
}
