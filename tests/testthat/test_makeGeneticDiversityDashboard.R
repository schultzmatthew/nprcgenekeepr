#' Copyright(c) 2017-2019 R. Mark Sharp
# This file is part of nprcmanager

geneticDiversityStats <- data.frame(
  group = c(paste0("Group_", 1:5), paste0("Corral_", 1:3)),
  highLow = sample(1:3, 8, replace = TRUE),
  indianOriginStatus = sample(1:3, 8, replace = TRUE),
  fecundity = sample(1:3, 8, replace = TRUE),
  kinshipWithMale = sample(1:3, 8, replace = TRUE),
  genotypePhenotype = sample(1:3, 8, replace = TRUE))

# test_that("makeGeneticDiversityDashboard creates image file", {
#   fn <- "images/dashboard_in_r.png"
#   if (file.exists(fn))
#     file.remove(fn)
#   expect_false(file.exists(fn))
#   makeGeneticDiversityDashboard(geneticDiversityStats,
#                               file = fn)
#   expect_true(file.exists(fn))
#   if (file.exists(fn))
#     file.remove(fn)
# })
