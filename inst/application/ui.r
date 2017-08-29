library(shiny)
if (!require("DT"))
  install.packages('DT')
library(DT)
source("../application/uitp_input_file_format.R")
source("../application/uitp_pedigree_browser.R")
source("../application/uitp_genetic_value_analysis.R")
source("../application/uitp_summary_statistics.R")
source("../application/uitp_breeding_group_formation.R")
source("../application/uitp_orip_reporting.R")
source("../application/uitp_pyramid_plot.R")
source("../application/uitp_readme.R")

shinyUI(tagList(
  tags$head(tags$style(
    HTML(
      ".progress-striped .bar {background-color: #149bdf;
      background-image: -webkit-gradient(linear, 0 100%, 100% 0,
      color-stop(0.25, rgba(255, 255, 255, 0.6)),
      color-stop(0.25, transparent),
      color-stop(0.5, transparent),
      color-stop(0.5, rgba(255, 255, 255, 0.6)),
      color-stop(0.75, rgba(255, 255, 255, 0.6)),
      color-stop(0.75, transparent), to(transparent));
      background-image: -webkit-linear-gradient(
      45deg, rgba(255, 255, 255, 0.6) 25%,
      transparent 25%, transparent 50%,
      rgba(255, 255, 255, 0.6) 50%,
      rgba(255, 255, 255, 0.6) 75%,
      transparent 75%, transparent);
      background-image: -moz-linear-gradient(
      45deg, rgba(255, 255, 255, 0.6) 25%,
      transparent 25%, transparent 50%,
      rgba(255, 255, 255, 0.6) 50%,
      rgba(255, 255, 255, 0.6) 75%,
      transparent 75%, transparent);
      background-image: -o-linear-gradient(
      45deg, rgba(255, 255, 255, 0.6) 25%,
      transparent 25%, transparent 50%,
      rgba(255, 255, 255, 0.6) 50%,
      rgba(255, 255, 255, 0.6) 75%,
      transparent 75%, transparent);
      background-image: linear-gradient(
      45deg, rgba(255, 255, 255, 0.6) 25%,
      transparent 25%, transparent 50%,
      rgba(255, 255, 255, 0.6) 50%,
      rgba(255, 255, 255, 0.6) 75%,
      transparent 75%, transparent);
      -webkit-background-size: 40px 40px;
      -moz-background-size: 40px 40px;
      -o-background-size: 40px 40px;
      background-size: 40px 40px;
      }
      "
  )
  )),
  navbarPage(
    title = NULL,
    uitp_input_file_format,
    uitp_pedigree_browser,
    uitp_genetic_value_analysis,
    uitp_summary_statistics,
    uitp_breeding_group_formation,
    uitp_orip_reporting,
    uitp_pyramid_plot,
    uitp_readme,
    id = "tab_pages"
  )
))
