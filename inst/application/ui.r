library(shiny)
#library(shinyBS)
if (!require("DT"))
  install.packages("DT")
library(DT)
source("../application/uitpInput.R")
source("../application/uitpPedigreeBrowser.R")
source("../application/uitpGeneticValueAnalysis.R")
source("../application/uitpSummaryStatistics.R")
source("../application/uitpBreedingGroupFormation.R")
source("../application/uitpPyramidPlot.R")
source("../application/uitpGvAndBgDesc.R")
if (getSiteInfo()$center == "ONPRC") {
  source("../application/uitpOripReporting.R")
  navbarPageArgs <- list(
    title = "Genetic Management Tools",
    uitpInput,
    uitpPedigreeBrowser,
    uitpGeneticValueAnalysis,
    uitpSummaryStatistics,
    uitpBreedingGroupFormation,
    uitpOripReporting,
    uitpPyramidPlot,
    uitpGvAndBgDesc,
    id = "tab_pages"
  )
} else {
  navbarPageArgs <- list(
    title = "Genetic Management Tools",
    uitpInput,
    uitpPedigreeBrowser,
    uitpGeneticValueAnalysis,
    uitpSummaryStatistics,
    uitpBreedingGroupFormation,
    uitpPyramidPlot,
    uitpGvAndBgDesc,
    id = "tab_pages"
  )
}

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
  do.call(navbarPage, navbarPageArgs)
  ))
