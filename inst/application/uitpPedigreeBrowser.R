uitpPedigreeBrowser <-
  tabPanel(
    "Pedigree Browser",
    # Side Panel
    div(
      div(
        style = paste(
          "float: left; width: 400px; height: 340px; padding: 10px;",
          "border: 1px solid lightgray; background-color: #EDEDED;",
          "margin-left: 3px; margin-top: 3px; margin-bottom: 3px;",
          "border-radius: 25px; box-shadow: 0 0 5px 2px #888"
        ),
        includeHTML("../extdata/pedigree_browser.html")
      ),

      # Main Panel
      div(
        style = "margin-left: 425px; width: 550px; padding: 10px;",
        helpText(paste0("If analysis of all individuals is not needed, IDs of ",
                        "selected breeding population members may be manually ",
                        "entered here (IDs may be pasted from Excel):")),
        div(
          style = "display:inline-block;width:250px;padding:10px",
          tags$textarea(id = "populationIds", rows = 5, cols = 30, ""),
          actionButton("specifyPop", label = "Update Breeding Colony")
        ),
        # fileInput("breedingColonyUpdate", "Choose CSV File",
        #           multiple = FALSE,
        #           accept = c("text/csv",
        #                      "text/comma-separated-values,text/plain",
        #                      ".csv")),
        div(
          style = "display:inline-block;width:250px;padding:10px",
          checkboxInput("uid", label = "Display UIDs",
                        value = TRUE),
          checkboxInput("trim",
                        label = "Trim pedigree based on specified population",
                        value = FALSE),
          downloadButton("downloadPedigree", "Export")
        ),
        helpText(
          "(A population must be defined before proceeding
          to the Genetic Value Analysis.)", style = "color:blue"
      )

        )
    ),
    hr(),
    DT::dataTableOutput("pedigree")
    )
