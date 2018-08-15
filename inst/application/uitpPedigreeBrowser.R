uitpPedigreeBrowser <-
  tabPanel(
    "Pedigree Browser",
    div(
    div(
      # Main Panel
      div(
        style = "float: left; width: 500px; padding: 1px;",
        helpText(paste0("IDs of selected focal animals may be manually ",
                        "entered here if analysis of all individuals is ",
                        "not needed. IDs may be pasted from Excel or you ",
                        "can browse for and select a file containing the list ",
                        "of focal animals.")),
      tags$textarea(id = "focalAnimalIds", rows = 5, cols = 60, ""),
      fileInput("focalAnimalUpdate", "Choose CSV file with focal animals",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      actionButton("specifyFocalAnimal", label = "Update Focal Animals"),
      checkboxInput("uid", label = "Display UIDs",
                        value = TRUE),
      checkboxInput("trim",
                    label = "Trim pedigree based on specified population",
                    value = FALSE),
      downloadButton("downloadPedigree", "Export"),
      helpText(
          "(A population must be defined before proceeding
          to the Genetic Value Analysis.)", style = "color:blue")
    ,
    div(
    hr(),
    DT::dataTableOutput("pedigree")
    )),
      div(
        # Right Side Panel
        style = paste("margin-left: 550px;",
          "width: 500px; height: 180px; padding: 10px;",
          "border: 1px solid lightgray; background-color: #EDEDED;",
          "margin-left: 560px; margin-top: 3px; margin-bottom: 3px;",
          "border-radius: 25px; box-shadow: 0 0 5px 2px #888"
        ),
        includeHTML("../extdata/pedigree_browser.html")
      )
    )
    ))
    
