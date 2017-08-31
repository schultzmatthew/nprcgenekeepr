uitpGeneticValueAnalysis <-
  tabPanel(
    "Genetic Value Analysis",

    div(
      style = "min-width:1000px",
      # Side Panel
      div(
        style = paste(
          "float: left; width: 400px; height: 190px; padding: 10px;",
          "border: 1px solid lightgray; background-color: #EDEDED;",
          "margin-left:3px; margin-top: 3px; margin-bottom: 3px;",
          "border-radius: 25px; box-shadow: 0 0 5px 2px #888"
        ),
        includeHTML("../extdata/genetic_value.html")
      ),

      # Main Panel
      div(
        style = "margin-left:425px;padding:10px;",

        div(
          style = "display:inline-block;width:250px;padding:10px",
          numericInput(
            "iterations",
            label = "Enter the number of simulations for the gene-drop analysis:",
            value = 1000,
            min = 2,
            max = 100000
          )
        ),
        div(
          style = "display:inline-block;width:250px;padding:10px",
          selectInput(
            "threshold",
            label = "Enter the genome uniqueness threshold:",
            choices = list(
              "0" = 1,
              "1" = 2,
              "2" = 3,
              "3" = 4,
              "4" = 5
            ),
            selected = 1
          )
        ),
        helpText("The analysis may take a significant amount of time (>20 min)"),
        actionButton("analysis", label = "Begin Analysis"),
        br(),
        hr(),
        helpText(h4("Results:")),
        helpText("Enter IDs of specific animals to be viewed:"),
        helpText("(Leave blank to view all)"),
        div(
          style = "display:inline-block;width:250px;padding:10px",
          tags$textarea(id = "view_ids", rows = 5, cols = 20, ""),
          actionButton("view", label = "Filter View")
        ),
        div(
          style = "display:inline-block;width:250px;padding:10px",
          downloadButton("downloadGVA_full", "Export All"),
          br(),
          br(),
          downloadButton("downloadGVA_subset", "Export Current Subset")
        )

      )
    ),
    DT::dataTableOutput("gva")
    #htmlOutput("gva")
  )
