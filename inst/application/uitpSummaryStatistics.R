uitpSummaryStatistics <-
  tabPanel(
    "Summary Statistics",

    # Side Panel
    # div(
    #  style = paste(
    #    "float: left; width: 400px; height: 76vh; padding: 10px;",
    #    "border: 1px solid lightgray; background-color: #EDEDED;",
    #    "margin-left: 3px; margin-top: 3px; margin-bottom: 3px;",
    #    "border-radius: 25px; box-shadow: 0 0 5px 2px #888"
    #  ),
   fluidRow(
     column(3,
            style = paste(
             "border: 1px solid lightgray; background-color: #EDEDED;",
             "border-radius: 25px; box-shadow: 0 0 5px 2px #888"
            ),
      includeHTML("../extdata/summary_stats.html"),
      downloadButton("downloadKinship", "Export Kinship Matrix"),
      br(),
      br(),
      downloadButton("downloadFirstOrder", "Export First-Order Relationships"),
      #popify(downloadButton("downloadFirstOrder", "Export First-Order Relationships"),
      #       "Exports All First-Order Relationships as CSV",
      #       "This exports all first-order relations to a CSV file in the users home directory"),
      br()
    ),

    # Main Panel
    column(6,
      # style = "margin-left:425px;padding:10px;",
      htmlOutput("summaryStats"),
      plotOutput("mkPlot", width = "400px", height = "400px"),
      plotOutput("zscorePlot", width = "400px", height = "400px"),
      plotOutput("guPlot", width = "400px", height = "400px"),
      DT::dataTableOutput("relations")
    ),
    column(9,
           # style = "margin-left:425px;padding:10px;",
           htmlOutput("summaryStats"),
           plotOutput("mkPlot", width = "400px", height = "400px"),
           plotOutput("zscorePlot", width = "400px", height = "400px"),
           plotOutput("guPlot", width = "400px", height = "400px"),
           DT::dataTableOutput("relations")
    )
   )
)
