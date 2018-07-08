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
     column(10, offset = 1,
            style = paste(
             "border: 1px solid lightgray; background-color: #EDEDED;",
             "border-radius: 25px; box-shadow: 0 0 5px 2px #888"
            ),
            withMathJax(includeHTML("../extdata/summary_stats.html"))
     )),
   br(),
   fluidRow(
     column(3, offset = 1,
            popify(downloadButton("downloadKinship", "Export Kinship Matrix"),
                   NULL, #"Exports Kinship Matrix as CSV",
                   "This exports the kinship matrix to a CSV file in the users home directory")),
     column(3,
            offset = 1,
            popify(downloadButton("downloadFirstOrder", "Export First-Order Relationships"),
             NULL, #"Exports All First-Order Relationships as CSV",
             "This exports all first-order relations to a CSV file in the users home directory")
    )), br(),
   fluidRow(
     column(10, offset = 1,
            htmlOutput("summaryStats")
     )),br(),br(),
   fluidRow(
    # Main Panel
    column(5, offset = 1,
      # style = "margin-left:425px;padding:10px;",
      plotOutput("mkPlot", width = "400px", height = "400px"),
      plotOutput("zscorePlot", width = "400px", height = "400px"),
      plotOutput("guPlot", width = "400px", height = "400px"),
      DT::dataTableOutput("relations")
    ),
    column(5,
           plotOutput("mkBox", width = "400px", height = "400px"),
           plotOutput("zscoreBox", width = "400px", height = "400px"),
           plotOutput("guBox", width = "400px", height = "400px")#,
    )
   ),
   fluidRow(
     column(10, offset = 1,
            style = paste(
              "border: 1px solid lightgray; background-color: #EDEDED;",
              "border-radius: 25px; box-shadow: 0 0 5px 2px #888"
            ),
            withMathJax(includeHTML("../extdata/population_genetics_terms.html"))
     ))

)
