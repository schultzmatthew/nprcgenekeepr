uitp_summary_statistics <-
  tabPanel(
    "Summary Statistics",

    # Side Panel
    div(
      style = paste(
        "float: left; width: 400px; height: 100vh; padding: 10px;",
        "border: 1px solid lightgray; background-color: #EDEDED;",
        "margin-left: 3px; margin-top: 3px; margin-bottom: 3px;",
        "border-radius: 25px; box-shadow: 0 0 5px 2px #888"
      ),
      includeHTML("../extdata/summary_stats.html"),
      downloadButton("downloadKinship", "Export Kinship Matrix"),
      br(),
      br(),
      downloadButton("downloadFirstOrder", "Export First-Order Relationships"),
      br()
    ),

    # Main Panel
    div(
      style = "margin-left:425px;padding:10px;",
      htmlOutput("summary_stats"),
      plotOutput("mk_plot", width = "400px", height = "400px"),
      plotOutput("zscore_plot", width = "400px", height = "400px"),
      #tableOutput("relations")
      DT::dataTableOutput("relations")
    )
  )