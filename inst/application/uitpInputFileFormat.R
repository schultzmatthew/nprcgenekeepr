uitpInputFileFormat <-
  tabPanel(
    "Input File Format",

    tags$style(
      type = "text/css",
      "table {border: 1px solid black; width: 100%; padding: 15px;}",
      "tr, td, th {border: 1px solid black; padding: 5px;}",
      "th {font-weight: bold; background-color: #7CFC00;}",
      "hr {border-width:2px;border-color:#A9A9A9;}"
    ),
    titlePanel(div(
      style = "height:125px;width:100%",
      div(
        style = "float:left;width:45%",
        img(
          src = getLogo()$file,
          height = getLogo()$height,
          width = getLogo()$width
        )
      ),
      div(style = "float:right;text-align:right;width:45%",
          "Genetic Management Tools")
    )),

    # Side Panel
    div(
      style = paste(
        "float: left; width: 400px; height: 100%; padding: 10px;",
        "border: 1px solid lightgray; background-color: #EDEDED;",
        "margin-left: 3px;",
        "border-radius: 25px; box-shadow: 0 0 5px 2px #888"
      ),
      helpText(
        "Select a pedigree file for analysis.
        See the 'Format' tab for file formatting descriptions."
      ),

      radioButtons(
        'sep',
        label = 'Separator',
        choices = list(
          'Comma' = ',',
          'Semicolon' = ';',
          'Tab' = '\t'
        ),
        selected = ','
      ),

      fileInput("select_file", label = "Select Input File"),
      textInput("minParentAge", label = "Minimum Parent Age (years)",
                   value = "2.5")
      ),
    # div(6,
    #   style = paste(
    #     "float: left; width: 400px; height: 100%; padding: 10px;",
    #     "border: 1px solid lightgray; background-color: #EDEDED;",
    #     "margin-left: 3px;",
    #     "border-radius: 25px; box-shadow: 0 0 5px 2px #888"
    #   ),
    #   helpText(
    #     "Select a genotype file corresponding to the pedigree file.
    #     See the 'Format' tab for file formatting descriptions."
    #   ),
    #
    #   radioButtons(
    #     'sep',
    #     label = 'Separator',
    #     choices = list(
    #       'Comma' = ',',
    #       'Semicolon' = ';',
    #       'Tab' = '\t'
    #     ),
    #     selected = ','
    #   ),
    #
    #   fileInput("genotype_file", label = "Select Input File")
    #
    #   ),


    # Main Panel
    div(style = "margin-left:425px;padding:10px;",
        includeHTML("../extdata/input_format.html"))
  )
