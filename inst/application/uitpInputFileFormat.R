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
    sidebarLayout(
      sidebarPanel(
        style = paste(
          "float: left; width: 400px; height: 100%; padding: 10px;",
          "border: 1px solid lightgray; background-color: #EDEDED;",
          "margin-left: 3px;",
          "border-radius: 25px; box-shadow: 0 0 5px 2px #888"
        ),
        helpText(
          "Select how you are submitting data.
          See the Input File Handling information on the right for
          file content and format specifications."
        ),
        radioButtons(
          "dataSource",
          label = "File Type(s)",
          choices = list(
            "Pedigree(s) file only; genotypes not provided" = "pedFile",
            "Pedigree(s) and genotypes in one file" = "commonPedGenoFile",
            "Pedigree(s) and genotypes in separate files" = "separatePedGenoFile"
          ),
          selected = "pedFile"
        ),
        conditionalPanel(
          condition = "input.dataSource == 'pedFile'",
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
          fileInput("pedigreeFile", label = "Select Pedigree File")
        ),
        conditionalPanel(
          condition = "input.dataSource == 'commonPedGenoFile'",
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
          fileInput("pedigreeFile", label = "Select Pedigree-Genotype File")
        ),
        conditionalPanel(
          condition = "input.dataSource == 'separatePedGenoFile'",
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
          fileInput("pedigreeFile", label = "Select Pedigree File"),
          fileInput("genotypeFile", label = "Select Genotype File")
        ),
        textInput("minParentAge", label = "Minimum Parent Age (years)",
                  value = "2.5"),
        helpText(
          "If a parent is not at least as old as the minimum parent age
          on the birth date of an offspring in the pedigree, the input
          file will not be accepted and a file named lowParentAge.csv will
          be written to the users home directory. Animals without birth dates
          are not considered."
        ),
        actionButton("getData", "Read files now.")
      ),
      # Main Panel
      mainPanel(#style = "margin-left:425px;padding:10px;",
        includeHTML("../extdata/input_format.html"))
    )
  )
