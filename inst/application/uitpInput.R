uitpInput <-
  tabPanel(
    "Input",

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
          "float: left; width: 360px; height: 100%; padding: 10px;",
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
            "Pedigree(s) and genotypes in separate files" =
              "separatePedGenoFile",
            "Breeders only; pedigree obtained from database" = "breeders"
          ),
          selected = NULL
        ),
        conditionalPanel(
          condition = "input.dataSource == 'pedFile'",
          radioButtons(
            "sepOne",
            label = "Separator",
            choices = list(
              "Comma" = ",",
              "Semicolon" = ";",
              "Tab" = "\t"
            ),
            selected = ","
          ),
          fileInput("pedigreeFileOne", label = "Select Pedigree File")
        ),
        conditionalPanel(
          condition = "input.dataSource == 'commonPedGenoFile'",
          radioButtons(
            "sepTwo",
            label = "Separator",
            choices = list(
              "Comma" = ",",
              "Semicolon" = ";",
              "Tab" = "\t"
            ),
            selected = ","
          ),
          fileInput("pedigreeFileTwo", label = "Select Pedigree-Genotype File")
        ),
        conditionalPanel(
          condition = "input.dataSource == 'separatePedGenoFile'",
          radioButtons(
            "sepThree",
            label = "Separator",
            choices = list(
              "Comma" = ",",
              "Semicolon" = ";",
              "Tab" = "\t"
            ),
            selected = ","
          ),
          fileInput("pedigreeFileThree", label = "Select Pedigree File"),
          fileInput("genotypeFile", label = "Select Genotype File")
        ),
        conditionalPanel(
          condition = "input.dataSource == 'breeders'",
          radioButtons(
            "sepFour",
            label = "Separator",
            choices = list(
              "Comma" = ",",
              "Semicolon" = ";",
              "Tab" = "\t"
            ),
            selected = ","
          ),
          fileInput("breederFile", label = "Select Breeder File")
        ),
        textInput("minParentAge", label = "Minimum Parent Age (years)",
                  value = "0.0"),
        helpText(
          "If a parent is not at least as old as the minimum parent age
          on the birth date of an offspring in the pedigree, the input
          file will not be accepted and a file named lowParentAge.csv will
          be written to the users home directory. Animals without birth dates
          are not considered."
        ),
        actionButton("getData", "Read files now"),
        checkboxInput("debugger", label = "Debug on", value = FALSE)#,
        #verbatimTextOutput("dBug")
        ),
      # Main Panel
      mainPanel(#style = "margin-left:425px;padding:10px;",
        includeHTML("../extdata/input_format.html"))
    )
  )
