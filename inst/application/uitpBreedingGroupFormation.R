uitpBreedingGroupFormation <-
  tabPanel(
    "Breeding Group Formation",

    #Side Panel
    # div(
    #  style = paste(
    #    "float: left; width: 400px; height: 62vh; padding: 10px;",
    #    "border: 1px solid lightgray; background-color: #EDEDED;",
    #    "margin-left: 3px; margin-top: 3px; margin-bottom: 3px;",
    #    "border-radius: 25px; box-shadow: 0 0 5px 2px #888"
    #  ),
    div(
      div(
        style = "display:inline-block;width:800px;padding:10px",
        div(
          style = "display:inline-block;width:350px;padding:10px",
          helpText("Enter IDs of seed animals for the new group(s):"),
          tags$textarea(id = "curGrp", rows = 10, cols = 40, "")
        ),
        div(
          style = "display:inline-block;width:350px;padding:10px",
          helpText("Enter IDs of candidates to be added to new group(s):"),
          tags$textarea(id = "grpIds", rows = 10, cols = 40, "")
        )
      ),
      fluidRow(
        column(width = 3,
        div(
        style = "display:inline-block;width:800px;padding:10px",
        div(
        style = "display:inline-block;width:150px;padding:10px",
        numericInput(
          "numGp",
          label = "Groups Desired:",
          value = 1,
          min = 1,
          max = 10
        ),

        style = "display:inline-block;width:150px;padding:10px",
        numericInput(
          "minAge",
          label = "Animals will be ignored below age:",
          value = 1,
          min = 0,
          max = 40,
          step = 0.1
        ),

        style = "display:inline-block;width:150px;padding:10px",
        numericInput(
          "gpIter",
          label = "Number of simulations:",
          value = 10,
          min = 1,
          max = 1000000
        )
      ))),
        column(width = 3,
               div(
      div(
        style = "display:inline-block;width:150px;padding:10px",
        selectInput(
          "kinThresh",
          label = "Animals will be ignored with kinship below:",
          choices = list(
            "0.015625" = 0.015625,
            "0.0625" = 0.0625,
            "0.125" = 0.125,
            "0.25" = 0.25
          ),
          selected = 1
        )
      ),
      div(
        style = "display:inline-block;width:250px;padding:10px",
        checkboxInput("lowVal",
                      label = "Include low-value animals in group formation",
                      value = FALSE)
      ),
      div(
        style = "display:inline-block;width:250px;padding:10px",
        checkboxInput("ffRel", label = "Ignore relatedness between females",
                      value = TRUE)
      ),
      checkboxInput("withKin", label = "With Kinship Coefficents",
                    value = FALSE)
        )),
      column(width = 3,
        div(
        actionButton("grpSim", label = "Make Groups"),
        helpText(
          "If no candidate IDs are specified above or in the input pedigree,
          all population members will be used."
        ),
      div(
        style = "display:inline-block;width:250px;padding:5px",
        selectInput(
          "viewGrp",
          label = "Enter the group to view:",
          choices = list("Group 1" = 1),
          selected = 1
        )
      ),
      div(
        style = "display:inline-block;width:250px;padding:5px",
        downloadButton("downloadGroup", "Export Current Group"),
        downloadButton("downloadGroupKin",
                       "Export Current Group Kinship Matrix")
      )))),
      DT::dataTableOutput("breedingGroups"),
      DT::dataTableOutput("breedingGroupKin"),

    div(
      style = paste(
        " padding: 10px; border: 1px solid lightgray; ",
        "background-color: #EDEDED;",
        "border-radius: 25px; box-shadow: 0 0 5px 2px #888"
      ),
      includeHTML("../extdata/group_formation.html")
    )
  )
)
