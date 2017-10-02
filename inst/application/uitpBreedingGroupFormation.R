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
    fluidRow(
      column(width = 3,
             style = paste(
               " padding: 10px; border: 1px solid lightgray; background-color: #EDEDED;",
               "border-radius: 25px; box-shadow: 0 0 5px 2px #888"
             ),
             includeHTML("../extdata/group_formation.html"),
      checkboxInput('low_val', label = "Include low-value animals in group formation",
                    value = FALSE),
      checkboxInput('ff_rel', label = "Ignore relatedness between females",
                    value = TRUE)
    ),

    #Main Panel
    column(9,
      #style = "margin-left: 425px; padding: 10px; width: 550px;",
      div(
        style = "display:inline-block;width:250px;padding:10px",
        numericInput(
          "numGp",
          label = "Groups Desired:",
          value = 1,
          min = 1,
          max = 10
        )
      ),
      div(
        style = "display:inline-block;width:250px;padding:10px",
        selectInput(
          "kin_thresh",
          label = "Animal kinship will be ignored below:",
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
        numericInput(
          "min_age",
          label = "Animal age will be ignored at or below:",
          value = 1,
          min = 0,
          max = 40,
          step = 0.1
        )
      ),
      div(
        style = "display:inline-block;width:250px;padding:10px",
        numericInput(
          "gp_iter",
          label = "Number of simulations:",
          value = 10,
          min = 1,
          max = 1000000
        )
      ),

      div(
        style = "display:inline-block;width:550px;padding:10px",
        div(
          style = "display:inline-block;width:250px;padding:10px",
          helpText("Enter IDs of candidate group members:"),
          tags$textarea(id = "grp_ids", rows = 5, cols = 20, "")
        ),
        div(
          style = "display:inline-block;width:250px;padding:10px",
          helpText("Enter IDs of animals currently in the group:"),
          tags$textarea(id = "cur_grp", rows = 5, cols = 20, "")
        ),
        actionButton("grp_sim", label = "Make Groups"),
        checkboxInput("withKin", label = "With Kinship Coefficents",
                      value = FALSE),
        helpText(
          "If no candidate IDs are specified above or in the input pedigree,
          all population members will be used."
        )
        ),
      div(
        style = "display:inline-block;width:250px;padding:5px",
        selectInput(
          "view_grp",
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
      ),
      #tableOutput("breeding_groups")
      DT::dataTableOutput("breeding_groups"),
      DT::dataTableOutput("breeding_groupKin")
      )

    )
  )
