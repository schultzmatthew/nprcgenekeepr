uitpBreedingGroupFormation <-
  tabPanel(
    "Breeding Group Formation",
    div(
      fluidRow(column(
        5,
        offset = 1,
        style = paste0(
          "border: 1px solid lightgray; background-color: #EDEDED; ",
          "border-radius: 1px; box-shadow: 0 0 5px 2px #888"
        ),
        radioButtons(
          "group_formation_rb",
          "Choose one group formation workflow:",
          choiceNames = list(
            paste0(
              "Randomly select from only high-value ",
              "animals in genetic value analysis"
            ),
            paste0("Randomly select from all animals in  ",
                   "genetic value analysis"),
            paste0("Use candidate animals entered below  ",
                   "to form groups")
          ),
          choiceValues = list("high-value", "all", "candidates"),
          width = "650px",
          selected = character(0)
        ),
        conditionalPanel(
          condition = paste0("input.group_formation_rb == 'candidates' "),
          div(
            style = "display:inline-block;width:400px;padding:5px",
            helpText("Enter IDs of candidates to be added to new group(s):"),
            tags$textarea(id = "grpIds", rows = 10, cols = 50, "")
          )
        )
      ),
      column(
        2,
        offset = 1,
        conditionalPanel(
          condition = "input.group_formation_rb == 'high-value' |
                       input.group_formation_rb == 'all' |
                       input.group_formation_rb == 'candidates'",
          div(
            style = paste0(
              "border: 1px solid #4BCEEF; background-color: #4BCEEF; ",
              "border-radius: 1px; box-shadow: 0 0 5px 2px #888"
            ),
            actionButton("grpSim", label = "Make Groups", width = "100%",
                         style = "color: #fff; background-color: #4BCEEF;
                                      border-color: #4BCEEF")
          )
        )
      )),
      fluidRow(column(
        10,
        offset = 1,
        checkboxInput("seedAnimals", label = "Add seed animals to group",
                      value = FALSE)
      )),
      fluidRow(
        column(
          5,
          offset = 1,
          style = "display:inline-block;width:400px;padding:10px",

          conditionalPanel(
            condition = paste0("input.seedAnimals"),
            div(
              style = "display:inline-block;width:400px;padding:10px",
              helpText("Enter IDs of seed animals for the new group 1:"),
              tags$textarea(id = "curGrp1", rows = 5, cols = 40, ""),
              helpText("Enter IDs of seed animals for the new group 3:"),
              tags$textarea(id = "curGrp3", rows = 5, cols = 40, ""),
              helpText("Enter IDs of seed animals for the new group 5:"),
              tags$textarea(id = "curGrp5", rows = 5, cols = 40, "")
            )
          )
        ),
        column(
          5,
          offset = 1,
          style = "display:inline-block;width:400px;padding:10px",

          conditionalPanel(
            condition = paste0("input.seedAnimals"),
            div(
              style = "display:inline-block;width:400px;padding:10px",
              helpText("Enter IDs of seed animals for the new group 2:"),
              tags$textarea(id = "curGrp2", rows = 5, cols = 40, ""),
              helpText("Enter IDs of seed animals for the new group 4:"),
              tags$textarea(id = "curGrp4", rows = 5, cols = 40, ""),
              helpText("Enter IDs of seed animals for the new group 6:"),
              tags$textarea(id = "curGrp6", rows = 5, cols = 40, "")
            )
          )
        )
      ),
      fluidRow(
        column(
          4,
          offset = 1,
          style = paste(
            "border: 1px solid lightgray; background-color: #EDEDED;",
            "border-radius: 1px; box-shadow: 0 0 5px 2px #888"
          ),
          HTML(
            "<p>Instead of assignment of animals to groups without regard to
            the sex of animals, you may choose to form <em>harem</em> groups
            or groups with a specified sex ratio.</p>"
          ),
          radioButtons(
            "group_sex_rb",
            "Sex of animals in groups:",
            choiceNames = list(
              "Ignore sex when forming groups ",
              "Form harems (one male)",
              "User specified sex ratio"
            ),
            choiceValues = list("ignore-sex", "harems", "sex-ratio"),
            width = "350px",
            selected = "ignore-sex"
          ),
          conditionalPanel(
            condition = "input.group_sex_rb == 'sex-ratio'",
            div(popify(
              numericInput(
                "sexRatio",
                label = "Sex Ratio (F/M):",
                value = 0.0,
                min = 0.5,
                max = 10,
                step = 0.5
              ),
              NULL,
              paste0(
                "Final sex ratios are approximate but are guaranteed to be ",
                "the nearest possible value given creation of the largest ",
                "group possible."
              )
            )))
        ),
        column(
          3,
          offset = 1,
          align = "center",
          style = "height:200px;padding-top:20px;",
          div(
             style = "height:100px;padding-top:20px;",
            checkboxInput("useMinParentAge", 
            label = paste0("Animals will be grouped with the mother below the ",
                           "minimum parent age ("),# input.minParentAge, ")."),
                          value = FALSE)
          ),
          conditionalPanel(
            condition = "!input.useMinParentAge",
            div(popify(
          numericInput(
            "minAge",
            label = "Animals will be grouped with the mother below age:",
            value = 1,
            min = 0,
            max = 40,
            step = 0.1
          ),
              NULL,
              paste0(
                "Animals will be groups with the mother below the age you ",
                "select."
              )
            ))),
          div(
            style = "height:100px;padding-top:20px;",
            checkboxInput("ffRel", label = "Ignore relatedness between females",
                          value = TRUE)
          )
        ),
        column(
          3,
          offset = 0,
          style = "height:200px;padding-top:20px;",
          selectInput(
            "kinThresh",
            label = "Animals with kinship above this value will be excluded:",
            choices = list(
              "0.015625" = 0.015625,
              "0.0625" = 0.0625,
              "Grandparent-Offspring" = 0.125,
              "0.25 (Parent-Offspring)" = 0.25
            ),
            selected = 1
          )
        )
      ),
      fluidRow(
        column(
          2,
          offset = 1,
          style = "height:250px;padding-top:20px;",
          numericInput(
            "numGp",
            label = "Groups Desired:",
            value = 1,
            min = 1,
            max = 10
          ),
          checkboxInput("withKin",
                        label = "Include kinship in display of groups",
                        value = FALSE)
        ),
        column(
          2,
          offset = 0,
          style = "height:250px;padding-top:20px;",
          numericInput(
            "gpIter",
            label = "Number of simulations:",
            value = 10,
            min = 1,
            max = 1000000
          )
        ),
        column(
          3,
          offset = 2,
          style = "height:250px;padding-top:20px;",
          numericInput(
            "viewGrp",
            label = "Enter the group to view:",
            value = 1,
            min = 1,
            max = 10
          ),
          div(
            style = "display:inline-block;width:250px;padding:5px",
            downloadButton("downloadGroup", "Export Current Group"),
            downloadButton("downloadGroupKin",
                           "Export Current Group Kinship Matrix")
          )
        )
      ),
      fluidRow(
        column(
          width = 10,
          offset = 1,
          DT::dataTableOutput("breedingGroups"),
          DT::dataTableOutput("breedingGroupKin")
        )
      ),
      fluidRow(
        column(
          width = 10,
          offset = 1,
          style = paste0(
            "border: 1px solid lightgray; background-color: #EDEDED; ",
            "border-radius: 15px; box-shadow: 0 0 5px 2px #888"
          ),
          includeHTML("../extdata/group_formation.html")
        )
      )
    )
  )
