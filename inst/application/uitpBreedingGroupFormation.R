uitpBreedingGroupFormation <-
  tabPanel(
    "Breeding Group Formation",
    div(
      div(
        fluidRow(
          column(10, offset = 1,
                 style = paste(
                   "border: 1px solid lightgray; background-color: #EDEDED;",
                   "border-radius: 25px; box-shadow: 0 0 5px 2px #888"
                 ),
                 radioButtons("group_formation_rb",
                              "Choose one group formation workflow:",
                              choiceNames = list(
                                paste0("Randomly select from only high-value ",
                                       "animals in genetic value analysis"),
                                paste0("Randomly select from all animals in  ",
                                       "genetic value analysis"),
                                paste0("Use candidate animals entered below  ",
                                       "to form groups")
                              ),
                              choiceValues = list(
                                "high-value", "all", "candidates"
                              ),
                              width = "650px", selected = character(0))
          )),
        fluidRow(
          column(10, offset = 1,
          conditionalPanel(
            condition = "input.group_formation_rb == 'candidates'",
            div(
              style = "display:inline-block;width:350px;padding:10px",
              helpText("Enter IDs of candidates to be added to new group(s):"),
              tags$textarea(id = "grpIds", rows = 10, cols = 40, "")
            )
          ))),
        fluidRow(
          column(10, offset = 1,
                 style = "display:inline-block;width:250px;padding:10px",
        checkboxInput("seedAnimals", label = "Add seed animals to group",
                      value = FALSE),

      conditionalPanel(
        condition = "input.seedAnimals",
        div(
        style = "display:inline-block;width:350px;padding:10px",
        helpText("Enter IDs of seed animals for the new group(s):"),
        tags$textarea(id = "curGrp", rows = 10, cols = 40, "")
        )
      ))),
     fluidRow(
          column(5, offset = 0,
                 style = paste(
                   "border: 1px solid lightgray; background-color: #EDEDED;",
                   "border-radius: 25px; box-shadow: 0 0 5px 2px #888"
                 ),
          HTML(
            "<p>Instead of assignment of animals to groups without regard to
            the sex of animals, you may choose to form <em>harem</em> groups
            or groups with a specified sex ratio.</p>"
          ),
          helpText("Adjust Group Sex Ratio: harem(s) or other sex ratios."),
          radioButtons("group_sex_rb",
                       "Sex of animals in groups:",
                       choiceNames = list(
                         "Ignore sex when forming groups ",
                          "Form harems (one male)",
                          "User specified sex ratio"
                       ),
                       choiceValues = list(
                         "ignore-sex", "harems", "sex-ratio"
                       ),
                       width = "350px", selected = "ignore-sex"),
         div(
          conditionalPanel(
            condition = "input.group_sex_rb == 'sex-ratio'",
            div(
              popify(numericInput(
                "sexRatio",
                label = "Sex Ratio (F/M):",
                value = 0.0,
                min = 0.5,
                max = 10,
                step = 0.5
              ), NULL, paste0(
                "Final sex ratios are approximate but are guaranteed to be the ",
                "nearest possible value given creation of the largest group ",
                "possible."))
            )
          ),
          div(
          style = "display:inline-block;width:150px;padding:10px",
          numericInput(
            "minAge",
            label = "Animals will be ignored below age:",
            value = 1,
            min = 0,
            max = 40,
            step = 0.1
          )),
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
         ),
         div(
           style = "display:inline-block;width:250px;padding:10px",
           checkboxInput("ffRel", label = "Ignore relatedness between females",
                         value = TRUE)
         ),
         numericInput(
          "numGp",
          label = "Groups Desired:",
          value = 1,
          min = 1,
          max = 10
        ),


        style = "display:inline-block;width:150px;padding:10px",
        numericInput(
          "gpIter",
          label = "Number of simulations:",
          value = 10,
          min = 1,
          max = 1000000
        )),
        column(5, offset = 1,
               style = paste(
                 "border: 1px solid lightgray; background-color: #EDEDED;",
                 "border-radius: 25px; box-shadow: 0 0 5px 2px #888"
               ),
               checkboxInput("withKin", label = "With Kinship Coefficents",
                    value = FALSE),
      actionButton("grpSim", label = "Make Groups"),
      helpText(
        "If no candidate IDs are specified above or in the input pedigree,
        all population members will be used."),
      div(
        style = "display:inline-block;width:250px;padding:5px",
        numericInput(
          "viewGrp",
          label = "Enter the group to view:",
          value = 1,
          min = 1,
          max = 10
        )
      ),
      div(
        style = "display:inline-block;width:250px;padding:5px",
        downloadButton("downloadGroup", "Export Current Group"),
        downloadButton("downloadGroupKin",
                       "Export Current Group Kinship Matrix")
      ))
      )),
      fluidRow(
        column(width = 12, offset = 0,
               style = paste(
                 "border: 1px solid lightgray; background-color: #EDEDED;",
                 "border-radius: 25px; box-shadow: 0 0 5px 2px #888"
               ),

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
))))
