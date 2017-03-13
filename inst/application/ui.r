library(shiny)
if (!require("DT")) install.packages('DT')
library(DT)

shinyUI(tagList(
  tags$head(
    tags$style(HTML(
      ".progress-striped .bar {background-color: #149bdf;
       background-image: -webkit-gradient(linear, 0 100%, 100% 0, color-stop(0.25, rgba(255, 255, 255, 0.6)), color-stop(0.25, transparent), color-stop(0.5, transparent), color-stop(0.5, rgba(255, 255, 255, 0.6)), color-stop(0.75, rgba(255, 255, 255, 0.6)), color-stop(0.75, transparent), to(transparent));
       background-image: -webkit-linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
       background-image: -moz-linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
       background-image: -o-linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
       background-image: linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
       -webkit-background-size: 40px 40px;
       -moz-background-size: 40px 40px;
       -o-background-size: 40px 40px;
       background-size: 40px 40px;
       }
   "))),
  navbarPage(
    title = NULL,

    tabPanel("Input File Format",

             tags$style(type = "text/css",
                        "table {border: 1px solid black; width: 100%; padding: 15px;}",
                        "tr, td, th {border: 1px solid black; padding: 5px;}",
                        "th {font-weight: bold; background-color: #7CFC00;}",
                        "hr {border-width:2px;border-color:#A9A9A9;}"
             ),

             titlePanel(div(style = "height:125px;width:100%",
                            div(style = "float:left;width:45%",
                                img(src = "../snprc-new2color.png", height = 80, width = 400)),
                            div(style = "float:right;text-align:right;width:45%",
                                "Genetic Management Tools")
             )
             ),

             # Side Panel
             div(style = paste("float: left; width: 400px; height: 100%; padding: 10px;",
                             "border: 1px solid lightgray; background-color: #EDEDED;",
                             "margin-left: 3px;",
                             "border-radius: 25px; box-shadow: 0 0 5px 2px #888"),
                 helpText("Select a pedigree file for analysis.
                          See the 'Format' tab for file formatting descriptions."),

                 radioButtons('sep',
                              label = 'Separator',
                              choices = list('Comma' = ',', 'Semicolon' = ';', 'Tab' = '\t'),
                              selected = ','),

                 fileInput("select_file", label = "Select Input File")

                 ),

             # Main Panel
             div(style = "margin-left:425px;padding:10px;",
                 includeHTML("../extdata/input_format.html")
             )
    ),

    tabPanel("Pedigree Browser",

             # Side Panel
             div(
               div(style = paste("float: left; width: 400px; height: 350px; padding: 10px;",
                               "border: 1px solid lightgray; background-color: #EDEDED;",
                               "margin-left: 3px; margin-top: 3px; margin-bottom: 3px;",
                               "border-radius: 25px; box-shadow: 0 0 5px 2px #888"),
                   includeHTML("../extdata/pedigree_browser.html")
               ),

               # Main Panel
               div(style = "margin-left: 425px; width: 550px; padding: 10px;",
                   helpText("If analysis of all individuals is not needed, IDs of selected breeding population members may be manually entered here  (IDs may be pasted from Excel):"),
                   div(style = "display:inline-block;width:250px;padding:10px",
                       tags$textarea(id = "population_ids", rows = 5, cols = 20, ""),
                       actionButton("specify_pop", label = "Update Breeding Colony")
                   ),
                   div(style = "display:inline-block;width:250px;padding:10px",
                       checkboxInput('uid', label = "Display UIDs for partial parentage",
                                     value = TRUE),
                       checkboxInput('trim', label = 'Trim pedigree based on specified population',
                                     value = FALSE),
                       downloadButton("downloadPedigree", "Export")
                   ),
                   helpText("(a population must be defined before proceeding
                          to the Genetic Value Analysis)")

               )
             ),
             hr(),
             #DT::dataTableOutput("pedigree",width = "100%")
             DT::dataTableOutput("pedigree")
    ),

    tabPanel("Genetic Value Analysis",

             div(style = "min-width:1000px",
                 # Side Panel
                 div(style = paste("float: left; width: 400px; height: 450px; padding: 10px;",
                                 "border: 1px solid lightgray; background-color: #EDEDED;",
                                 "margin-left:3px; margin-top: 3px; margin-bottom: 3px;",
                                 "border-radius: 25px; box-shadow: 0 0 5px 2px #888"),
                     includeHTML("../extdata/genetic_value.html")
                 ),

                 # Main Panel
                 div(style = "margin-left:425px;padding:10px;",

                     div(style = "display:inline-block;width:250px;padding:10px",
                         numericInput("iterations", label = "Enter the number of simulations for the gene-drop analysis:",
                                      value = 1000, min = 2, max = 100000)),
                     div(style = "display:inline-block;width:250px;padding:10px",
                         selectInput("threshold", label = "Enter the genome uniqueness threshold:",
                                     choices = list("0" = 1, "1" = 2, "2" = 3, "3" = 4, "4" = 5),
                                     selected = 1)),
                     helpText("The analysis may take a significant amount of time (>20 min)"),
                     actionButton("analysis", label = "Begin Analysis"),
                     br(),
                     hr(),
                     helpText(h4("Results:")),
                     helpText("Enter IDs of specific animals to be viewed:"),
                     helpText("(Leave blank to view all)"),
                     div(style = "display:inline-block;width:250px;padding:10px",
                         tags$textarea(id = "view_ids", rows = 5, cols = 20, ""),
                         actionButton("view", label = "Filter View")
                     ),
                     div(style = "display:inline-block;width:250px;padding:10px",
                         downloadButton("downloadGVA_full", "Export All"),
                         br(),
                         br(),
                         downloadButton("downloadGVA_subset", "Export Current Subset")
                     )

                 )
             ),
             DT::dataTableOutput("gva")
             #htmlOutput("gva")
    ),

    tabPanel("Summary Statistics",

             # Side Panel
             div(style = paste("float: left; width: 400px; height: 100vh; padding: 10px;",
                             "border: 1px solid lightgray; background-color: #EDEDED;",
                             "margin-left: 3px; margin-top: 3px; margin-bottom: 3px;",
                             "border-radius: 25px; box-shadow: 0 0 5px 2px #888"),
                 includeHTML("../extdata/summary_stats.html"),
                 downloadButton("downloadKinship", "Export Kinship Matrix"),
                 br(),
                 br(),
                 downloadButton("downloadFirstOrder", "Export First-Order Relationships"),
                 br()
             ),

             # Main Panel
             div(style = "margin-left:425px;padding:10px;",
                 htmlOutput("summary_stats"),
                 plotOutput("mk_plot", width = "400px", height = "400px"),
                 plotOutput("zscore_plot", width = "400px", height = "400px"),
                 #tableOutput("relations")
                 DT::dataTableOutput("relations")
             )
    ),

    tabPanel("Breeding Group Formation",

             #Side Panel
             div(style = paste("float: left; width: 400px; height: 100vh; padding: 10px;",
                             "border: 1px solid lightgray; background-color: #EDEDED;",
                             "margin-left: 3px; margin-top: 3px; margin-bottom: 3px;",
                             "border-radius: 25px; box-shadow: 0 0 5px 2px #888"),
                 includeHTML("../extdata/group_formation.html"),
                 checkboxInput('low_val', label = "Include low-value animals in group formation",
                               value = FALSE),
                 checkboxInput('ff_rel', label = "Ignore relatedness between females",
                               value = TRUE)
             ),

             #Main Panel
             div(style = "margin-left: 425px; padding: 10px; width: 550px;",
                 div(style = "display:inline-block;width:250px;padding:10px",
                     numericInput("numGp", label = "Groups Desired:",
                                  value = 1, min = 1, max = 10)),
                 div(style = "display:inline-block;width:250px;padding:10px",
                     selectInput("kin_thresh", label = "Animal kinship will be ignored below:",
                                 choices = list("0.015625" = 0.015625,
                                                "0.0625" = 0.0625,
                                                "0.125" = 0.125,
                                                "0.25" = 0.25),
                                 selected = 1)),

                 div(style = "display:inline-block;width:250px;padding:10px",
                     numericInput("min_age", label = "Animal age will be ignored at or below:",
                                  value = 1, min = 0, max = 40, step = 0.1)),
                 div(style = "display:inline-block;width:250px;padding:10px",
                     numericInput("gp_iter", label = "Number of simulations:",
                                  value = 10, min = 1, max = 10000)),

                 div(style = "display:inline-block;width:550px;padding:10px",
                     div(style = "display:inline-block;width:250px;padding:10px",
                         helpText("Enter IDs of candidate group members:"),
                         tags$textarea(id = "grp_ids", rows = 5, cols = 20, "")
                     ),
                     div(style = "display:inline-block;width:250px;padding:10px",
                         helpText("Enter IDs of animals currently in the group:"),
                         tags$textarea(id = "cur_grp", rows = 5, cols = 20, "")
                     ),
                     actionButton("grp_sim", label = "Make Groups"),
                     helpText("If no candidate IDs are specified above or in the input pedigree,
                             all population members will be used.")),
                 div(style = "display:inline-block;width:250px;padding:5px",
                     selectInput("view_grp", label = "Enter the group to view:",
                                 choices = list("Group 1" = 1), selected = 1)
                 ),
                 div(style = "display:inline-block;width:250px;padding:5px",
                     downloadButton("downloadGroup", "Export Current Group")
                 ),
                 #tableOutput("breeding_groups")
                 DT::dataTableOutput("breeding_groups")
             )

    ),

    tabPanel("ORIP Reporting",

             # Side Panel
             div(style = paste("float: left; width: 400px; height: 100vh; padding: 10px;",
                             "border: 1px solid lightgray; background-color: #EDEDED;",
                             "margin-left: 3px; margin-top: 3px;",
                             "border-radius: 25px; box-shadow: 0 0 5px 2px #888"),
                 helpText("This tab will eventually contain a report formatted
                         for submission to ORIP.")
             ),

             # Main Panel
             div(style = "margin-left:425px;padding:10px;",
                 img(src = "../../extdata/www/under_construction.jpg", height = 300, width = 600)
             )

    ),
    #source("ui_pyramid_plot.R", local = TRUE),
    tabPanel("Pyramid Plot",

             div(style = "min-width:1000px",
                 # Side Panel
                 div(style = paste("float: left; width: 400px; height: 450px; padding: 10px;",
                                   "border: 1px solid lightgray; background-color: #EDEDED;",
                                   "margin-left:3px; margin-top: 3px; margin-bottom: 3px;",
                                   "border-radius: 25px; box-shadow: 0 0 5px 2px #888"),
                     includeHTML("../extdata/pyramid_plot.html")
                 ),

                 # Main Panel
                 div(style = "margin-left:425px;padding:10px;",
                     get_pyramid_plot())
             )),
    tabPanel("README",

             includeHTML("../extdata/readme.html")

    ),
    id = "tab_pages"
  )
))
