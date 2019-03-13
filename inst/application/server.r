`%then%` <- shiny:::`%OR%`
library(futile.logger)
library(ggplot2)
library(DT)
shinyServer(function(input, output, session) {
  errorLst <- getEmptyErrorLst()
  nprcmanagerLog <- paste0(getSiteInfo()$homeDir, "nprcmanager.log")
  flog.logger("nprcmanager", INFO,
              appender = appender.file(nprcmanagerLog))

  #############################################################################
  # Functions for handling initial pedigree upload and QC
#  source("../application/sreactiveGetSelectedBreeders.R")
  getSelectedBreeders <- reactive({
    input$getData # This button starts it all
    if (input$debugger) {
      flog.threshold(DEBUG, name = "nprcmanager")
    } else {
      flog.threshold(INFO, name = "nprcmanager")
    }
    isolate({
      flog.debug(paste0("1st. input$dataSource: ", input$dataSource,
                        "; input$sepOne: ", input$sepOne,
                        "; input$sepTwo: ", input$sepTwo,
                        "; input$sepThree: ", input$sepThree),
                 name = "nprcmanager")
      if (input$dataSource == "pedFile") {
        sep <- input$sepOne
        pedigreeFile <- input$pedigreeFileOne
        flog.debug(paste0("pedigreeFileOne - pedigreeFile$name: ",
                          pedigreeFile$name,
                          "; pedigreeFile$datapath: ", pedigreeFile$datapath),
                   name = "nprcmanager")
      } else if (input$dataSource == "commonPedGenoFile") {
        sep <- input$sepTwo
        pedigreeFile <- input$pedigreeFileTwo
        flog.debug(paste0("pedigreeFileTwo - pedigreeFile$name: ",
                          pedigreeFile$name,
                          "; pedigreeFile$datapath: ", pedigreeFile$datapath),
                   name = "nprcmanager")
      } else if (input$dataSource == "separatePedGenoFile") {
        sep <- input$sepThree
        pedigreeFile <- input$pedigreeFileThree
        genotypeFile <- input$genotypeFile
        flog.debug(paste0("pedigreeFileThree - pedigreeFile$name: ",
                          pedigreeFile$name, "; ",
                          "; pedigreeFile$datapath: ", pedigreeFile$datapath,
                          "; genotypeFile$name: ", genotypeFile$name,
                          "; genotypeFile$datapath: ", genotypeFile$datapath),
                   name = "nprcmanager")
      } else if (input$dataSource == "breeders") {
        sep <- input$sepFour
        pedigreeFile <- input$breederFile
        flog.debug(paste0("breederFile - pedigreeFile$name: ",
                          pedigreeFile$name, "; ",
                          "pedigreeFile$datapath: ", pedigreeFile$datapath),
                   name = "nprcmanager")
      } else {
        stop("Data source was not defined.")
      }
      # The minParentAge -- numeric values to set the minimum age in years for
      # an animal to have an offspring. Defaults to 2 years. The check is not
      # performed for animals with missing birth dates. See qcStudbook().
      flog.debug("sep: %s", sep, name = "nprcmanager")
      minParentAge <- tryCatch(as.numeric(input$minParentAge),
                               warning = function(cond) {
                                 return(NULL)
                               },
                               error = function(cond) {
                                 return(NULL)
                               }
      )
      globalMinParentAge <<- minParentAge
      flog.debug(paste0("minParentAge: ", minParentAge),
                 name = "nprcmanager")

      # pedigreeFile and breederFile will be NULL initially.
      # After the user selects a file, it will be a filepath.
      if (is.null(pedigreeFile)) {
        return(NULL)
      }
      flog.debug(paste0("before read.table input$dataSource: ",
                        input$dataSource),
                 name = "nprcmanager")
      # Load pedigree table
      if (input$dataSource == "breeders") {
        flog.debug(paste0("before getBreederPed: ", pedigreeFile$name),
                   name = "nprcmanager")
        breederPed <- getBreederPed(pedigreeFile$datapath, sep = sep)
        if (any("nprcmanagErr" %in% class(breederPed))) {
            errorLst <- breederPed
            breederPed <- NULL
        } else if (is.null(breederPed)) {
          flog.debug(paste0("after getBreederPed: ", pedigreeFile$name,
                            "; NULL was returned by getBreederPed function"),
                     name = "nprcmanager")
        } else {
          flog.debug(paste0("after getBreederPed: ", pedigreeFile$name,
                            "; contents rows: ", nrow(breederPed),
                            ", columns: ", ncol(breederPed), "; col names: '",
                            paste(names(breederPed), collapse = "', '"), "'",
                            sep = ""),
                     name = "nprcmanager")
        }
      } else {
        breederPed <- read.table(pedigreeFile$datapath,
                      header = TRUE,
                      sep = sep,
                      stringsAsFactors = FALSE,
                      na.strings = c("", "NA"),
                      check.names = FALSE)
        flog.debug(paste0("after read.table pedigreeFile$name: ",
                          pedigreeFile$name,
                          "; contents rows: ", nrow(breederPed),
                          ", columns: ", ncol(breederPed), "; col names: '",
                          paste(names(breederPed), collapse = "', '"), "'",
                          sep = ""),
                   name = "nprcmanager")
      }

      if (is.null(input$dataSource)) {
        stop("Did not expect input$dataSource to be NULL")
      } else if (input$dataSource == "separatePedGenoFile") {
        # Load pedigree table
        flog.debug(paste0("before read.table genotypeFile$datapath: ",
                          genotypeFile$datapath,
                          "; contents rows: ", nrow(breederPed),
                          ", columns: ", ncol(breederPed), "; col names: '",
                          paste(names(breederPed), collapse = "', '"), "'",
                          sep = ""),
                   name = "nprcmanager")
        genotype <- read.table(genotypeFile$datapath,
                             header = TRUE,
                             sep = sep,
                             stringsAsFactors = FALSE,
                             na.strings = c("", "NA"),
                             check.names = FALSE)
        flog.debug(paste0("genotype$name: ", genotype$name,
                          "; contents rows: ", nrow(genotype),
                          ", columns: ", ncol(genotype), "; col names: '",
                          paste(names(genotype), collapse = "', '"), "'",
                          sep = ""),
                   name = "nprcmanager")
        genotype <- tryCatch(checkGenotypeFile(genotype),
                             warning = function(cond) {
                               return(NULL)
                             },
                             error = function(cond) {
                               return(NULL)
                             },
                             finally = {
                               flog.debug(paste0("   tryCatch checkGenotype ",
                                                 "file. ", geterrmessage()),
                                          name = "nprcmanager")
                             }
        )
        breederPed <- addGenotype(breederPed, genotype)
        flog.debug(paste0("After addGenotype - genotypeFile$name: ",
                          genotypeFile$name,
                          "; contents rows: ", nrow(breederPed),
                          ", columns: ", ncol(breederPed), "; col names: '",
                          paste(names(breederPed), collapse = "', '"), "'",
                          sep = ""),
                   name = "nprcmanager")
      } else {
        flog.debug(paste0("Setting genotype to NULL."),
                   name = "nprcmanager")
        genotype <- NULL
      }
      flog.debug(paste0("Data files may have been read.\n",
                        "contents rows: ", nrow(breederPed),
                        ", columns: ", ncol(breederPed), "; col names: '",
                        paste(names(breederPed), collapse = "', '"), "'",
                        sep = ""),
                 name = "nprcmanager")

      if (!is.null(minParentAge)) {
        flog.debug(paste0("Before qcStudbook.\n",
                          "contents rows: ", nrow(breederPed),
                          ", columns: ", ncol(breederPed), "; col names: '",
                          paste(names(breederPed), collapse = "', '"), "'",
                          sep = ""),
                   name = "nprcmanager")
        if (is.null(errorLst)) {
          errorLst <- tryCatch(
            qcStudbook(breederPed, minParentAge, reportChanges = FALSE, reportErrors = TRUE),
            warning = function(cond) {return(NULL)},
            error = function(cond) {return(NULL)})
        }
        removeTab(inputId = "tab_pages", target = "Changed Columns")
        removeTab(inputId = "tab_pages", target = "Error List")
        if (checkErrorLst(errorLst)) {
          insertTab(inputId = "tab_pages",
                    getErrorTab(errorLst, pedigreeFile$name), target = "Input",
                    position = "before", select = TRUE)
          breederPed <- NULL
        } else {
          if (checkChangedColsLst(errorLst$changedCols)) {
            insertTab(inputId = "tab_pages",
                      getChangedColsTab(errorLst, pedigreeFile$name), target = "Input",
                      position = "before", select = FALSE)
          }
          breederPed <- tryCatch(qcStudbook(breederPed, minParentAge),
                                 warning = function(cond) {return(NULL)},
                                 error = function(cond) {return(NULL)})
          flog.debug(paste0("After qcStudbook.\n",
                            "contents rows: ", nrow(breederPed),
                            ", columns: ", ncol(breederPed), "; col names: '",
                            paste(names(breederPed), collapse = "', '"), "'",
                            sep = ""),
                     name = "nprcmanager")
        }
      }
      flog.debug(paste0("before validate()."),
                 name = "nprcmanager")
      validate(need(!is.null(minParentAge),
                    paste0("   Error uploading data. ",
                           geterrmessage())) %then%
                 need(!is.null(breederPed), paste0("   Error uploading data. ",
                                          geterrmessage()))
      )
      if (!is.null(breederPed)) {
        updateTabsetPanel(session, "tab_pages", selected = "Pedigree Browser")
      }
      flog.debug(paste0("After validate(); nrow(breederPed) = ",
                        nrow(breederPed), "; ncol(breederPed): ",
                        ncol(breederPed)), name = "nprcmanager")
      breederPed
    })
  })

  # Load and QA-QC the pedigree once a file has been specified

  getPed <- reactive({
    flog.debug(paste0("In ped <- reactive()\n"), name = "nprcmanager")
    if (is.null(getSelectedBreeders())) {
      return(NULL)
    }
    flog.debug(paste0("In ped <- reactive() and ",
                      "!is.null(getSelectedBreeders()) == TRUE\n"),
        name = "nprcmanager")

    ped <- getSelectedBreeders()
    flog.debug(paste0("column names: '", paste(names(ped), collapse = "', '"),
                      "'"), name = "nprcmanager")
    flog.debug(" - after ped <- getSelectedBreeders() before tryCatch with ",
               "resetPopulation.", name = "nprcmanager")
    ped <- tryCatch({
        ped <- getSelectedBreeders()
        flog.debug(paste0("column names: '", paste(names(ped),
                                                   collapse = "', '"),
                          "'"), name = "nprcmanager")
        flog.debug(" - in tryCatch before resetPopulation.",
            name = "nprcmanager")
        ## resetPopulation adds the population column if not already present
        ## resetPopulation indicates all id to be in the population if
        ##  specifyFocalAnimals() is NULL
        ## otherwise ids returned by specifyFocalAnimals() are set to TRUE and
        ##  others become FALSE
        ped <- resetPopulation(ped, specifyFocalAnimals())
        flog.debug(paste0("column names: '", paste0(names(ped),
                                                    collapse = "', '"),
                          "'"), name = "nprcmanager")
        flog.debug(paste0("resetPopulation() called\n"),
            name = "nprcmanager")

        if (input$trim) {
          probands <- ped$id[ped$population]
          ped <- trimPedigree(probands, ped, removeUninformative = FALSE,
                            addBackParents = FALSE)
          #ped <- trimPedigree(probands, ped, removeUninformative = TRUE,
          #                  addBackParents = TRUE)
          flog.debug(paste0("trimPedigree() called\n"),
              name = "nprcmanager")
        }

        ped["pedNum"] <- findPedigreeNumber(ped$id, ped$sire, ped$dam)
        ped["gen"] <- findGeneration(ped$id, ped$sire, ped$dam)

        ped
      },
      error = function(cond) {
        return(FALSE)
      })

    validate(
      need(ped, geterrmessage())
    )

    return(ped)
  })

  # Changing the active tab to the "Pedigree Browser" tab
  observe({
    status <- getSelectedBreeders()
    if (!is.null(status))
      updateTabsetPanel(session, "tab_pages", selected = "Pedigree Browser")
  })

  # Creating the pedigree table to be displayed on the Pedigree Browser tab
  output$pedigree <- DT::renderDataTable(DT::datatable({
    if (is.null(getPed())) {
      return(NULL)
    }

    # convert columns to "character" so xtables displays them properly
    ped <- toCharacter(getPed())

    if (!input$uid) {
      ped <- ped[!grepl("^U", ped$id, ignore.case = TRUE), ]
      ped$sire[grepl("^U", ped$sire, ignore.case = TRUE)] <- NA
      ped$dam[grepl("^U", ped$dam, ignore.case = TRUE)] <- NA
    }

    names(ped) <- headerDisplayNames(names(ped))

    ped
  }#, options = list(
   #    initComplete = I("function(settings, json) {$('th:eq(1)').each( function(){this.setAttribute( 'title', 'TEST' );});$('th').tooltip();}")
  #)
  )
  )

  specifyFocalAnimals <- eventReactive(input$specifyFocalAnimal, {
    ped <- unlist(strsplit(input$focalAnimalIds, "[ ,;\t\n]"))
    if (!is.null(input$focalAnimalUpdate)) {
      focalAnimalUpdate <- input$focalAnimalUpdate
      flog.debug(paste0("focalAnimalUpdate - focalAnimalUpdate$name: ",
                        focalAnimalUpdate$name, "; ",
                        "focalAnimalUpdate$datapath: ", focalAnimalUpdate$datapath),
                 name = "nprcmanager")
      focalAnimalUpdateDf <- unlist(read.table(focalAnimalUpdate$datapath,
                                                  header = TRUE,
                                                  sep = ",",
                                                  stringsAsFactors = FALSE,
                                                  na.strings = c("", "NA"),
                                                  check.names = FALSE))
      flog.debug(paste0("focalAnimalUpdate - focalAnimalUpdateDf: ",
                        focalAnimalUpdateDf),
                 name = "nprcmanager")
      updateTextAreaInput(session, "focalAnimalIds",
                          label = paste0(focalAnimalUpdateDf),
                          value = paste0(focalAnimalUpdateDf))
      ped <- focalAnimalUpdateDf
    }
    if (length(ped) == 0) {
      return(NULL)
    } else{
      return(ped)
    }
  },
  ignoreNULL = FALSE)

  # Download handler to download the full or trimmed pedigree
  output$downloadPedigree <- downloadHandler(
    filename = function() {
      paste("Pedigree", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(getPed(), file, na = "", row.names = FALSE)
    }
  )
  #############################################################################
  # Functions for handling the genetic value analysis generation

  geneticValue <- eventReactive(input$analysis, {
    if (is.null(getPed())) {
      return(NULL)
    }
    # Ensuring the pedigree has been trimmed
    # (if there are too many animals, the program will crash)
    ped <- getPed()
    probands <- ped$id[ped$population]
    ped <- trimPedigree(probands, ped, removeUninformative = FALSE,
                      addBackParents = FALSE)

    validate(
      need(length(probands) != 0, "Error: No population specified")
    )

    # Setting up the progress bar
    progress <- shiny::Progress$new()
    on.exit(progress$close())

    updateProgress <- function(n = 1, detail = NULL, value = 0, reset = FALSE) {
      if (reset) {
        progress$set(detail = detail, value = value)
      } else{
        progress$inc(amount = 1 / n)
      }
    }
    #
    return(reportGV(ped, gu.iter = input$iterations,
                    gu.thresh = as.integer(input$threshold),
                    byID = TRUE,
                    updateProgress = updateProgress))
  })
  # Returns the geneticValue() report
  rpt <- reactive({
    if (is.null(geneticValue())) {
      return(NULL)
    }
    return(geneticValue()[["report"]])
  })

  # Functions for displaying the Genetic Value Analysis
  gvaView <- reactive({
    if (is.null(rpt())) {
      return(NULL)
    }
    if (input$view == 0) {
      return(rpt())
    } else{
      ids <- unlist(strsplit(isolate(input$viewIds), "[ ,;\t\n]"))
      if (length(ids) == 0) {
        return(rpt())
      } else{
        return(filterReport(ids, rpt()))
      }
    }
  })

  output$gva <- DT::renderDataTable(DT::datatable({
    if (is.null(rpt())) {
      return(NULL)
    }

    g <- gvaView()
    g$indivMeanKin <- round(g$indivMeanKin, 5)
    g$zScores <- round(g$zScores, 2)
    g$gu <- round(g$gu, 5)
    g <- toCharacter(g)
    names(g) <- headerDisplayNames(names(g))

    return(g)
  }))

  # Download handlers for all or a subset of the Genetic Value Analysis
  output$downloadGVAFull <- downloadHandler(
    filename = function() {
      paste("GVA_full", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(rpt(), file, na = "", row.names = FALSE)
    }
  )

  output$downloadGVASubset <- downloadHandler(
    filename = function() {
      paste("GVA_subset", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(gvaView(), file, na = "", row.names = FALSE)
    }
  )

  #############################################################################
  # Functions for handling printing summary statistics and outputting the
  # kinship matrix

  kmat <- reactive({
    if (is.null(geneticValue())) {
      return(NULL)
    }
    return(geneticValue()[["kinship"]])
  })

  # Download handler for the kinship matrix
  output$downloadKinship <- downloadHandler(
    filename = function() {
      paste("Kinship", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(kmat(), file, na = "")
    }
  )

  output$summaryStats <- renderText({
    if (is.null(geneticValue())) {
      return(NULL)
    }

    f <- geneticValue()[["total"]]
    mf <- geneticValue()[["maleFounders"]]
    ff <- geneticValue()[["femaleFounders"]]
    fe <- geneticValue()[["fe"]]
    fg <- geneticValue()[["fg"]]

    mk <- summary(rpt()[, "indivMeanKin"])
    gu <- summary(rpt()[, "gu"])
    fe_title_txt <- JS(paste("Founder equivalents estimates the expected number
	of equally contributing founders that would be
	required to produce the observed genetic diversity
	in the current population. $f_e = 1 / \\sigma;(p_{i_{2}})$"))
#  </MATH>Where <MATH>p<sub>i</sub></MATH>is the proportion of the genes of the living,
#	descendant population contributed by founder <MATH>i</MATH>."))
    founder <- htmltools::withTags(table(
      class = "display",
      thead(
        tr(
          th("Known Founders"),
          th("Known Female Founders"),
          th("Known Male Founders"),
          th(JS("Founder Equivalents")),
          th("Founder Genome Equivalents"))),
      tbody(td(as.character(f)), td(as.character(ff)), td(as.character(mf)),
            td(as.character(round(fe, digits = 2))),
            td(as.character(round(fg, digits = 2))))))

      header <- paste("<tr>",
                    "<th></th>",
                    "<th>Min</th>",
                    "<th>1st Quartile</th>",
                    "<th>Mean</th>",
                    "<th>Median</th>",
                    "<th>3rd Quartile</th>",
                    "<th>Max</th>",
                    "</tr>")

    k <- paste("<tr>",
               "<td>Mean Kinship</td>",
               "<td>", as.character(round(mk["Min."], 4)), "</td>",
               "<td>", as.character(round(mk["1st Qu."], 4)), "</td>",
               "<td>", as.character(round(mk["Mean"], 4)), "</td>",
               "<td>", as.character(round(mk["Median"], 4)), "</td>",
               "<td>", as.character(round(mk["3rd Qu."], 4)), "</td>",
               "<td>", as.character(round(mk["Max."], 4)), "</td>",
               "</tr>")

    g <- paste("<tr>",
               "<td>Genome Uniqueness</td>",
               "<td>", as.character(round(gu["Min."], 4)), "</td>",
               "<td>", as.character(round(gu["1st Qu."], 4)), "</td>",
               "<td>", as.character(round(gu["Mean"], 4)), "</td>",
               "<td>", as.character(round(gu["Median"], 4)), "</td>",
               "<td>", as.character(round(gu["3rd Qu."], 4)), "</td>",
               "<td>", as.character(round(gu["Max."], 4)), "</td>",
               "</tr>")

    return(paste(founder, "<br>", "<br>", "<table>", header, k, g, "</table>"))
  })

  output$mkPlot <- renderPlot({
    if (is.null(rpt())) {
      return(NULL)
    }
    mk <- rpt()[, "indivMeanKin"]
    avg <- mean(mk, na.rm = TRUE)
    # std.dev <- sd(mk, na.rm = TRUE)
    # upper <- avg + (2 * std.dev)
    # lower <- avg - (2 * std.dev)

    brx <- pretty(range(mk), 25)
    ggplot(data.frame(mk = mk), aes(x = mk, y=..density..)) +
      geom_histogram(bins = 25, color="darkblue", fill="lightblue",
                     breaks = brx) +
      theme_classic() +
      xlab("Kinship") + ylab("Frequency") +
      ggtitle("Distribution of Individual Mean Kinship Coefficients") +
      geom_vline(aes(xintercept = avg, color = "red"), linetype = "dashed",
                 show.legend = FALSE)# +
  })

  output$zscorePlot <- renderPlot({
    if (is.null(rpt())) {
      return(NULL)
    }
    z <- rpt()[, "zScores"]
    avg <- mean(z, na.rm = TRUE)
    # std.dev <- sd(z, na.rm = TRUE)
    # upper <- avg + (2 * std.dev)
    # lower <- avg - (2 * std.dev)

    brx <- pretty(range(z), 25)
    ggplot(data.frame(z = z), aes(x = z, y=..density..)) +
      geom_histogram(bins = 25, color="darkblue", fill="lightblue", breaks = brx) +
      theme_classic() +
      xlab("Z-Score") + ylab("Frequency") +
      ggtitle("Distribution of Mean Kinship Coefficients Z-scores") +
      geom_vline(aes(xintercept = avg, color = "red"), linetype = "dashed",
                 show.legend = FALSE)# +
  })
  output$guPlot <- renderPlot({
    if (is.null(rpt())) {
      return(NULL)
    }
    gu <- rpt()[, "gu"]
    avg <- mean(gu, na.rm = TRUE)
    # std.dev <- sd(gu, na.rm = TRUE)
    # upper <- avg + (2 * std.dev)
    # lower <- avg - (2 * std.dev)

    brx <- pretty(range(gu), 25)
    ggplot(data.frame(gu = gu), aes(x = gu, y=..density..)) +
      geom_histogram(color="darkblue", fill="lightblue", breaks = brx) +
      theme_classic() +
      xlab("Genome Uniqueness Score") + ylab("Frequency") +
      ggtitle("Genome Uniqueness") +
      geom_vline(aes(xintercept = avg, color = "red"), linetype = "dashed",
                 show.legend = FALSE)# +
  })
  output$mkBox <- renderPlot({
    if (is.null(rpt())) {
      return(NULL)
    }
    gu <- rpt()[, "indivMeanKin"]
    ggplot(data.frame(gu = gu), aes(x = "", y = gu)) +
      geom_boxplot(color="darkblue", fill="lightblue", notch = FALSE,
                   outlier.color = "red", outlier.shape = 1) +
      theme_classic() + geom_jitter(width = 0.2) + coord_flip() +
      ylab("Kinship")  + ggtitle("Distribution of Individual Mean Kinship Coefficients")
  })
  output$zscoreBox <- renderPlot({
    if (is.null(rpt())) {
      return(NULL)
    }
    gu <- rpt()[, "zScores"]
    ggplot(data.frame(gu = gu), aes(x = "", y = gu)) +
      geom_boxplot(color="darkblue", fill="lightblue", notch = FALSE,
                   outlier.color = "red", outlier.shape = 1) +
      theme_classic() + geom_jitter(width = 0.2) + coord_flip() +
      ylab("Z-Score")  + ggtitle("Z-Score")
  })
  output$guBox <- renderPlot({
    if (is.null(rpt())) {
      return(NULL)
    }
    gu <- rpt()[, "gu"]
    ggplot(data.frame(gu = gu), aes(x = "", y = gu)) +
      geom_boxplot(color="darkblue", fill="lightblue", notch = FALSE,
                   outlier.color = "red", outlier.shape = 1) +
      theme_classic() + geom_jitter(width = 0.2) + coord_flip() +
      ylab("Score")  + ggtitle("Genetic Uniqueness")
  })
  box_and_whisker_desc <- paste0("The upper whisker extends from the hinge to
                              the largest value no further than 1.5 * IQR
                              from the hinge (where IQR is the
                              inter-quartile range, or distance between
                              the first and third quartiles). The lower
                              whisker extends from the hinge to the
                              smallest value at most 1.5 * IQR of the
                              hinge. Data beyond the end of the whiskers
                              are called \"outlying\" points and are plotted
                              individually.")
  addPopover(session, "mkBox", "Mean Kinship Coefficients",
             content = box_and_whisker_desc,
             placement = "bottom", trigger = "hover", options = NULL)
  addPopover(session, "zscoreBox", "Z-scores",
             content = box_and_whisker_desc,
             placement = "bottom", trigger = "hover", options = NULL)
  addPopover(session, "guBox", "Genetic Uniqueness",
             content = box_and_whisker_desc,
             placement = "bottom", trigger = "hover", options = NULL)

  output$relations <- eventReactive(input$relations, {
    renderTable({
      if (is.null(kmat())) {
        return(NULL)
      }
      j <- nrow(kmat()) * ncol(kmat())
      # Setting up the progress bar
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Finding Relationship Designations", value = 0)
      updateProgress <- function() {
        progress$inc(amount = 1 / j)
      }

      kin <- convertRelationships(kmat(), getPed(),
                                  updateProgress = updateProgress)

      progress$set(message = "Preparing Table", value = 1)
      r <- makeRelationClasseTable(kin)

      toCharacter(r)
    },
    rownames = FALSE
    )
  })

  # Download handler for the first-order relationships
  output$downloadFirstOrder <- downloadHandler(
    filename = function() {
      paste("FirstOrder", ".csv", sep = "")
    },
    content = function(file) {
      ped <- getPed()
      r <- countFirstOrder(ped, ids = ped$id[ped$population])
      write.csv(r, file, na = "")
    }
  )

  output$downloadRelations <- downloadHandler(
    filename = function() {
      paste("Relations", ".csv", sep = "")
    },
    content = function(file) {
      ped <- getPed()
      probands <- ped$id[ped$population]

      r <- convertRelationships(kmat(), getPed(), ids = probands)
      write.csv()
    }
  )

  #############################################################################
  # Functions for handling the breeding group formation process

  bg <- eventReactive(input$grpSim, {
    if (is.null(rpt())) {
      return(NULL)
    }
    output$textAreas <- renderUI({
      numGp <- input$numGp # default 1
      lapply(1:numGp, function(i) {
        textAreaInput(inputId = paste0("curGrp", i), rows = 5, cols = 40)
      })
    })
    browser()
    currentGroups <- reactive({
      numGp <- as.integer(input$numGp)
      currentGroups <- sapply(1:numGp, function(i) {
        unlist(strsplit(input[[paste0("curGrp", i)]], "[ \t\n]"))
      })
      currentGroups
    })
    ped <- getPed()
    # Filter out unknown animals added into ped
    ped <- removeUnknownAnimals(ped)
    ids <- character(0)
    if (input$group_formation_rb == "candidates")
      ids <- unlist(strsplit(input$grpIds, "[ \t\n]"))
    # currentGroups[[1]] <- unlist(strsplit(input$curGrp1, "[ \t\n]"))
    # currentGroups[[2]] <- unlist(strsplit(input$curGrp2, "[ \t\n]"))
    # currentGroups[[3]] <- unlist(strsplit(input$curGrp3, "[ \t\n]"))
    # currentGroups[[4]] <- unlist(strsplit(input$curGrp4, "[ \t\n]"))
    # currentGroups[[5]] <- unlist(strsplit(input$curGrp5, "[ \t\n]"))
    # currentGroups[[6]] <- unlist(strsplit(input$curGrp6, "[ \t\n]"))
    # currentGroups <- nprcmanager:::compactCurrentGroups(currentGroups)
    if (length(ids) > 0) {
      ped <- resetGroup(ped, ids)
      candidates <- ids
    } else{
      candidates <- getGrpIds()
    }

    # Assume an animal that is in the group can't also be a candidate
    if (length(unlist(currentGroups)) > 0) {
      candidates <- setdiff(candidates, unlist(currentGroups))
    }

    # Filter out low-value animals if desired
    useLv <- input$group_formation_rb != "high-value"
    if (!useLv) {
      rpt <- rpt()
      lv <- rpt$id[rpt$value == "low value"]
      candidates <- setdiff(candidates, lv)
    }
    candidates <- intersect(candidates, ped$id)

    harem <- input$group_sex_rb == "harems"

    validate(
      need(length(candidates == 0), "No candidates defined"),
      need(!(length(setdiff(candidates, ped$id)) > 0),
           paste("Group candidates present that are",
                 "not in the provided pedigree\n",
                 paste(setdiff(candidates, ped$id), sep = "\n"))),
      need(!(length(setdiff(unlist(currentGroups), ped$id)) > 0),
           paste("Current group members present that",
                 "are not in the provided pedigree\n",
                 paste(setdiff(unlist(currentGroups), ped$id), sep = "\n")))
    )
    ignore <- input$ffRel
    ignore <- if (ignore) list(c("F", "F")) else NULL
    threshold <- input$kinThresh
    if (input$useMinParentAge) {
      minAge <- globalMinParentAge
      output$minParentAge <- renderText({paste0(minAge)})
    } else {
      minAge <- input$minAge
    }

    withKin <- input$withKin
    iter <- input$gpIter
    numGp <- ({
      input$numGp
      })
    sexRatio <- input$sexRatio
    # Setting up the progress bar
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Generating Groups", value = 0)

    n <- 1
    updateProgress <- function() {
      progress$inc(amount = 1 / iter, detail = paste("Iteration:", n))
      n <<- n + 1
    }

    grp <- groupAddAssign(candidates = candidates,
                         currentGroups = currentGroups,
                         kmat = kmat(),
                         ped = ped,
                         threshold = threshold,
                         ignore = ignore,
                         minAge = minAge,
                         iter = iter,
                         numGp = numGp,
                         updateProgress = updateProgress,
                         harem = harem, sexRatio,
                         withKin = withKin)

    return(grp)
  })

  getGrpIds <- reactive({
    ped <- getPed()
    if ("group" %in% colnames(ped)) {
      return(ped$id[ped$group])
    } else{
      return(ped$id[ped$population])
    }
  })

  # Functions to handle breeding group display
  observe({
    if (!is.null(bg())) {
      x <- length(bg()$group)

      if (x > 1) {
        gp <- list()
        for (i in 1:(x - 1)) {
          gp[paste("Group", as.character(i))] <- i
        }
        gp["Unused"] <- x

        updateSelectInput(session, "viewGrp",
                          label = "Enter the group to view:",
                          choices = gp, selected = 1)
      } else if (x == 1) {
        updateSelectInput(session, "viewGrp",
                          label = "Enter the group to view:",
                          choices = list("Unused" = 1), selected = 1)
      } else{
        updateSelectInput(session, "viewGrp",
                          label = "Enter the group to view:",
                          choices = list(" " = 1), selected = 1)
      }
    }
  })

  bgGroupView <- reactive({
    if (is.null(bg())) {
      return(NULL)
    }
    i <- as.numeric(input$viewGrp)
    gp <- bg()$group[[i]]
    ped <- getPed()
    gp <- addSexAndAgeToGroup(gp, ped)
    gp$age <- round(gp$age, 1)
    colnames(gp) <- c("Ego ID", "Sex", "Age in Years")

    if (nrow(gp) == 0) {
      return(NULL)
    } else {
      return(gp[order(gp$`Ego ID`), , drop = FALSE])
    }
  })
  bgGroupKinView <- reactive({
    if (is.null(bg()$groupKin)) {
      return(NULL)
    }
    i <- as.numeric(input$viewGrp)
    kmat <- bg()$groupKin[[i]]
    kmat <- as.data.frame(round(kmat, 6))

    if (nrow(kmat) == 0) {
      return(NULL)
    } else{
      return(kmat)
    }
  })

  output$breedingGroups <- DT::renderDataTable(DT::datatable({
    if (is.null(bg())) {
      return(NULL)
    }
    return(bgGroupView())
  }))
  output$breedingGroupKin <- DT::renderDataTable(DT::datatable({
    if (is.null(bg()$groupKin)) {
      return(NULL)
    }
    return(bgGroupKinView())
  }))

  # Download handler for the current group
  output$downloadGroup <- downloadHandler(
    filename = getDatedFilename(paste0("Group-", input$viewGrp,
                                      ".csv", sep = "")),
    content = function(file) {
      write.csv(bgGroupView(), file, na = "", row.names = FALSE)
    },
    contentType = "text/csv"
  )

  output$downloadGroupKin <- downloadHandler(
    filename = getDatedFilename(paste0("GroupKin-", input$viewGrp,
                                         ".csv", sep = "")),
    content = function(file) {
      write.csv(bgGroupKinView(), file, na = "", row.names = TRUE)
    },
    contentType = "text/csv"
  )

  #############################################################################
  # Function to handle display of pyramid plot
  flog.debug("before renderPlot(getPyramidPlot(ped)))", name = "nprcmanager")
  output$pyramidPlot <- renderPlot(getPyramidPlot(getPed()))
  flog.debug("after renderPlot(getPyramidPlot(ped)))", name = "nprcmanager")
})
