# source("../../R/PedigreeCuration.R")
# source("../../R/GeneticValueAnalysis.R")
# source("../../R/GroupFormation.R")
# source("../../R/Relations.R")

shinyServer(function(input, output, session) {
  #############################################################################
  # Functions for handling initial pedigree upload and QC

  # Opening the file browser when the button is clicked
  # selectFile <- eventReactive(input$select_file, {
  #   return(
  #         tryCatch(file.choose(), error = function(e) {NULL})
  #       )
  # })

  # Load/QC the pedigree once a file has been specified
  sb <- reactive({
    # inFile will be NULL initially.
    # After the user selects a file, it will be a filepath.
    inFile <- input$select_file
    if (is.null(inFile)) {
      return(NULL)
    }

    # Load and QC the data table
    d <- read.csv(inFile$datapath,
                  header = TRUE,
                  sep = input$sep,
                  stringsAsFactors = FALSE,
                  na.strings = c("", "NA"),
                  check.names = FALSE)

    d <- tryCatch(qc.Studbook(d),
                  warning = function(cond) {
                    return(NULL)
                  },
                  error = function(cond) {
                    return(NULL)
                  }
                  )

    validate(
      need(!is.null(d), "Error uploading data.")
    )

    d
  })

  ped <- reactive({
    if (is.null(sb())) {
      return(NULL)
    }
    p <- sb()

    p <- tryCatch(
      {
        p <- resetPopulation(specifyPopulation(), p)

        if (input$trim) {
          probands <- p$id[p$population]
          p <- trimPedigree(probands, p)
          #p <- trimPedigree2(probands, p)
        }

        p["ped.num"] <- findPedigreeNumber(p$id, p$sire, p$dam)
        p["gen"] <- findGeneration(p$id, p$sire, p$dam)

        p
      },
      error = function(cond) {
        return(FALSE)
      })

    validate(
      need(p, geterrmessage())
    )

    return(p)
  })

  # Changing the active tab to the "Pedigree Browser" tab
  observe({
    if (!is.null(input$select_file)) {
      updateTabsetPanel(session, "tab_pages", selected = "Pedigree Browser")
    }
  })

  # Creating the pedigree table to be displayed on the Pedigree Browser tab
  output$pedigree <- DT::renderDataTable(DT::datatable({
    if (is.null(ped())) {
      return(NULL)
    }

    # convert columns to "character" so xtables displays them properly
    p <- toCharacter(ped())

    if (!input$uid) {
      p <- p[!grepl("^U", p$id, ignore.case = TRUE), ]
      p$sire[grepl("^U", p$sire, ignore.case = TRUE)] <- NA
      p$dam[grepl("^U", p$dam, ignore.case = TRUE)] <- NA
    }

    names(p) <- headerDisplayNames(names(p))

    p
  }), filter = "top")

  specifyPopulation <- eventReactive(input$specify_pop, {
    p <- unlist(strsplit(input$population_ids, "[ \t\n]"))

    if (length(p) == 0) {
      return(NULL)
    } else{
      return(p)
    }
  }, ignoreNULL = FALSE)

  # Download handler to download the full or trimmed pedigree
  output$downloadPedigree <- downloadHandler(
    filename = function() {paste("Pedigree", ".csv", sep = "")},
    content = function(file) {
      write.csv(ped(), file, na = "", row.names = FALSE)
    }
  )
  #############################################################################
  # Functions for handling the genetic value analysis generation

  genetic_value <- eventReactive(input$analysis, {
    if (is.null(ped())) {
      return(NULL)
    }
    # Ensuring the pedigree has been trimmed
    # (if there are too many animals, the program will crash)
    p <- ped()
    probands <- p$id[p$population]
    p <- trimPedigree(probands, p)

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
        progress$inc(amount = 1/n)
      }
    }
    #
    return(reportGV(p, gu.iter = input$iterations,
                    gu.thresh = as.integer(input$threshold),
                    by.id = TRUE,
                    updateProgress = updateProgress))
  })

  rpt <- reactive({
    if (is.null(genetic_value())) {
      return(NULL)
    }
    return(genetic_value()[["report"]])
  })

  # Functions for displaying the Genetic Value Analysis
  gva_view <- reactive({
    if (is.null(rpt())) {
      return(NULL)
    }
    if (input$view == 0) {
      return(rpt())
    } else{
      ids <- unlist(strsplit(isolate(input$view_ids), "[ \t\n]"))
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

    g <- gva_view()
    g$indiv.avgs <- round(g$indiv.avgs, 5)
    g$z.scores <- round(g$z.scores, 2)
    g <- toCharacter(g)
    names(g) <- headerDisplayNames(names(g))

    return(g)
  }), filter = "top")

  # output$gva <- renderText({
  #   if (is.null(rpt())) {
  #     return(NULL)
  #   }
  #
  #   g <- gva_view()
  #   g$indiv.avgs <- round(g$indiv.avgs, 5)
  #   g$z.scores <- round(g$z.scores, 2)
  #   g <- toCharacter(g)
  #   names(g) <- headerDisplayNames(names(g))
  #
  #   html <- "<table>"
  #   html <- paste(html, makeHtmlRow(names(g), header = TRUE))
  #
  #   highlight <- which(gva_view()[,"value"] == "low value")
  #
  #   for(i in 1:nrow(g)) {
  #     r <- g[i, ]
  #     if (i %in% highlight) {
  #       html <- paste(html, makeHtmlRow(r, color = "#FFD280"))
  #     } else{
  #       html <- paste(html, makeHtmlRow(r))
  #     }
  #   }
  #
  #   html <- paste(html, "</table>")
  #
  #   return(html)
  # })

  # makeHtmlRow <- function(r, header = FALSE, color = NULL) {
  #   if (is.null(color)) {
  #     html <- "<tr>"
  #   } else{
  #     html <- paste('<tr bgcolor = "', color, '">', sep = "")
  #   }
  #
  #   for(i in 1:length(r)) {
  #     d <- r[i]
  #     if (is.na(d)) {
  #       d <- ""
  #     }
  #
  #     if (header) {
  #       html <- paste(html, "<th>", as.character(d), "</th>")
  #     } else{
  #       html <- paste(html, "<td>", as.character(d), "</td>")
  #     }
  #   }
  #   html <- paste(html, "</tr>")
  #   return(html)
  # }

  # Download handlers for all or a subset of the Genetic Value Analysis
  output$downloadGVA_full <- downloadHandler(
    filename = function() {paste("GVA_full", ".csv", sep = "")},
    content = function(file) {
      write.csv(rpt(), file, na = "", row.names = FALSE)
    }
  )

  output$downloadGVA_subset <- downloadHandler(
    filename = function() {paste("GVA_subset", ".csv", sep = "")},
    content = function(file) {
      write.csv(gva_view(), file, na = "", row.names = FALSE)
    }
  )

  #############################################################################
  # Functions for handling printing summary statistics and outputting the
  # kinship matrix

  kmat <- reactive({
    if (is.null(genetic_value())) {
      return(NULL)
    }
    return(genetic_value()[["kinship"]])
  })

  # Download handler for the kinship matrix
  output$downloadKinship <- downloadHandler(
    filename = function() {paste("Kinship", ".csv", sep = "")},
    content = function(file) {
      write.csv(kmat(), file, na = "")
    }
  )

  output$summary_stats <- renderText({
    if (is.null(genetic_value())) {
      return(NULL)
    }

    f <- genetic_value()[["total"]]
    mf <- genetic_value()[["male.founders"]]
    ff <- genetic_value()[["female.founders"]]
    fe <- genetic_value()[["fe"]]
    fg <- genetic_value()[["fg"]]

    mk <- summary(rpt()[, "indiv.avgs"])
    gu <- summary(rpt()[, "gu"])

    founder <- paste("Known Founders: ", as.character(f),
                     "<br>",
                     "Known Female Founders: ", as.character(ff),
                     "<br>",
                     "Known Male Founders: ", as.character(mf),
                     "<br>",
                     "Founder Equivalents: ",
                     as.character(round(fe, digits = 2)),
                     "<br>",
                     "Founder Genome Equivalents: ",
                     as.character(round(fg, digits = 2))
    )

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
               "<td>", as.character(mk["Min."]), "</td>",
               "<td>", as.character(mk["1st Qu."]), "</td>",
               "<td>", as.character(mk["Mean"]), "</td>",
               "<td>", as.character(mk["Median"]), "</td>",
               "<td>", as.character(mk["3rd Qu."]), "</td>",
               "<td>", as.character(mk["Max."]), "</td>",
               "</tr>")

    g <- paste("<tr>",
               "<td>Genome Uniqueness</td>",
               "<td>", as.character(gu["Min."]), "</td>",
               "<td>", as.character(gu["1st Qu."]), "</td>",
               "<td>", as.character(gu["Mean"]), "</td>",
               "<td>", as.character(gu["Median"]), "</td>",
               "<td>", as.character(gu["3rd Qu."]), "</td>",
               "<td>", as.character(gu["Max."]), "</td>",
               "</tr>")

    return(paste(founder, "<br>", "<br>", "<table>", header, k, g, "</table>"))
  })

  output$mk_plot <- renderPlot({
    if (is.null(rpt())) {
      return(NULL)
    }
    mk <- rpt()[, "indiv.avgs"]
    avg <- mean(mk, na.rm = TRUE)
    std.dev <- sd(mk, na.rm = TRUE)

    u <- avg + (2 * std.dev)
    l <- avg - (2 * std.dev)

    hist(mk,
         breaks = 25,
         main = "Individual Mean Kinships",
         xlab = "Kinship",
         ylab = "Frequency")
    abline(v = avg,col = "red",lty = "dashed", lwd = 2)
  })

  output$zscore_plot <- renderPlot({
    if (is.null(rpt())) {
      return(NULL)
    }
    z <- rpt()[, "z.scores"]
    avg <- mean(z, na.rm = TRUE)
    std.dev <- sd(z, na.rm = TRUE)

    u <- avg + (2 * std.dev)
    l <- avg - (2 * std.dev)

    hist(z,
         breaks = 25,
         main = "Individual Mean Kinship Z-Scores",
         xlab = "Z-Score",
         ylab = "Frequency")
    abline(v = avg,col = "red",lty = "dashed", lwd = 2)
  })

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
        progress$inc(amount = 1/j)
      }

      kin <- convertRelationships(kmat(), ped(), updateProgress = updateProgress)

      progress$set(message = "Preparing Table", value = 1)
      r <- relationClasses(kin)

      toCharacter(r)
    },
    include.rownames = FALSE
    )
  })

  # Download handler for the first-order relationships
  output$downloadFirstOrder <- downloadHandler(
    filename = function() {paste("FirstOrder", ".csv", sep = "")},
    content = function(file) {
      p <- ped()
      r <- countFirstOrder(p, ids = p$id[p$population])
      write.csv(r, file, na = "")
    }
  )

  output$downloadRelations <- downloadHandler(
    filename = function() {paste("Relations", ".csv", sep = "")},
    content = function(file) {
      p <- ped()
      probands <- p$id[p$population]

      r <- convertRelationships(kmat(), ped(), ids = probands)
      write.csv()
    }
  )

  #############################################################################
  # Functions for handling the breeding group formation process

  bg <- eventReactive(input$grp_sim, {
    if (is.null(rpt())) {
      return(NULL)
    }

    p <- ped()
    ids <- unlist(strsplit(input$grp_ids, "[ \t\n]"))
    current.group <- unlist(strsplit(input$cur_grp, "[ \t\n]"))

    if (length(ids) > 0) {
      p <- resetGroup(ids, p)
      candidates <- ids
    } else{
      candidates <- getGrpIds()
    }

    # Assume an animal that is in the group can't also be a candidate
    if (length(current.group) > 0) {
      candidates <- setdiff(candidates, current.group)
    }

    # Filter out low-value animals if desired
    use.lv <- input$low_val
    if (!use.lv) {
      rpt <- rpt()
      lv <- rpt$id[rpt$value == "low value"]
      candidates <- setdiff(candidates, lv)
    }

    validate(
      need(length(candidates == 0), "No candidates defined"),
      need(!(length(setdiff(candidates, p$id)) > 0),
           paste("Group candidates present that are",
                 "not in the provided pedigree\n",
                 paste(setdiff(candidates, p$id), sep = "\n"))),
      need(!(length(setdiff(current.group, p$id)) > 0),
           paste("Current group members present that",
                 "are not in the provided pedigree\n",
                 paste(setdiff(current.group, p$id), sep = "\n")))
    )

    ignore <- input$ff_rel
    ignore <- if (ignore) list(c("F", "F")) else NULL

    threshold <- input$kin_thresh
    min.age <- input$min_age

    iter <- input$gp_iter
    numGp <- ({input$numGp})

    # Setting up the progress bar
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Generating Groups", value = 0)

    n <- 1
    updateProgress <- function() {
      progress$inc(amount = 1 / iter, detail = paste("Iteration:", n))
      n <<- n + 1
    }

    if (length(current.group) > 0) {
      grp <- groupAddition(candidates,
                           current.group,
                           kmat(),
                           p,
                           threshold = threshold,
                           ignore = ignore,
                           min.age = min.age,
                           iter = iter,
                           updateProgress = updateProgress)
    } else{
      grp <- groupAssign(candidates, kmat(), p,
                         threshold = threshold,
                         ignore = ignore,
                         min.age = min.age,
                         iter = iter,
                         numGp = numGp,
                         updateProgress = updateProgress)
    }

    return(grp$group)
  })

  getGrpIds <- reactive({
    p <- ped()
    if ("group" %in% colnames(p)) {
      return(p$id[p$group])
    } else{
      return(p$id[p$population])
    }
  })

  # Functions to handle breeding group display
  observe({
    if (!is.null(bg())) {
      x <- length(bg())

      if (x > 1) {
        gp <- list()
        for (i in 1:(x - 1)) {
          gp[paste("Group", as.character(i))] <- i
        }
        gp["Unused"] <- x

        updateSelectInput(session, "view_grp", label = "Enter the group to view:",
                          choices = gp, selected = 1)
      } else if (x == 1) {
        updateSelectInput(session, "view_grp", label = "Enter the group to view:",
                          choices = list("Unused" = 1), selected = 1)
      } else{
        updateSelectInput(session, "view_grp", label = "Enter the group to view:",
                          choices = list(" " = 1), selected = 1)
      }
    }
  })

  bg_view <- reactive({
    if (is.null(bg())) {
      return(NULL)
    }
    i <- as.numeric(input$view_grp)
    gp <- bg()[[i]]
    gp <- as.data.frame(gp)
    colnames(gp) <- "Ego ID"

    if (nrow(gp) == 0) {
      return(NULL)
    } else{
      return(gp[order(gp$`Ego ID`), , drop = FALSE])
    }
  })

  # output$breeding_groups <- renderTable({
  #   if (is.null(bg())) {
  #     return(NULL)
  #   }
  #
  #   return(bg_view())
  # })

  output$breeding_groups <- DT::renderDataTable(DT::datatable({
    if (is.null(bg())) {
      return(NULL)
    }

    return(bg_view())
  }), filter = "top")

  # Download handler for the current group
  output$downloadGroup <- downloadHandler(
    filename = function() {paste("Group-", input$view_grp, ".csv", sep = "")},
    content = function(file) {
      write.csv(bg_view(), file, na = "", row.names = FALSE)
    }
  )
  output$pyramide_plot <- renderPlot({pyramide_plot})
})


###############################################################################
# Functions for proper display of the pedigree on the Pedigree Browser tab

#' Converts all columns of a data.frame to of class "character"
#'
#' NOTE:
#' Function was created to deal with a display bug in xtables (used by Shiny)
#' The bug causes Date columns to be displayed improperly
#' Shiny also displays all Numbers (including integers) with 2 decimal places
#' @param a dataframe
#' @param  df: Any data.frame where the first three columns can be coerced to
#' character.
#' @return A dataframe with the first three columns converted to class
#' "character" for
#'             display with xtables (in shiny)
#' @export
toCharacter <- function(df) {
  #headers <- names(df)
  headers <- c("id", "sire", "dam")

  for (i in 1:length(headers)) { # should be i in seq_along(headers)
    df[[i]] <- as.character(df[[i]])
  }
  return(df)
}
#' Convert internal column names to display or header names.
#'
#' Converts the column names of a Pedigree or Genetic value Report to
#'   something more descriptive
#'
#' @param character vector of length one having a string to be display names or
#' header names.
#'
#' @param key: A column name
#' @return A converted column header
#' @export
nameConversion <- function(key) {
  switch(key,
         id = "Ego ID",
         sire = "Sire ID",
         dam = "Dam ID",
         sex = "Sex",
         gen = "Generation #",
         birth = "Birth Date",
         exit = "Exit Date",
         age = "Age (in years)",
         population = "Breeding Colony Member",
         group = "Subset Member",
         origin = "Origin",
         indiv.avgs = "Individual Mean Kinship",
         z.scores = "Z-score (Mean Kinship)",
         genome.unique = "Genome Uniqueness",
         total.offspring = "Total Offspring",
         living.offspring = "Living Offspring",
         rank = "Rank",
         value = "Value Designation",
         status = "Status",
         ancestry = "Ancestry",
         gu = "Genome Uniqueness (%)",
         ped.num = "Pedigree #",
         spf = "SPF",
         condition = "Condition")
}

#' Convert internal column names to display or header names.
#'
#' Converts the column names of a Pedigree or Genetic value Report to
#' something more descriptive.
#'
#' @param headers a character vector of column (header) names
#'
#' @return Updated list of column names
#' @export
headerDisplayNames <- function(headers) {
  return(unlist(lapply(headers, nameConversion)))
}

#' Filters a genetic value report down to only the specified animals
#'
#' @param ids character vector of animal IDs
#' @param rpt a dataframe with required colnames \code{id}, \code{gu},
#' \code{z.scores}, \code{import}, \code{total.offspring}, which is
#' a data.frame of results from a genetic value analysis.
#' @return A copy of report specific to the specified animals
filterReport <- function(ids, rpt) {
 return(rpt[rpt$id %in% ids,])
}



