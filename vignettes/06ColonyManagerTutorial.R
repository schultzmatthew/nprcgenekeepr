## ----setup, include=FALSE---------------------------------------------------------------
library(png)
library(kableExtra)
library(grid)
library(stringi)
library(nprcgenekeepr)
knitr::opts_chunk$set(eval = FALSE, echo = TRUE, results = "markup", cache = FALSE)
pdf.options(useDingbats = TRUE)
start_time <- proc.time()


## ----gh-installation--------------------------------------------------------------------
#  install.packages("devtools")
#  devtools::install_github("rmsharp/nprcgenekeepr")
#  

## ----start, echo = TRUE, include=TRUE, eval = FALSE-------------------------------------
#  library(nprcgenekeepr)
#  runGeneKeepR()
#  

## ----eopening-screen-top, eval = TRUE, fig.width = 5.5, fig.height = 7, echo = FALSE----
img <- readPNG("./shiny_app_use/opening_screen_top.png")
grid.raster(img)


## ----eopening-screen-middle, eval = TRUE, fig.width = 5.5, fig.height = 7, echo = FALSE----
img <- readPNG("./shiny_app_use/opening_screen_middle.png")
grid.raster(img)


## ----eopening-screen-bottom, eval = TRUE, fig.width = 5.5, fig.height = 7, echo = FALSE----
img <- readPNG("./shiny_app_use/opening_screen_bottom.png")
grid.raster(img)


## ----eexamplePedigreeTutorial, eval = TRUE, fig.width = 5.5, fig.height = 7, echo = FALSE----
img <- readPNG("./shiny_app_use/examplePedigreeTutorial.png")
grid.raster(img)


## ----eexamplePedigreeTutorial-with_alleles, eval = TRUE, fig.width = 5.5, fig.height = 7, echo = FALSE----
img <- readPNG("./shiny_app_use/examplePedigreeTutorial_with_alleles.png")
grid.raster(img)


## ----eopening-screen-top-red-oval, eval = TRUE, fig.width = 5.5, fig.height = 7, echo = FALSE----
img <- readPNG("./shiny_app_use/opening_screen_top_red_oval.png")
grid.raster(img)


## ----make-example-file------------------------------------------------------------------
#  makeExamplePedigreeFile()
#  

## ----example-pedigree, eval = TRUE, fig.width = 2.5, fig.height = 3.5, echo = FALSE-----
img <- readPNG("./shiny_app_use/input_example_pedigree_xlsx.png")
grid.raster(img)


## ----example-pedigree-minParentAgeSequence, eval = TRUE, fig.width = 11, fig.height = 6, echo = FALSE----
img <- readPNG("./shiny_app_use/input_minParentAgeSequence.png")
grid.raster(img)


## ----read-and-check-pedigree, eval = TRUE, fig.width = 2.5, fig.height = 3.5, echo = FALSE----
img <- readPNG("./shiny_app_use/read_and_check_pedigree.png")
grid.raster(img)


## ----make-errorList-definition-tbl, echo = FALSE, eval=TRUE-----------------------------
errorTypes <- names(getEmptyErrorLst())
errorDescriptions <- c(
  "Database connection failed: configuration or permissions are invalid",
  "Columns that must be within the pedigree file are missing.",
  "Values, which are supposed to be dates, cannot be interpreted as a date.",
  "Parents were too young on the date of birth of to have been the parent.",
  "Individuals listed as female or hermaphroditic and as a sire.",
  "Individuals are listed as male and as a dam.", 
  "Individuals who are listed as both a sire and a dam.",
  "IDs listed more than once.",
  stri_c("System Crash. These are unanticipated errors that should be ", 
         "reported to the package maintainer"),
  stri_c("Columns that have been changed to conform to the program (ex: id -> Ego ID).")
)
errorTbl <- data.frame(`Error` = errorTypes, Definition = errorDescriptions, 
                       stringsAsFactors = FALSE) 


## ----print-error-definition-tbl, eval = TRUE, echo=FALSE, include = TRUE, results='markup'----
knitr::kable(errorTbl)  %>%
  kable_styling(full_width = F) %>%
  column_spec(1, bold = T, color = "blue") %>%
  column_spec(2, width = "30em")


## ----pb-10-rows-display-unknown-ids, eval = TRUE, fig.width = 5.5, fig.height = 7, echo = FALSE----
img <- readPNG("./shiny_app_use/pb_10_rows_display_unknown_ids.png")
grid.raster(img)


## ----unknown-displayed, eval = TRUE, fig.width = 5, fig.height = 3.5, echo = FALSE------
img <- readPNG("./shiny_app_use/pb_unknown_displayed.png")
grid.raster(img)


## ----no-unknown-displayed, eval = TRUE, fig.width = 5, fig.height = 3.5, echo = FALSE----
img <- readPNG("./shiny_app_use/pb_no_unknown_displayed.png")
grid.raster(img)


## ----focal-animal-text-box, eval = TRUE, fig.width = 5, fig.height = 3.5, echo = FALSE----
img <- readPNG("./shiny_app_use/pb_focal_animal_text_box.png")
grid.raster(img)


## ----pedigree-browser-5-focal-animals-small, eval = TRUE, fig.width = 8.0, echo = FALSE----
img <- png::readPNG("./shiny_app_use/pb_5_focal_animals_small.png")
grid::grid.raster(img)


## ----selection-large-focal-group, eval = TRUE, fig.width = 4, fig.height = 4, echo = FALSE----
img <- readPNG("./shiny_app_use/pb_selection_large_focal_group.png")
grid.raster(img)


## ----select-trim-for-focal-animals, eval = TRUE, fig.width = 6, fig.height = 4, echo = FALSE----
img <- readPNG("./shiny_app_use/pb_select_trim_for_focal_animals.png")
grid.raster(img)


## ----trimmed-for-focal-animals, eval = TRUE, fig.width = 4, fig.height = 2.5, echo = FALSE----
img <- readPNG("./shiny_app_use/pb_trimmed_for_focal_animals.png")
grid.raster(img)


## ----cleared-of-focal-animals, eval = TRUE, fig.width = 6, echo = FALSE-----------------
img <- png::readPNG("./shiny_app_use/pb_cleared_focal_animals_combined.png")
grid::grid.raster(img)


## ----age-plot, eval = TRUE, fig.width = 5.5, fig.height = 7, echo = FALSE---------------
img <- readPNG("./shiny_app_use/age_plot.png")
grid.raster(img)


## ----gva-calculating, eval = TRUE, fig.width = 7, fig.height = 5.5, echo = FALSE--------
img <- readPNG("./shiny_app_use/gva_calculating.png")
grid.raster(img)


## ----gva-first-high-value, eval = TRUE, fig.width = 6, fig.height = 5.5, echo = FALSE----
img <- readPNG("./shiny_app_use/gva_first_high_value.png")
grid.raster(img)


## ----gva-high-and-low-value, eval = TRUE, fig.width = 5.5, fig.height = 4, echo = FALSE----
img <- readPNG("./shiny_app_use/gva_high_and_low_value.png")
grid.raster(img)


## ----ss-first-view, eval = TRUE, fig.width = 5.5, fig.height = 5.5, echo = FALSE--------
img <- readPNG("./shiny_app_use/ss_first_view.png")
grid.raster(img)


## ----ss-kinship-matrix, eval = TRUE, fig.width = 6, fig.height = 4.5, echo = FALSE------
img <- readPNG("./shiny_app_use/ss_kinship_matrix.png")
grid.raster(img)


## ----first-order-relationships, eval = TRUE, fig.width = 5, fig.height = 5, echo = FALSE----
img <- readPNG("./shiny_app_use/ss_first_order_relationships.png")
grid.raster(img)


## ----female-founders, eval = TRUE, fig.width = 5, fig.height = 5, echo = FALSE----------
img <- readPNG("./shiny_app_use/ss_female_founders.png")
grid.raster(img)


## ----ss-trimmed-all-plots, eval = TRUE, fig.width = 5, fig.height = 5.5, echo = FALSE----
img <- readPNG("./shiny_app_use/ss_trimmed_all_plots.png")
grid.raster(img)


## ----breeding-group-first-view, eval = TRUE, fig.width = 7, fig.height = 5.5, echo = FALSE----
img <- readPNG("./shiny_app_use/breeding_group_first_view.png")
grid.raster(img)


## ----breeding-group-1, eval = TRUE, fig.width = 8, fig.height = 7.5, echo = FALSE-------
img <- readPNG("./shiny_app_use/breeding_group_1.png")
grid.raster(img)


## ----breeding-group-6-infants-with-dam, eval = TRUE, fig.width = 6, fig.height = 5.5, echo = FALSE----
img <- readPNG("./shiny_app_use/breeding_group_6_infants_with_dam.png")
grid.raster(img)


## ----breeding-group-first-group-no-kinship-seeds-indicated, eval = TRUE, fig.width = 6, fig.height = 5.5, echo = FALSE----
img <- readPNG("./shiny_app_use/breeding_group_first_group_no_kinship_seeds_indicated.png")
grid.raster(img)


## ----breeding-group-6-seed-grps-grp-6-kinship, eval = TRUE, fig.width = 5.5, fig.height = 5.5, echo = FALSE----
img <- readPNG("./shiny_app_use/breeding_group_6_seed_grps_grp_6_kinship.png")
grid.raster(img)


## ----breeding-group-sex-ratio-specification, eval = TRUE, fig.width = 5.5, echo = FALSE----
img <- png::readPNG("./shiny_app_use/breeding_group_sex_ratio_specification.png")
grid::grid.raster(img)


