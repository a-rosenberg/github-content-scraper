## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "README-"
)


## ----eval=FALSE----------------------------------------------------------
## # Install package from CRAN:
## install.packages("collapsibleTree")
## 
## # Alternately, install the latest development version from GitHub:
## # install.packages("devtools")
## devtools::install_github("AdeelK93/collapsibleTree")


## ----eval=FALSE----------------------------------------------------------
## library(collapsibleTree)
## 
## collapsibleTree(warpbreaks, c("wool", "tension", "breaks"))


## ----eval=FALSE----------------------------------------------------------
## # Data from US Forest Service DataMart
## species <- read.csv("https://apps.fs.usda.gov/fia/datamart/CSV/REF_SPECIES_GROUP.csv")
## 
## collapsibleTree(
##   species,
##   hierarchy = c("REGION", "CLASS", "NAME"),
##   fill = c(
##     # The root
##     "seashell",
##     # Unique regions
##     rep("brown", length(unique(species$REGION))),
##     # Unique classes per region
##     rep("khaki", length(unique(paste(species$REGION, species$CLASS)))),
##     # Unique names per region
##     rep("forestgreen", length(unique(paste(species$NAME, species$REGION))))
##   )
## )


## ----eval=FALSE----------------------------------------------------------
## collapsibleTreeSummary(
##   warpbreaks,
##   c("wool", "tension", "breaks"),
##   attribute = "breaks",
##   maxPercent = 50
## )


## ----eval=FALSE----------------------------------------------------------
## collapsibleTreeSummary(
##   warpbreaks,
##   c("wool", "tension", "breaks"),
##   attribute = "breaks",
##   maxPercent = 50,
##   nodeSize = "breaks",
##   collapsed = FALSE
## )


## ----eval=FALSE----------------------------------------------------------
## # Create a simple org chart
## org <- data.frame(
##   Manager = c(
##     NA, "Ana", "Ana", "Bill", "Bill", "Bill", "Claudette", "Claudette", "Danny",
##     "Fred", "Fred", "Grace", "Larry", "Larry", "Nicholas", "Nicholas"
##   ),
##   Employee = c(
##     "Ana", "Bill", "Larry", "Claudette", "Danny", "Erika", "Fred", "Grace",
##     "Henri", "Ida", "Joaquin", "Kate", "Mindy", "Nicholas", "Odette", "Peter"
##   ),
##   Title = c(
##     "President", "VP Operations", "VP Finance", "Director", "Director", "Scientist",
##     "Manager", "Manager", "Jr Scientist", "Operator", "Operator", "Associate",
##      "Analyst", "Director", "Accountant", "Accountant"
##   )
## )
## 
## # Add in colors and sizes
## org$Color <- org$Title
## levels(org$Color) <- colorspace::rainbow_hcl(11)
## 
## # Use unsplash api to add in random photos to tooltip
## org$tooltip <- paste0(
##   org$Employee,
##   "<br>Title: ",
##   org$Title,
##   "<br><img src='https://source.unsplash.com/collection/385548/150x100'>"
## )
## 
## collapsibleTreeNetwork(
##   org,
##   attribute = "Title",
##   fill = "Color",
##   nodeSize = "leafCount",
##   tooltipHtml = "tooltip"
## )


## ----eval=FALSE----------------------------------------------------------
## # Basic Shiny Interaction
## shiny::runApp(system.file("examples/02shiny", package = "collapsibleTree"))
## 
## # Interactive Gradient Mapping
## shiny::runApp(system.file("examples/03shiny", package = "collapsibleTree"))


## ------------------------------------------------------------------------
library(collapsibleTree)
date()

testthat::test_dir("tests/testthat", reporter = testthat::SummaryReporter)

