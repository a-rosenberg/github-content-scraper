## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(collapsibleTree)


## ----org-----------------------------------------------------------------
org <- data.frame(
    Manager = c(
        NA, "Ana", "Ana", "Bill", "Bill", "Bill", "Claudette", "Claudette", "Danny",
        "Fred", "Fred", "Grace", "Larry", "Larry", "Nicholas", "Nicholas"
    ),
    Employee = c(
        "Ana", "Bill", "Larry", "Claudette", "Danny", "Erika", "Fred", "Grace",
        "Henri", "Ida", "Joaquin", "Kate", "Mindy", "Nicholas", "Odette", "Peter"
    ),
    Title = c(
        "President", "VP Operations", "VP Finance", "Director", "Director", "Scientist",
        "Manager", "Manager", "Jr Scientist", "Operator", "Operator", "Associate",
        "Analyst", "Director", "Accountant", "Accountant"
    )
)


## ----collapsibleTree-----------------------------------------------------
collapsibleTree(org, c("Manager", "Employee"), collapsed = FALSE)


## ----basic---------------------------------------------------------------
collapsibleTreeNetwork(org, attribute = "Title", collapsed = FALSE)


## ----color---------------------------------------------------------------
org$Color <- org$Title
levels(org$Color) <- colorspace::rainbow_hcl(11)
collapsibleTreeNetwork(
  org,
  attribute = "Title",
  fill = "Color",
  nodeSize = "leafCount",
  collapsed = FALSE
)


## ----unsplash------------------------------------------------------------
org$tooltip <- paste0(
  org$Employee,
  "<br>Title: ",
  org$Title,
  "<br><img src='https://source.unsplash.com/collection/385548/150x100'>"
)

collapsibleTreeNetwork(
  org,
  attribute = "Title",
  fill = "Color",
  nodeSize = "leafCount",
  tooltipHtml = "tooltip",
  collapsed = FALSE
)

