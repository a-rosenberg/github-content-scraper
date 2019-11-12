## ----eval = FALSE--------------------------------------------------------
## rmarkdown::render("diamond-sizes.Rmd", output_format = "word_document")


## ---- echo = FALSE, out.width = NULL-------------------------------------
knitr::include_graphics("screenshots/rmarkdown-knit.png")


## ---- eval = FALSE-------------------------------------------------------
## knitr::opts_chunk$set(echo = FALSE)


## ---- echo = FALSE, out.width = "75%"------------------------------------
knitr::include_graphics("screenshots/rmarkdown-flexdashboard.png")


## ----comment = "", echo = FALSE------------------------------------------
cat(readr::read_file("rmarkdown/dashboard.Rmd"))


## ------------------------------------------------------------------------
library(leaflet)
leaflet() %>%
  setView(174.764, -36.877, zoom = 16) %>% 
  addTiles() %>%
  addMarkers(174.764, -36.877, popup = "Maungawhau") 


## ---- eval = FALSE-------------------------------------------------------
## library(shiny)
## 
## textInput("name", "What is your name?")
## numericInput("age", "How old are you?", NA, min = 0, max = 150)

## ---- echo = FALSE, out.width = NULL-------------------------------------
knitr::include_graphics("screenshots/rmarkdown-shiny.png")


## ----echo = FALSE, comment = ""------------------------------------------
cat(readr::read_file("rmarkdown/example-site.yml"))

