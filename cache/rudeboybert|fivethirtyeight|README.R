## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "README-"
)
library(fivethirtyeight)


## ---- eval = FALSE-------------------------------------------------------
## install.packages("fivethirtyeight")


## ---- eval = FALSE-------------------------------------------------------
## # If you haven't installed the remotes package yet, do so:
## # install.packages("remotes")
## remotes::install_github("rudeboybert/fivethirtyeight", build_vignettes = TRUE)


## ---- eval = FALSE-------------------------------------------------------
## library(fivethirtyeight)
## 
## # Load the bechdel data set. Note that all data in the fivethirtyeight package
## # is lazy-loaded, so one can also access this data without running data(bechdel).
## data(bechdel)
## head(bechdel)
## ?bechdel
## 
## # If using RStudio:
## View(bechdel)


## ---- eval = FALSE-------------------------------------------------------
## vignette("user_contributed_vignettes.Rmd", package = "fivethirtyeight")

