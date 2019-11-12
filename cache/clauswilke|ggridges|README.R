## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/README-"
)


## ----eval = FALSE--------------------------------------------------------
## install.packages("ggridges")


## ----eval = FALSE--------------------------------------------------------
## library(devtools)
## install_github("clauswilke/ggridges")


## ----diamonds------------------------------------------------------------
library(ggplot2)
library(ggridges)
    
ggplot(diamonds, aes(x = price, y = cut)) +
  geom_density_ridges(scale = 4) + theme_ridges() +
  scale_y_discrete(expand = c(0.01, 0)) +   # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0))      # for both axes to remove unneeded padding

