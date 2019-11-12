## ------------------------------------------------------------------------
# some example data
data(starwars, package = "dplyr")
data(storms, package = "dplyr")


## ---- message=FALSE, warning=FALSE---------------------------------------
library(dplyr)
star_1 <- starwars %>% sample_n(50)
star_2 <- starwars %>% sample_n(50) %>% select(-1, -2)


## ------------------------------------------------------------------------
library(inspectdf)
inspect_cat(starwars)


## ------------------------------------------------------------------------
inspect_cat(starwars)$levels$hair_color


## ------------------------------------------------------------------------
inspect_cat(starwars) %>% show_plot()


## ------------------------------------------------------------------------
inspect_cat(starwars) %>% 
  show_plot(high_cardinality = 1)


## ------------------------------------------------------------------------
inspect_cat(star_1, star_2)


## ------------------------------------------------------------------------
inspect_cat(star_1, star_2) %>% show_plot()

