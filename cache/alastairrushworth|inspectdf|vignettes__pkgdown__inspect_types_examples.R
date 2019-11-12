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
# return tibble showing columns types
inspect_types(starwars)


## ------------------------------------------------------------------------
# print visualisation of column types
inspect_types(starwars) %>% show_plot()


## ------------------------------------------------------------------------
inspect_types(star_1, star_2)


## ------------------------------------------------------------------------
# print visualisation of column type comparison
inspect_types(star_1, star_2) %>% show_plot()

