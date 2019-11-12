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
inspect_na(starwars)


## ------------------------------------------------------------------------
inspect_na(starwars) %>% show_plot()


## ------------------------------------------------------------------------
inspect_na(star_1, star_2)


## ------------------------------------------------------------------------
inspect_na(star_1, star_2) %>% show_plot()

