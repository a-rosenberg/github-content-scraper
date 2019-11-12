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
inspect_num(storms, breaks = 10)


## ------------------------------------------------------------------------
inspect_num(storms)$hist$pressure


## ------------------------------------------------------------------------
inspect_num(storms, breaks = 10) %>%
  show_plot()


## ------------------------------------------------------------------------
inspect_num(storms, storms[-c(1:10), -1])


## ------------------------------------------------------------------------
inspect_num(storms, storms[-c(1:10), -1]) %>% 
  show_plot()

