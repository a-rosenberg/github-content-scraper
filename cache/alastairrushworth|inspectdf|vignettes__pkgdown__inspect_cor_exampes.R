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
inspect_cor(storms)


## ------------------------------------------------------------------------
inspect_cor(storms) %>% show_plot()


## ------------------------------------------------------------------------
inspect_cor(storms, storms[-c(1:200), ])


## ------------------------------------------------------------------------
inspect_cor(storms, storms[-c(1:200), ]) %>% 
  slice(1:20) %>%
  show_plot()

