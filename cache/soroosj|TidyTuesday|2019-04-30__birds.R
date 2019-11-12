## ----setup, warning = FALSE, results = FALSE, message = FALSE------------

library (tidyverse)
library (janitor)
library (ggthemes)
library (ggridges)
library (stringr)



## ----source, warning = FALSE, results = FALSE, message = FALSE-----------

birds_raw <- read_delim(
   "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-30/bird_call.csv",
   delim = " "
   )%>%
   clean_names() 
   


## ------------------------------------------------------------------------

birds <- birds_raw %>%
   drop_na () %>%
   group_by (species) %>%
   filter (flight > 50)

tabyl (birds, collisions)

   


## ----chart---------------------------------------------------------------

ggplot (data = birds, aes(x = flight, y = collisions)) +
  geom_density_ridges ()


