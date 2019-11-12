## ----message = F, warning = F, include = F-------------------------------
library(tidyverse)

## ---- message = F, warning = F-------------------------------------------
world1 <- sf::st_as_sf(maps::map('world', plot = FALSE, fill = TRUE))

# data countries not in the worldmap
cnty1 <- read_csv("../data/week14_global_life_expectancy.csv") %>% 
  filter(!is.na(code)) %>% 
  anti_join(world1, by = c("country" = "ID")) %>% 
  distinct(country)
cnty1$country

