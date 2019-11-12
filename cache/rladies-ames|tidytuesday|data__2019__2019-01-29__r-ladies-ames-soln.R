## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----pkgs----------------------------------------------------------------
library(tidyverse)
library(janitor)
library(visdat)
library(sf)
library(USAboundaries)
# devtools::install_github('thomasp85/gganimate')
library(gganimate)


## ----getdat--------------------------------------------------------------
cheese <- read_csv("clean_cheese.csv")
fluid_milk <- read_csv("fluid_milk_sales.csv")
milkfacts <- read_csv("milk_products_facts.csv")
cowfacts <- read_csv("milkcow_facts.csv")
state_milk <- read_csv("state_milk_production.csv")


## ----glimpsedat----------------------------------------------------------
glimpse(cheese)
glimpse(fluid_milk)
glimpse(cowfacts)
glimpse(milkfacts)
glimpse(state_milk)


## ----usamap--------------------------------------------------------------
usa <- us_states()
usa <- usa %>% filter(name != "Alaska", name != "Hawaii", jurisdiction_type != "territory")

usa <- usa %>% filter(name != "District of Columbia")

usa_milk <- usa %>% left_join(state_milk, by = c("name" = "state"))
# usa_milk %>% filter(year == 1970) %>% 
# ggplot() + 
#   geom_sf(aes(fill = milk_produced)) + 
#   scale_fill_distiller(name = paste("Pounds of Milk Year", i), palette = "YlOrBr", direction = 2) + 
#   coord_sf() + 
#   theme_void() + 
#   theme(panel.grid = element_line(color = 'white'))


## ----ggani---------------------------------------------------------------

usa_milk %>% mutate(year = as.integer(year), milk_produced = milk_produced/10^9) %>% 
ggplot() +
  geom_sf(aes(fill = milk_produced)) +
  scale_fill_distiller(name = "Billions of Pounds of\nMilk Produced", palette = "YlOrBr", direction = 2) + 
  #facet_wrap(~year)
  labs(title = "Milk Production in {frame_time}") + 
   # Here comes the gganimate specific bits
  transition_time(year) 
# anim_save(filename = "milk.gif", animation = last_animation())

