###############################
#                             #
#       Tidy Tuesday          #
#        2018-05-07           #
#                             #
###############################

# created using taraskaduk's amazing tutorial:
# https://taraskaduk.com/2017/11/26/pixel-maps/ (crosspost: https://medium.com/taras-kaduk/r-walkthrough-create-a-pixel-map-537ce12c2f0c)


library(readxl)
library(tidyverse)
library(maps)
library(janitor)

coffee <-
  read_excel(".//data//week6_coffee_chains.xlsx") %>% 
  clean_names()

coffee <- 
  coffee %>% 
  mutate(long_round = round(longitude),
         lat_round = round(latitude, 0))

lat <- 
  data_frame(lat = seq(-90, 90, by = 1))

long <- 
  data_frame(long = seq(-180, 180, by = 1))

dots <- 
  lat %>% 
  merge(long, all = TRUE)

# only include dots that are within borders
# also, exclude lakes

dots <- 
  dots %>% 
  mutate(country = map.where('world', long, lat),
         lakes = map.where('lakes', long, lat)) %>% 
  filter(!is.na(country) & is.na(lakes)) %>% 
  select(-lakes)

theme <- 
  theme_void() +
  theme(panel.background = element_rect(fill="#212121"),
        plot.background = element_rect(fill="#212121"),
        plot.title = element_text(face="bold", colour="#3C3C3C", size = 16),
        plot.subtitle = element_text(colour="#3C3C3C", size=12),
        plot.caption = element_text(colour="#3C3C3C",size=10),  
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

map_brands <- 
  ggplot() +   
  # base layer of map dots
  geom_point(data = dots, aes(x = long, y = lat), col = "#F2EEE9", size = 0.7) + 
  geom_point(data = coffee %>% filter(brand == 'Starbucks'), aes(x = long_round, y = lat_round), color = "#43634A", size = 0.7) +
  geom_point(data = coffee %>% filter(brand == 'Teavana'), aes(x = long_round, y = lat_round), color = "#AD7E5C", size = 0.7) +
  geom_point(data = coffee %>% filter(brand == 'Coffee House Holdings'), aes(x = long_round, y = lat_round), color = "#E7E1D5", size = 0.7) +
  geom_point(data = coffee %>% filter(brand == 'Evolution Fresh'), aes(x = long_round, y = lat_round), color = "#5D3B24", size = 0.7) +
  theme

map_brands

map_empty <- 
  ggplot() +   
  # base layer of map dots
  geom_point(data = dots, aes(x = long, y = lat), col = "#F2EEE9", size = 0.7) + 
  geom_point(data = coffee, aes(x = long_round, y = lat_round), color = "#212121", size = 0.7) +
  theme

map_empty

ggsave('.//plot//map_empty.png', 
       plot = map_empty,
       device = 'png', 
       path = getwd(), 
       width = 360, 
       height = 180, 
       units = 'mm',
       dpi = 250)
