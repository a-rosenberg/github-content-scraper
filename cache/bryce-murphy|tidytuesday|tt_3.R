library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(RCurl)
library(maps)
library(mapdata)
library(leaflet)
library(ggplot2)
library(plotly)

path <- getURL("https://raw.githubusercontent.com/bryce-murphy/tidytuesday/master/global_mortality_3.csv")


# Read in the mortality dataset from github, clean the variable names, and make observations more readable

mort <- read_csv(path) %>%
  clean_names() %>%
  rename_all(
    funs(
      str_replace(., "_percent", ""))) %>%
  mutate_if(is.numeric, funs(round(., 4)))

# United States is called USA in the mapdata package

mort$country <-
  mort$country %>%
  str_replace("United States", "USA")

world <- map_data("world") %>%
  rename(country = region)

# Combine the datasets and filter

world_mort <- 
  mort %>%
  inner_join(world, by = "country") %>%
  filter(year == 2016) %>%
  gather(key = mortality_type, value = mortality_rate, -c(1:3, 36:40)) %>%
  filter(mortality_type == "cancers") %>%
  mutate(z_score = ave(mortality_rate, mortality_type, FUN = scale))


# Save ggplot object to be used in plotly

p <- ggplot(world_mort, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = mortality_rate), color = "black") +
  coord_fixed(1.3) +
  scale_fill_gradient(high = "red", low = "green") +
  labs(title = "Cancer Mortality Rates Across the Globe, 2016")

#
ggplotly(p)



























m = leaflet() %>% addTiles()
df = data.frame(
  lat = rnorm(100),
  lng = rnorm(100),
  size = runif(100, 5, 20),
  color = sample(colors(), 100)
)
m = leaflet(df) %>% addTiles()
m %>% addCircleMarkers(radius = ~size, color = ~color, fill = FALSE)
m %>% addCircleMarkers(radius = runif(100, 4, 10), color = c('red'))

