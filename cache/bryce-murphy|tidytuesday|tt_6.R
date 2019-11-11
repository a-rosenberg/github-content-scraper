library(tidyverse)
library(readxl)
library(leaflet)

# Read in all of the chains and clean some of the column names
path <- "C:/Users/Bryce/Desktop/Github_Projects/tidytuesday/tt6_coffee.xlsx"

starbucks <- 
  read_excel(path) %>% # Can specify sheet after path to select a different workbook page
  unite(col = "Geolocation", c("City", "State/Province", "Country"), sep = " | ")

starbucks %>%
  leaflet() %>%
  addTiles() %>%
  addAwesomeMarkers(clusterOptions = markerClusterOptions(), label = ~as.character(Geolocation))
  













