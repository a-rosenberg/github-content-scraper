library(tidyverse)
library(ggmap)
library(maps)
library(gganimate)
library(leaflet)
library(rgeos)
library(rworldmap)
library(stringr)





# set working dir to source file location
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


wine <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv")


# Alternative country centroids from: https://worldmap.harvard.edu/data/geonode:country_centroids_az8
# centroids <- read_csv("country_centroids_az8.csv")

# But now using these centroids 
# get world map
wmap <- getMap(resolution="high")
# get centroids
centroids <- gCentroid(wmap, byid=TRUE)
# get a data.frame with centroids
centroids <- as.data.frame(centroids)
names(centroids) <- c('Longitude', 'Latitude')
centroids$country <- row.names(centroids)

# the countries in wine
wine_countries <- unique(wine$country)
# which country names are written differently?
wine_countries[!(wine_countries %in% row.names(centroids))]
# fix those
# Where's Serbia?
# row.names(centroids)[str_detect(row.names(centroids), "erbi")]
wine$country <- recode(wine$country,
                       'US' = 'United States of America',
                       'England' = 'United Kingdom',
                       'Serbia' = 'Republic of Serbia')

# Aggregate the wine data to country level
wine_summ <- wine %>% 
    arrange(country, desc(points)) %>% 
    group_by(country) %>% 
    summarise(best_wine_title = first(title),
              best_wine_points = first(points),
              best_wine_price = first(price),
              country_mean_points = mean(points, na.rm = TRUE),
              country_mean_price = mean(price, na.rm = TRUE),
              country_n_wines = n()) %>% 
              filter(!is.na(country))

# Add latitude and longitude of country centroids
wine_summ <- left_join(wine_summ,
          centroids,
          by='country')

# TODO format long titles so they include a html break
# wine_summ %>%
#     mutate(title_formatted = str_locate(pattern =' ', best_wine_title)[1]) %>% 
#     select(title_formatted)
# max(nchar(wine_summ$best_wine_title))


# Example code from: https://www.r-graph-gallery.com/19-map-leafletr/
# Create a color palette with handmade bins.
mybins=seq(83, 92, by=1)
mypalette = colorBin("Reds", domain=wine_summ$country_mean_points, na.color="transparent", bins=mybins)

# Prepar the text for the tooltip:
mytext=paste("Country: ", wine_summ$country, "<br/>",
             "Nr. of wines: ", wine_summ$country_n_wines, "<br/>",
             "Mean wine rating: ", round(wine_summ$country_mean_points, 1), "<br/>",
             "Best wine in country: ", wine_summ$best_wine_title, "<br/>",
             "Best wine rating: ", wine_summ$best_wine_points,"<br/>",
             "Best wine price: $", wine_summ$best_wine_price, sep="") %>%
    lapply(htmltools::HTML)

# Final Map
leaflet(wine_summ) %>% 
    addTiles()  %>% 
    setView( lat=10, lng=10 , zoom=2) %>%
    addProviderTiles("Esri.WorldImagery") %>%
    addCircleMarkers(~Longitude, ~Latitude, 
                     fillColor = ~mypalette(country_mean_points), fillOpacity = 0.7, color="white", radius=wine_summ$country_n_wines ^ (1/4), stroke=FALSE,
                     label = mytext,
                     labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
    ) %>%
    addLegend(pal=mypalette, values=~country_mean_points, opacity=0.9, title = "Mean Wine Rating", position = "bottomright" )













