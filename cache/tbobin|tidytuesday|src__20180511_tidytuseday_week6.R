

library(tidyverse)
library(readxl)
library(scales)
library(geojsonio)
library(broom)
library(viridis)
library(rgeos)
library(skimr)


# read in data
df_cof_raw <- readxl::read_xlsx(paste0(here::here(),"/data/week6_coffee_chains.xlsx"))

# let's start with stores in the US by Brand

df_cof_all <- df_cof_raw %>% 
  filter(Country == "US") %>% 
  group_by(`State/Province`, Brand) %>% 
  count()

# let's take a look at the data
df_cof_all %>% 
  ggplot(aes(n)) +
  geom_histogram(binwidth = 100)

df_cof_all %>% ungroup %>% select(-(`State/Province`)) %>% skim(n)

## From r-graph-gallery: https://www.r-graph-gallery.com/328-hexbin-map-of-the-usa/
# Hexbin available in the geojson format here: https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map. Download it and then:
spdf <- geojson_read(paste0(here::here(),"/data/us_states_hexgrid.geojson"),  what = "sp")

# I need to 'fortify' the data to be able to show it with ggplot2 (we need a data frame format)
# spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf_fortified <- tidy(spdf, region = "iso3166_2")


# join data with spital data
spdf_fortified <- spdf_fortified %>% 
  left_join(df_cof_all, by=c("id" = "State/Province"))

#
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))

# Prepare binning
spdf_fortified$bin = cut( spdf_fortified$n , breaks=c(seq(0,500,100), Inf), 
                          labels=c("0-100", "101-200", "201-300", "301-400", "401-500", "500+" ), include.lowest = TRUE )



# lets plot the Starbucks map
spdf_fortified %>% 
  filter(Brand == "Starbucks") %>% 
  ggplot(aes()) +
  geom_polygon(aes(fill = bin, x = long, y = lat, group = group) , size=0, alpha=0.9) +
  geom_text(data=centers, aes(x=x, y=y, label=id), color="white", size=3, alpha=0.6) +
  scale_fill_viridis(option = "viridis", discrete=TRUE,
                     name = "") +
  theme_minimal() +
  labs(title = "Starbucks Coffee stores per State",
       caption = "@T_Bobin \nsource: kaggle.com")+
  theme(panel.border=element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom") +
  coord_map() 

#ggsave("/graphs/20180512_tidyTuseday_week_6.png", width = 10, dpi = 600)

