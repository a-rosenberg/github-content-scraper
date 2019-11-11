
source('get_data.R')
library(tidyverse)
library(plotly)
library(magrittr)
library(countrycode)
library(processx)
library(glue)

wildlife_impacts %>%
  filter(airport_id == 'ZZZZ') %>%
  group_by(airport) %>%
  summarise(n()) 

# combining the data with open flights data:
# Data by:h ttps://openflights.org/data.html#license

airports <- readr::read_csv('airports.csv', col_names = c('Index', 'Airport_name',
                                                          'City', 'Country', 'three_letter_code',
                                                          'four_letter_code', 'latitude',
                                                          'longitude', 'altitude', 'timezone',
                                                          'dst', 'tz_db', 'type', 'source'))
clean_data <- wildlife_impacts %>%
  left_join(airports, by = c('airport_id'='four_letter_code')) %>% 
  na.omit(latitude) %>%
  mutate(Country_code = countrycode(Country, origin = 'country.name',
                                    destination = 'iso3c'))

filtered_grouped_data <- clean_data %>%
  group_by(Country_code) %>%
  summarise(number_of_incidents = n())



fonts <- list(
  family = "sans serif",
  size = 22,
  color = 'black')

p_global <- filtered_grouped_data %>%
  plot_geo() %>%
  add_trace(
    z = ~number_of_incidents, color = ~number_of_incidents, colors = 'YlGnBu',
    text = ~Country_code, locations = ~Country_code
  ) %>%
  colorbar(title = 'Number of incidents reported',
           y = 0.8) %>%
  layout(
    title = glue("<br>Reported wildlife incidents between {format(min(clean_data$incident_date), '%B %Y')} and {format(max(clean_data$incident_date), '%B %Y')}"
        ),
    font=fonts) 

export(p_global, file = "global.png")


p_us_only <- clean_data %>%
  filter(Country == 'United States') %>%
  group_by(state) %>%
  summarise(number_of_incidents = n()) %>%
  plot_geo(locationmode = 'USA-states') %>%
  add_trace(
    z = ~number_of_incidents, color = ~number_of_incidents, colors = 'YlGnBu',
    text = ~state, locations = ~state
  ) %>%
  colorbar(title = 'Number of incidents reported',
          #x = -0.05,
          y = 0.8) %>%
  layout(
    title = glue("<br>Reported wildlife incidents between {format(min(clean_data$incident_date), '%B %Y')} and {format(max(clean_data$incident_date), '%B %Y')}"
    ),
    font=fonts,
    geo = list(scope = 'usa')) 

export(p_us_only, file = "us_only.png")

