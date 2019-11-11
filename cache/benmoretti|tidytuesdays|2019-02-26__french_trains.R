#' french_trains.R
#'
#' @author Ben Moretti
#'
#' @description Reads data for the 26 Feb 2019 #tidytuesday - French Trains - and visualises using ggalluvial


# Libraries -----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(ggalluvial)
library(gganimate)

# Gather ------------------------------------------------------------------

trains_raw <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-26/full_trains.csv")


# Condition ---------------------------------------------------------------

# select data for paris departures for july 2017
paris_2017_trips <- trains_raw %>% 
  filter(
    year == 2017,
    month == 7,
    str_detect(departure_station, "PARIS")
  ) %>% 
  select(departure_station, arrival_station, total_num_trips, month) 

# check that it's in alluvial format - should return true
is_alluvia_form(paris_2017_trips, axes = 1:2, silent = TRUE)  

# Plot --------------------------------------------------------------------

# create alluvial plot
paris_2017_trips %>%
  ggplot(aes(y = total_num_trips, axis1 = departure_station, axis2 = arrival_station)) +
  geom_alluvium(aes(fill = departure_station)) +
  guides(fill = FALSE) +
  geom_label(stat = "stratum",
             label.strata = TRUE,
             size = 2) +
  scale_x_discrete(limits = c("Departure", "Arrival"),
                   expand = c(0.05, 0.05)) +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "Train trips departing from Paris stations in July 2017",
    y = "Total Number of Trips",
    subtitle = "Source: SNCF",
    caption = "#tidytuesday by @benmoretti"
  ) 


# Output ------------------------------------------------------------------


#save png
ggsave("2019-02-26/paris_july_2017_trains.png", units = "mm", width=297, height=210)
