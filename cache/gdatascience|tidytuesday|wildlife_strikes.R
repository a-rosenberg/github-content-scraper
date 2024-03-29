## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
theme_set(theme_light())

wildlife_impacts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-23/wildlife_impacts.csv") %>%
  mutate(flight_phase = na_if(str_to_title(phase_of_flt), "Unknown"))

summary(wildlife_impacts)


## ------------------------------------------------------------------------
wildlife_impacts %>%
  count(incident_year) %>%
  ggplot(aes(incident_year, n)) +
  geom_line() + 
  geom_smooth(method = "lm")


## ------------------------------------------------------------------------
wildlife_impacts %>%
  filter(state != "N/A") %>%
  count(state, sort = TRUE)


## ------------------------------------------------------------------------
wildlife_impacts %>%
  filter(!is.na(flight_phase)) %>%
  group_by(flight_phase) %>%
  summarise(n = n()) %>%
  mutate(flight_phase = fct_reorder(flight_phase, n)) %>%
  ggplot(aes(flight_phase, n, fill = flight_phase)) + 
  geom_col(show.legend = FALSE) + 
  coord_flip() +
  labs(x = "Phase of flight",
       y = "# of strikes",
       title = "Flying? Watch out for birds on approach!!",
       subtitle = "There are also many strikes during take-off, climb, and landing",
       caption = "Designer: Tony Galvan @gdatascience1  |  Source: FAA")


## ------------------------------------------------------------------------
ggsave("wildlife_strikes.png", width = 5.5)


## ------------------------------------------------------------------------
top_struck_aircraft <- wildlife_impacts %>%
  filter(!is.na(flight_phase)) %>%
  group_by(atype) %>%
  summarise(n = n()) %>%
  top_n(9, n) %>%
  ungroup()

wildlife_impacts %>%
  inner_join(top_struck_aircraft) %>%
  filter(!is.na(flight_phase)) %>%
  group_by(atype, flight_phase) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(flight_phase = fct_reorder(flight_phase, n, sum),
         atype = fct_reorder(atype, n, sum)) %>%
  ggplot(aes(flight_phase, n, fill = flight_phase)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~atype, scales = "free_x") +
  coord_flip() +
  labs(x = "Phase of flight",
       y = "# of strikes",
       title = "When do strikes occur for the top struck aircraft?",
       subtitle = "Why does the MD-88 get struck so much during landing roll?",
       caption = "Designer: Tony Galvan @gdatascience1  |  Source: FAA")


## ------------------------------------------------------------------------
top_struck_airports <- wildlife_impacts %>%
  filter(!is.na(flight_phase)) %>%
  group_by(airport_id) %>%
  summarise(n = n()) %>%
  top_n(9, n) %>%
  ungroup()

wildlife_impacts %>%
  inner_join(top_struck_airports) %>%
  filter(!is.na(flight_phase)) %>%
  group_by(airport_id, airport, flight_phase) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(airport_name = paste0(airport, " (", airport_id, ")"),
         flight_phase = fct_reorder(flight_phase, n, sum),
         airport_name = fct_reorder(airport_name, n, sum)) %>%
  ggplot(aes(flight_phase, n, fill = flight_phase)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~airport_name, scales = "free_x") +
  coord_flip() +
  labs(x = "Phase of flight",
       y = "# of strikes",
       title = "In which phase of flight do strikes occur at airports with most strikes?",
       subtitle = "Why do so many strikes occur during landing roll and take-off run in Denver?",
       caption = "Designer: Tony Galvan @gdatascience1  |  Source: FAA")

