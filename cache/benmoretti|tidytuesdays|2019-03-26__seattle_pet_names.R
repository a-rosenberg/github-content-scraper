#' tidytuesday 20-03-26
#' Seattle Pet Names


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(zipcode)
library(timetk) 
library(leaflet)
library(janitor)

# Gather ------------------------------------------------------------------

data(zipcode)

seattle_pets_tbl <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-26/seattle_pets.csv")


# Condition ---------------------------------------------------------------

seattle_pets_conditioned_tbl <- seattle_pets_tbl %>%
  clean_names() %>%
  mutate(
    license_issue_date = mdy(license_issue_date)
  ) %>%
  inner_join(zipcode, by = c("zip_code"="zip")) %>%
  tk_augment_timeseries_signature()



# Visualise ---------------------------------------------------------------

# map the location of cats in 2018 by zip code
seattle_pets_conditioned_tbl %>%
  filter(
    species == "Cat",
    year == 2018
  ) %>% 
  count(zip_code, latitude, longitude) %>%
  leaflet() %>% addTiles() %>% addCircleMarkers()


common_names_plot <- seattle_pets_tbl %>%
  count(animals_name, species) %>% 
  spread(species, n) %>%
  filter(
    ! is.na(Cat),
    ! is.na(Dog),
    ! is.na(Pig)
  ) %>%
  select(-Goat) %>%
  mutate(total = Cat + Dog + Pig) %>%
  gather(species, count, -animals_name, -total) %>%
  drop_na(animals_name) %>%
  mutate(
    animals_name = fct_reorder(animals_name, total)
  ) %>%
  ggplot(aes(animals_name, count)) +
  geom_col(aes(fill=species)) +
  theme_light() +
  scale_fill_viridis_d() +
  coord_flip() +
  theme(legend.position = "bottom") +
  labs(
    title = "Common Seattle Pet Names",
    x = "Animal Name",
    y = "Count",
    fill = "Animal Species",
    subtitle = "Source: Seattle Open Data",
    caption = "#tidytuesday / 2019-03-26 / @benmoretti"
  ) 

ggsave("2019-03-26/common_names.png", common_names_plot)  
  
  