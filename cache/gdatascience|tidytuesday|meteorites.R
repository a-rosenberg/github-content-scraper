## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------
library(tidyverse)
theme_set(theme_light())

meteorites <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-11/meteorites.csv") %>%
  filter(!is.na(year) & !is.na(mass)) %>%
  mutate(decade = 10 * (year %/% 10))


## ------------------------------------------------------------------------
meteorites %>%
  ggplot(aes(decade)) +
  geom_histogram(binwidth = 30)


## ------------------------------------------------------------------------
meteorites %>%
  mutate(class = fct_lump(class, 24)) %>%
  count(class, sort = TRUE) %>%
  mutate(class = fct_reorder(class, n)) %>%
  ggplot(aes(class, n, fill = class)) + 
  geom_col(show.legend = FALSE) + 
  coord_flip() + 
  labs(x = "Class",
       y = "# of meteorites",
       title = "Top meteorite classes",
       caption = "Designer: Tony Galvan @gdatascience1  |  Source: Meteoritical Society")


## ------------------------------------------------------------------------
meteorites %>%
  mutate(log_mass = log10(mass)) %>%
  ggplot(aes(log_mass)) +
  geom_histogram()


## ------------------------------------------------------------------------
meteorites %>%
  mutate(class = fct_lump(class, 24)) %>%
  ggplot(aes(long, lat, size = mass, color = class)) + 
  geom_point(alpha = 0.5) + 
  coord_map() + 
  theme_void() + 
  theme(legend.position = "none") +
  facet_wrap(~class)


## ------------------------------------------------------------------------
meteorites %>%
  filter(!is.na(mass) & mass > 0) %>%
  group_by(decade) %>%
  summarise(n = n(),
            average = mean(mass),
            heaviest = max(mass),
            lightest = min(mass))

