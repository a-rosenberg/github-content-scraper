## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------
library(tidyverse)
theme_set(theme_light())

bird_counts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-18/bird_counts.csv")


## ------------------------------------------------------------------------
bird_counts %>%
  summary()


## ------------------------------------------------------------------------
bird_counts %>%
  group_by(year) %>%
  summarise(total = sum(how_many_counted)) %>%
  ggplot(aes(year, total)) +
  geom_line() + 
  labs(x = "",
       y = "# of birds",
       title = "Christmas birds counted",
       subtitle = "from 1921 to 2017",
       caption = "Designer: Tony Galvan @gdatascience1  |  Source: Bird Studies Canada")


## ------------------------------------------------------------------------
bird_counts %>%
  filter(!is.na(total_hours)) %>%
  group_by(year) %>%
  summarise(hours = mean(total_hours)) %>%
  ggplot(aes(year, hours)) +
  geom_line() + 
  labs(x = "",
       y = "# of hours",
       title = "Hours spent counting Christmas birds",
       subtitle = "from 1921 to 2017",
       caption = "Designer: Tony Galvan @gdatascience1  |  Source: Bird Studies Canada")

