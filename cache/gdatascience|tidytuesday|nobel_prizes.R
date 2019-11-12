## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------
library(tidyverse)

theme_set(theme_minimal())

nobel_winners <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")
nobel_winner_all_pubs <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winner_all_pubs.csv")


## ------------------------------------------------------------------------
nobel_winners %>%
  group_by(laureate_id, birth_country) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  ungroup() %>%
  mutate(birth_country = fct_lump(birth_country, n = 7)) %>%
  group_by(birth_country) %>%
  summarise(n2 = n()) %>%
  filter(!is.na(birth_country)) %>%
  mutate(birth_country = fct_reorder(birth_country, n2)) %>%
  ggplot(aes(birth_country, n2, fill = birth_country)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(x = "",
       y = "# of multiple Nobel Prize winners", 
       title = "The USA has the most multiple Nobel Prize winners")

