## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------
library(tidyverse)
library(ggthemes)
board_games <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-12/board_games.csv")


## ------------------------------------------------------------------------
board_games %>%
  group_by(year_published) %>%
  summarise(total = n(),
            total_ratings = sum(users_rated),
            average_ratings = sum(users_rated * average_rating) / sum(users_rated)) %>%
  filter(total > 20) %>%
  ggplot(aes(x = year_published, y = average_ratings)) +
  geom_point(aes(size = total)) +
  labs(x = "Year Published",
       y = "Average Rating",
       title = "Board Games are getting better over time",
       subtitle = "based on years with at least 20 games",
       size = "Total games",
       caption = "Data source: Board Game Geek, by: @trevin_flick") +
  theme_light()


## ------------------------------------------------------------------------
ggsave("board-games.png")


## ------------------------------------------------------------------------
board_games %>% 
  ggplot(aes(x = users_rated, y = average_rating)) +
  geom_point() +
  labs(x = "Number of user ratings",
         y = "Average rating",
         title = "Good games have more ratings")


## ------------------------------------------------------------------------
board_games %>%
  filter(playing_time < 1440) %>%
  ggplot(aes(x = playing_time, y = average_rating)) +
  geom_point() +
  labs(x = "Playing time",
         y = "Average rating",
         title = "")


## ------------------------------------------------------------------------
board_games_tidy <- board_games %>%
  separate(mechanic, sep = ",", into = paste("mechanic", 1:17, sep = "_"))


## ------------------------------------------------------------------------
mechanics <- board_games_tidy %>%
  gather(game, name, mechanic_1:mechanic_17, na.rm = TRUE)


## ------------------------------------------------------------------------
mechanics %>%
  group_by(name) %>%
  summarise(total = n(),
            total_ratings = sum(users_rated),
            average_ratings = sum(users_rated * average_rating) / sum(users_rated)) %>%
  arrange(desc(total_ratings)) %>%
  View()

