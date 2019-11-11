#' boardgames.R


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(janitor)
library(vapoRwave)
library(extrafont)


# Gather ------------------------------------------------------------------

board_games_tbl <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-12/board_games.csv") %>% clean_names()


# Visualise -------------------------------------------------------------------


# Top 8 Board Games’ Mechanics vs Time ------------------------------------

# get a character array of the top 8 mechanics
top_8_mechanics <- board_games_tbl %>% 
  separate_rows(mechanic, sep = ",") %>% 
  count(mechanic) %>%
  arrange(desc(n)) %>%
  drop_na(mechanic) %>%
  slice(1:8) %>%
  pull(mechanic)

# just a simple line plot but using vapoRwave theme
board_games_tbl %>% 
  separate_rows(mechanic, sep = ",") %>%
  filter(mechanic %in% top_8_mechanics) %>%
  count(mechanic, year_published) %>%
  ggplot(aes(year_published, n)) +
  geom_line(aes(colour=mechanic), size=2) +
  floral_shoppe() + 
  scale_color_floralShoppe() +
  labs(
    title = "Top 8 Board Games' Mechanics vs Time",
    subtitle = "Source: boardgamegeek.com / @benmoretti",
    x = "Year",
    y = "Number of Games"
  ) +
  theme(
    legend.position = "bottom"
  )
  
ggsave("2019-03-12/Growth_vs_time.png")


# The average rating for the top game of each year vs time -------------------------------------

board_games_tbl %>%
  group_by(year_published) %>%
  arrange(desc(average_rating)) %>%
  slice(1) %>%
  ungroup() %>%
  ggplot(aes(year_published, average_rating)) +
  geom_point(aes(size=users_rated), colour="#E3D26F") +
  geom_smooth(se=FALSE, linetype="dashed", colour = "#FAA275") +
  floral_shoppe() + 
  scale_colour_floralShoppe() +
  labs(
    title = "The average rating for the top game of each year vs time",
    subtitle = "Source: boardgamegeek.com / @benmoretti",
    x = "Year",
    y = "Average Rating"
  ) +
  theme(
    legend.position = "bottom"
  )

ggsave("2019-03-12/rating_vs_time.png")

