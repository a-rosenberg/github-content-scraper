board_games <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-12/board_games.csv")

#https://stackoverflow.com/questions/11857935/plotting-the-average-values-for-each-level-in-ggplot2
library(tidyverse)
ggplot(data = board_games, mapping = aes(x = year_published, y = playing_time)) +
  stat_summary(fun.y = 'mean', geom = 'bar') +
  ggtitle("Average Playing Time by Year")

games_1979 <- filter(board_games, year_published == 1979 & playing_time > 800)
