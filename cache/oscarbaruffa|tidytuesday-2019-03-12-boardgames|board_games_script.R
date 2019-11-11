library(tidyverse)
library(tidylog)
library(extrafont)
library(jpeg)


loadfonts(device = "win")


board_games <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-12/board_games.csv")

fam_games <- board_games %>% 
  filter(min_age <= 5) %>% 
  filter(max_playtime <= 30) %>% 
  filter(users_rated >= 300)
  

fam_games %>% 
  ggplot(aes(year_published, average_rating)) +
  geom_point(aes(size = users_rated, fill = average_rating), 
             colour = "black", shape = 21, stroke = 2, alpha = 0.8) +
 
  labs(title = "Modern family-friendly games are better",
       subtitle = "User rating out of 10 for n=99 games\nMax playing time< 30 min ,\nMin age 5 or below and at least 300 ratings",
       y = "",
       x = "",
       caption = "plot: @oscar_b1233\nData:Board Game Geeks") +
  expand_limits(y = c(0,10)) +
  scale_x_continuous(breaks = seq(1960, 2020, 10)) +
  theme(legend.position = "none",
        panel.border = element_rect(colour = "black", fill = NA, size = 3),
        panel.background = element_rect(fill = "#c9e341"),
        plot.background = element_rect(fill = "#b67904"),
        axis.text = element_text(colour = "white"),
        plot.title = element_text(colour = "white"),
        plot.subtitle = element_text(colour = "white"),
        plot.caption = element_text(colour = "grey"),
        text = element_text(family = "Kristen ITC")) 
  
  
ggsave("rplot.png", type = 'cairo')


#EDA scratch pad

#DataExplorer::create_report(board_games) #Didnt work

board_games %>% 
  ggplot() +geom_histogram(aes(min_age))

board_games %>% 
  filter(max_playtime < 250) %>% 
  ggplot() +geom_histogram(aes(max_playtime))



