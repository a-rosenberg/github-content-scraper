### Load libraries 
library(tidyverse)
library(ggplot2)

### Read tidy *.csv
tidy_anime <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/tidy_anime.csv")

### Select variables
tidy_anime %>%
  select(name, type, source, studio, episodes, rating,
         scored_by, rank, popularity, members, favorites) %>%
  
  ### Extract unique elements  
  unique() %>%
  
  ### Arrange in descending order and select the first 10 elements
  arrange(desc(episodes)) %>% 
  head(10) %>%
  
    ### Plot
    ggplot(aes(x=reorder(name, episodes), y = episodes, fill = name)) +
    geom_col() +
    coord_flip() +
    labs(title = "How many episodes?",
         subtitle = "Top 10", 
         x = "", 
         y = "Number of episodes", 
         caption = "Data source: MyAnimeList.net \n @_apuglisi_ #TidyTuesday Week 17") +
    theme(legend.position = "none",
          plot.background = element_rect(fill = "#000000"),
          plot.title = element_text(colour = "#ffffff", size = 16, hjust = 0.5), 
          plot.subtitle = element_text(colour = "#ffffff", size = 13, hjust = 0.5),
          plot.caption = element_text(colour = "#ffffff"),
          panel.background = element_rect(fill = "#edeaf2"),
          axis.title.x = element_text(colour = "#ffffff"),
          axis.text = element_text(colour = "#ffffff", size = 14))

ggsave("2019-04-23_Anime Dataset.png")
