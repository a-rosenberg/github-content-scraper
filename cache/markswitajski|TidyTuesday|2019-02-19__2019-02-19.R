library(tidyverse)
library(gganimate)

degrees <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-19/phd_by_field.csv") %>% 
  filter(major_field == "Mathematics and statistics" & !is.na(n_phds))

phds <- 
  ggplot(degrees, aes(x = reorder(field, n_phds), y = n_phds)) +
  geom_bar(stat = "identity", fill= "#810F7C") +
  coord_flip() + 
  
  theme_minimal(16) +
  theme(plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 13.5, color = "gray40", face = "bold"),
        axis.title.x = element_text(size = 15, color = "gray40", face = "bold"),
        axis.title.y = element_text(size = 15, color = "gray40", face = "bold")) +

  labs(
    title = "What Are Young\nMathematicians Studying?",
    subtitle = 'Year: {frame_time}', x = 'Field of Study', y = 'Number of PhDs Awarded') +
    transition_time(year) +
    ease_aes('quintic-in-out')

phds

anim_save(filename = "20190219_Mathematics_PhDs.gif", width = 40)
