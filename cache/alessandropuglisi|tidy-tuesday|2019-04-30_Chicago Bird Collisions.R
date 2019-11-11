### Load libraries
library(tidyverse)
library(ggplot2)
library(RColorBrewer)

### Get the data
bird_collisions <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-30/bird_collisions.csv")
mp_light <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-30/mp_light.csv")

### MP as locality + left join with mp_light
bird_collisions_complete <- bird_collisions %>%
  filter(locality == "MP") %>%
  left_join(mp_light, by = "date") %>%
  filter(!is.na(light_score))
  
### Plot       
bird_collisions_complete %>%
  ggplot(aes(flight_call, light_score, fill = flight_call)) +
  geom_violin() +
  annotate("rect", xmin = 1.8, xmax = 2.2, ymin = 7, ymax = 10.5, alpha = .2) +
  annotate("text", x = 2.3, y = 6, label = "Why?") +
  annotate("segment", x = 2.3, xend = 2.2, y = 6.4, yend = 7) +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "Flight call vs Light score @ McCormick Place, 2000-2016",
    x = "Flight call",
    y = "Light score",
    caption = "Source: https://doi.org/10.1098/rspb.2019.0364 \n @_apuglisi_ #TidyTuesday Week 18") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16),
    plot.caption = element_text(size = 10),
    plot.background = element_rect(fill = "#f7d78c"),
    panel.background = element_rect(fill = "#edeff2"))

ggsave("2019-04-30_Chicago Bird Collisions.png")
