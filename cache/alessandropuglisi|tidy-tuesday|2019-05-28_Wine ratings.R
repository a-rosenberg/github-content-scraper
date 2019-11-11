## Load packages
library(tidyverse)
library(extrafont)
library(ggrepel)

## Get data
wine_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv")

## Drop X1 column, drop NAs, group by tasters and summarize
wine_tasters <- wine_ratings %>%
  select(-X1) %>%
  drop_na(taster_name, description, points) %>%
  group_by(taster_name) %>%
  summarize(points_mean = mean(points), mean_description_length = mean(stringi::stri_length(description)))
  
## Plot
ggplot(wine_tasters, aes(x = points_mean, y = mean_description_length, color = taster_name, label = taster_name)) +
  geom_jitter() +
  geom_label_repel(family = "Gabriola", size = 5) +
  labs(
    title = "Any correlation between points and description length?",
    x = "Points (mean)",
    y = "Description length (mean)"
  ) +
  theme(
    plot.background = element_rect(fill = "#722F37"),
    plot.title = element_text(family = "Gabriola", color = "#ffffff", size = 18),
    axis.title = element_text(family = "Gabriola", color = "#ffffff", size = 15),
    axis.text = element_text(family = "Gabriola", color = "#ffffff", size = 15),
    axis.ticks = element_line(color = "#ffffff"),
    legend.position = "none"
  )
