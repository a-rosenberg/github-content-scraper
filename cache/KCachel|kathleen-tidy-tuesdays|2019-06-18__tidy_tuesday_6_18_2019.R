library(tidyverse)
library(ggthemes)
library(ggdark)
library(magick)
library(grid)
library(gridExtra)
library(gganimate)
bird_counts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-18/bird_counts.csv")

summary(bird_counts)

top8birds <- bird_counts %>%
  group_by(species) %>%
  summarize(s = sum(how_many_counted)) %>%
  arrange(-s) %>%
  head(8) %>%
  inner_join(bird_counts) %>%
  filter(species != "European Starling")

b <- ggplot(data = top8birds, aes(y = how_many_counted, x = species, fill = species)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = how_many_counted, color = species), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.5) +
  #expand_limits(x = 1) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Spectral") +
  scale_fill_brewer(palette = "Spectral") +
  coord_flip() +
  theme_bw()


a <- ggplot(top8birds, aes(x = total_hours, y = how_many_counted, color = species)) + 
  geom_point(size = 3.5) +
  scale_color_brewer(palette = "Spectral") + 
  theme_bw()
  
a + transition_time(year) +
  labs(title = "Year: {frame_time}")+
  shadow_wake(wake_length = 0, alpha = FALSE)

p <- ggplot(data = top8birds, aes(y = how_many_counted, x = year, color = species)) +
  geom_line(stat= 'identity')+
  scale_color_brewer(palette = "Spectral") +
  theme_bw()

p + transition_reveal(year)
