library(tidyverse)
library(countrycode)
library(ggridges)


ramen_ratings_raw <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-04/ramen_ratings.csv")


## Cargar fuentes de windows
library(extrafont)
loadfonts(device = "win")


ramen_ratings <- ramen_ratings_raw %>% 
  filter(style %in% c("Bowl", "Cup", "Pack", "Tray")) %>% 
  mutate(continent = countrycode(sourcevar = country, 
                                 origin = "country.name.en", 
                                 destination = "continent")) %>% 
  filter(!is.na(continent))


## Density ridges plot

ramen_ratings %>% 
  ggplot(aes(x = stars, y = continent, fill = continent, group = continent)) + 
  geom_density_ridges(alpha = 0.4) + 
  facet_wrap(. ~ style, scales = "free") + 
  labs(title = "Ramen rating distribution by continent",
       x = "Stars",
       y = "Continent",
       caption = "Data source: The Ramen Rater.")+
  theme(text = element_text(family = "Maiandra GD"),
        plot.background = element_rect(fill='#fff7ec'),
        plot.title = element_text(color='black',
                                  size=20),
        strip.text = element_text(size = 9),
        strip.background = element_rect(color = "#efe3d2", fill = '#f7e9d7'),
        panel.background = element_rect(color = "#efe3d2", fill = '#fff7ec'))

## Violin plot

ramen_ratings %>% 
  ggplot(aes(x = continent, y = stars, fill = continent, group = continent)) + 
  geom_violin(alpha = 0.4) + 
  facet_wrap(. ~ style, scales = "free") + 
  coord_flip() + 
  labs(title = "Ramen rating distribution by continent",
       x = "Stars",
       y = "Continent",
       caption = "Data source: The Ramen Rater.")+
  theme(text = element_text(family = "Maiandra GD"),
        plot.background = element_rect(fill='#fff7ec'),
        plot.title = element_text(color='black',
                                  size=20),
        strip.text = element_text(size = 9),
        strip.background = element_rect(color = "#efe3d2", fill = '#f7e9d7'),
        panel.background = element_rect(color = "#efe3d2", fill = '#fff7ec'))
