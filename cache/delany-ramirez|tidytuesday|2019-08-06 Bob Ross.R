library(tidyverse)
library(janitor)
library(RColorBrewer)
library(extrafont)

font_import()
loadfonts(device = "win")


bob_ross_raw <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-06/bob-ross.csv")

glimpse(bob_ross_raw)

bob_ross <- bob_ross_raw %>% 
  gather(key = "element", value = "presence", APPLE_FRAME:WOOD_FRAMED) %>% 
  filter(presence != 0) %>% 
  separate(col = EPISODE, into = c("season", "episode"), sep = "E") %>% 
  mutate(season = parse_number(season),
         season = factor(season),
         episode = parse_integer(episode)) %>% 
  clean_names() %>% 
  select(-presence)

bob_ross_season_top10 <- bob_ross %>% 
  group_by(season) %>% 
  count(element, sort = TRUE) %>% 
  slice(1:10) %>% 
  ungroup() 

getPalette = colorRampPalette(brewer.pal(9, "Greens"))

bob_ross_season_top10 %>% 
  ggplot(aes(season, n, fill = element, label = paste(str_to_lower(element), n, sep = " "))) + 
  geom_col() + 
  geom_text(position = "stack", size = 3, hjust = 1) + 
  coord_flip() + 
  labs(title = "Bob Ross Top 10 word-element by Season",
       x = "Season",
       y = "Number of episodes mentioned",
       caption = "Data Source: 538") + 
  scale_fill_manual(values = getPalette(22)) + 
  theme_minimal() + 
  theme(legend.position = "none",
        text = element_text(family = "Maiandra GD"),
        plot.background = element_rect(fill='#fff7ec'),
        plot.title = element_text(color='black',
                                  size=20),
        strip.text = element_text(size = 9),
        strip.background = element_rect(color = "#efe3d2", fill = '#f7e9d7'),
        panel.background = element_rect(color = "#efe3d2", fill = '#fff7ec'))

