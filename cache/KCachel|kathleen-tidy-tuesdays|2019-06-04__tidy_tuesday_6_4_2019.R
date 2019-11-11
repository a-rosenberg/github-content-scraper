library(tidyverse)
library(ggthemes)
library(ggdark)
library(LaCroixColoR)
ramen_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-04/ramen_ratings.csv")

summary(ramen_ratings)

#Find the top 20 producing countries 
ramen_topcountries <- ramen_ratings %>%
  group_by(country,style) %>%
  tally() %>%
  arrange(-n) %>%
  #head(20) %>%
  inner_join(ramen_ratings)

#Find average ratings per country:
country_average <- ramen_topcountries %>%
  group_by(country,style) %>%
  summarize(
    avg = mean(stars, na.rm = TRUE)
  ) %>%
  arrange(-avg)

topcountrieslist <- ramen_topcountries %>% 
  group_by(country) %>%
  summarize(
    avg = mean(stars, na.rm = TRUE)
  ) %>%
  arrange(-avg) %>%
  head(25) %>% 
  select(country)

country_averagestyle <- filter(country_average, style == "Bowl" |
                                 style == "Cup" |
                                 style == "Pack") %>%
  inner_join(topcountrieslist)

# Create palette
pal <- c("#803515", "#F0B630", "#E6442E")

###gplot
ggplot(country_averagestyle, aes(avg, country, fill = style)) +
  #geom_point(size = 4) +
  geom_bar(stat = "identity")
  dark_mode() +
  scale_fill_manual(values = pal, name = "Style:") +
    coord_flip()
  labs(x = "", y = "Country", title = "Average Ramen Rating by Country") +
  theme(legend.position = "top",
        axis.text.x=element_text(size=11, face = "bold", hjust = 1, color = "#666C1C"),
        axis.text.y = element_text(size = 10, color = "#666C1C"),
        axis.title.y = element_text(color = "#666C1C", face = "bold"),
        legend.title = element_text(color = "#666C1C", size = 12, face = "bold"),
        legend.text = element_text(color = "#666C1C", face = "bold"),
        plot.title = element_text(color = "#666C1C", face = "bold")) 



