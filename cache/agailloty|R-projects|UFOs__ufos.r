# The dataset url
ufo_sightings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv")

# Loading the packages
suppressPackageStartupMessages(library(tidyverse)) # It loads also ggplot2 and other useful packages
library(tidytext)
library(hrbrthemes) # For beautiful themes
library(lubridate) # FOr dealing with dates and times


# Text cleaning 
ufo <- ufo_sightings %>% 
select(date_time, state, country, description) %>%
unnest_tokens(output = word, input = description)
ufo <- ufo %>% anti_join(stop_words, by = "word") # remove stop words
ufo <- ufo %>%
filter(!str_detect(word, '[[:digit:]]+')) # remove numbers

# Most used words for describing UFOs
options(repr.plot.res = 400)
ufo2 %>%
count(word, sort = TRUE) %>%
  filter(n > 1000) %>% top_n(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
theme_ft_rc() +
  geom_col(fill = ft_cols$yellow) +
geom_text(aes(label=n), hjust = 1, col = "black") +
  xlab(NULL) + ylab(NULL) + labs(title = "Most used words for describing UFO's") +
  coord_flip()


ufo2 <- ufo %>%
mutate(date_time = dmy_hm(date_time),
      month = month(date_time),
      weekday = weekdays(date_time),
      year = year(date_time))
      
ufo_selected_years <- ufo2 %>%
filter(year %in% c(1950, 1980, 2000, 2010, 2012))

# Common words used to describe UFOs
options(repr.plot.res = 450, repr.plot.height = 7)
ufo_selected_years %>%
  count(year, word) %>%
  group_by(year) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(year, proportion) %>%
gather(year, proportion, `1950`:`2010`) %>%
ggplot(aes(x = proportion, y = `2012`, color = abs(`2012` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(name = "Proportion", 
                       limits = c(0, 0.001), low = "darkslategray4", high = ft_cols$yellow) +
  facet_wrap(~year, ncol = 2) +  theme_ft_rc() +
  theme(legend.position="bottom", legend.key.width = unit(15, "mm")) +
  labs(title = "Common words used to describe UFOs", x = NULL, y= NULL) 
  
# World map
  options(repr.plot.height = 6)
ufo_sightings %>%
ggplot(aes(x = longitude, y = latitude)) +
borders("world") + 
geom_point(alpha = 1/8, col = ft_cols$yellow, size = 1/2) + 
theme_ft_rc() + 
labs(title = "Where in the world UFOs are seen ?", 
     subtitle = "It seems that UFOs really love to appear in the United States",
    x = NULL, y = NULL) +
theme(axis.text = element_blank())
  
