## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(tidytext)
library(sentimentr)
theme_set(theme_light())

ufo_sightings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv") %>%
  rownames_to_column(var = "id") %>%
  mutate(id = as.numeric(id),
         date = mdy(date_documented),
         year = year(date))


## ------------------------------------------------------------------------
summary(ufo_sightings$date)


## ------------------------------------------------------------------------
ufo_sightings %>%
  mutate(ufo_shape = if_else(is.na(ufo_shape), "unknown", ufo_shape),
         ufo_shape = fct_lump(ufo_shape, 10, other_level = "other")) %>%
  group_by(ufo_shape) %>%
  summarise(n = n()) %>%
  mutate(ufo_shape = fct_reorder(ufo_shape, n)) %>%
  ggplot(aes(ufo_shape, n, fill = ufo_shape)) +
  geom_col(show.legend = FALSE) + 
  coord_flip() + 
  labs(x = "UFO shape",
       y = "# of sightings",
       title = "Top 10 UFO Shapes",
       caption = "Designer: Tony Galvan @gdatascience1  |  Source: NUFORC")


## ------------------------------------------------------------------------
ufo_sightings %>%
  unnest_tokens(tbl = ., output = word, input = description) %>%
  count(word, sort = TRUE) %>%
  filter(is.na(as.numeric(word))) %>%
  anti_join(get_stopwords()) %>%
  filter(n > 2000) %>%
  na.omit() %>%
  wordcloud2::wordcloud2(shape = "cardiod")


## ------------------------------------------------------------------------
ufo_sentences <- ufo_sightings %>%
  pull(description) %>%
  get_sentences()


## ------------------------------------------------------------------------
ufo_sentiment <- sentiment_by(ufo_sentences)


## ------------------------------------------------------------------------
ufo_sightings %>%
  inner_join(ufo_sentiment, by = c("id" = "element_id")) %>%
  group_by(country) %>%
  summarise(avg_sentiment = mean(ave_sentiment)) %>%
  mutate(country = fct_reorder(country, avg_sentiment)) %>%
  ggplot(aes(country, avg_sentiment, fill = country)) +
  geom_col(show.legend = FALSE) +
  coord_flip()


## ------------------------------------------------------------------------
ufo_sightings %>%
  inner_join(ufo_sentiment, by = c("id" = "element_id")) %>%
  filter(country == "us") %>%
  group_by(state) %>%
  summarise(avg_sentiment = mean(ave_sentiment)) %>%
  mutate(state = fct_reorder(state, avg_sentiment)) %>%
  ggplot(aes(state, avg_sentiment, fill = state)) +
  geom_col(show.legend = FALSE) +
  coord_flip()


## ------------------------------------------------------------------------
ufo_sightings %>%
  filter(country == "us" & state != "hi" & state != "ak" & state != "pr") %>%
  ggplot() + 
  geom_map(data = map_data("state"), 
           map = map_data("state"), 
           aes(long, lat, map_id = region, group = group),
           fill = "white", color = "black", size = 0.1) + 
  geom_point(aes(longitude, latitude), 
             size = 0.75, alpha = 0.25, color = "blue") +
  theme_void() + 
  coord_map() +
  gganimate::transition_states(year) +
  #facet_wrap(~year) + 
  labs(title = "US UFO Sightings in {closest_state}",
       caption = "Designer: Tony Galvan @gdatascience1  |  Source: NUFORC")


## ---- eval=FALSE, echo=FALSE---------------------------------------------
## gganimate::anim_save(filename = "us_ufo_sightings.gif", animation = gganimate::last_animation())

