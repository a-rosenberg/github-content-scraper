## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
theme_set(theme_light())

video_games <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-30/video_games.csv") %>%
  mutate(game = str_to_title(game),
         release_date = str_replace(release_date, "8 Oct, 2014", "Oct 8, 2014"),
         release_date = mdy(release_date),
         release_year = year(release_date),
         release_month = month(release_date, label = TRUE),
         owners = as.numeric(gsub(",","",str_extract(owners,"[0-9]+(,[0-9]+)*"))),
         owners = if_else(owners == 0, 1, owners),
         revenue = owners * price) %>%
  select(-number)


## ------------------------------------------------------------------------
video_games %>%
  arrange(desc(owners)) %>%
  select(game, release_date, price, owners, revenue) %>%
  top_n(7, owners)


## ------------------------------------------------------------------------
paste0(sum(is.na(video_games$price)), " out of ", nrow(video_games), " = ", round(100*sum(is.na(video_games$price))/nrow(video_games), 2), "%")


## ------------------------------------------------------------------------
video_games %>%
  filter(is.na(price)) %>%
  group_by(release_year) %>%
  summarise(n = n()) %>%
  ggplot(aes(release_year, n)) + 
  geom_col()


## ------------------------------------------------------------------------
video_games %>%
  ggplot(aes(release_date, price)) + 
  geom_point(alpha = 0.25) + 
  geom_smooth(method = "lm")


## ------------------------------------------------------------------------
video_games %>%
  group_by(release_year) %>%
  summarise(avg_price = mean(price, na.rm = TRUE)) %>%
  ggplot(aes(release_year, avg_price)) + 
  geom_col() +
  geom_smooth(method = "lm")


## ------------------------------------------------------------------------
avg_video_game_revenue <- mean(video_games$revenue, na.rm = TRUE)

video_games %>%
  group_by(release_year, release_month) %>%
  summarise(avg_revenue = mean(revenue, na.rm = TRUE)) %>%
  ggplot(aes(release_month, avg_revenue, fill = as.factor(release_month))) + 
  geom_col() + 
  facet_wrap(~release_year, scales = "free_y") +
  geom_hline(yintercept = avg_video_game_revenue, linetype = 2)


## ------------------------------------------------------------------------
video_games %>%
  arrange(desc(revenue)) %>%
  select(game, release_date, price, owners, revenue)


## ------------------------------------------------------------------------
video_games %>%
  ggplot(aes(release_date, revenue)) + 
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm")


## ------------------------------------------------------------------------
video_games %>%
  group_by(release_year) %>%
  summarise(total_revenue = sum(revenue, na.rm = TRUE)) %>%
  ggplot(aes(release_year, total_revenue, fill = as.factor(release_year))) + 
  geom_col(show.legend = FALSE) +
  scale_y_continuous(labels = scales::dollar_format(scale = 0.000000001, suffix = "B")) + 
  labs(x = "Release year",
       y = "Total revenue (in billions)",
       title = "2017 PC video games generated over $4 billion",
       subtitle = "Based on price multiplied by the number of owners",
       caption = "Designer: Tony Galvan @gdatascience1  |  Source: Steam Spy")


## ------------------------------------------------------------------------
video_games %>%
  tidytext::unnest_tokens(tbl = ., output = word, input = game) %>%
  count(word, sort = TRUE) %>%
  filter(is.na(as.numeric(word))) %>%
  anti_join(get_stopwords()) %>%
  filter(n > 100) %>%
  na.omit() %>%
  wordcloud2::wordcloud2(shape = "cardiod")

