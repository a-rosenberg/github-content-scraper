## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------
library(tidyverse)

theme_set(theme_minimal())

bird_collisions <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-30/bird_collisions.csv")
mp_light <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-30/mp_light.csv")


## ------------------------------------------------------------------------
glimpse(bird_collisions)


## ------------------------------------------------------------------------
summary(bird_collisions$date)


## ------------------------------------------------------------------------
mp_light %>%
  summary()


## ------------------------------------------------------------------------
mp_light_processed <- mp_light %>%
  group_by(date) %>%
  summarise(n = n()) %>%
  filter(n == 1) %>%
  select(date) %>%
  left_join(mp_light)


## ------------------------------------------------------------------------
mp_light_processed %>%
  mutate(weekday = lubridate::wday(date, label = TRUE)) %>%
  group_by(weekday) %>%
  summarize(pct_light_score = mean(light_score)/100) %>%
  ggplot(aes(weekday, pct_light_score, fill = weekday)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Weekday", 
       y = "Average % of window bays illuminated",
       title = "Thursday was the most illuminated day of the week",
       subtitle = "McCormick Place Between 3/6/2000 and 5/26/2018")


## ------------------------------------------------------------------------
mp_light_processed %>%
  mutate(month = lubridate::month(date, label = TRUE)) %>%
  group_by(month) %>%
  summarize(pct_light_score = mean(light_score)/100) %>%
  ggplot(aes(month, pct_light_score, fill = month)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Month", 
       y = "Average % of window bays illuminated",
       title = "September was the most illuminated month",
       subtitle = "McCormick Place Between 3/6/2000 and 5/26/2018")


## ------------------------------------------------------------------------
mp_bird_collisions <- bird_collisions %>%
  filter(locality == "MP") %>%
  left_join(mp_light_processed) %>%
  filter(date > "2000-03-05" & !is.na(light_score)) %>%
  mutate(light_score = light_score/100) %>%
  group_by(date, light_score) %>%
  summarise(n = n())

summary(mp_bird_collisions$date)


## ------------------------------------------------------------------------
mp_bird_collisions %>%
  ggplot(aes(date, light_score, color = n)) +
  geom_point(aes(size = n), alpha = 0.25) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Date", 
       y = "Average % of window bays illuminated",
       title = "More bird collisions occur when more windows are illuminated",
       subtitle = "McCormick Place Between 3/6/2000 and 5/26/2018")


## ------------------------------------------------------------------------
mp_bird_collisions %>%
  ggplot(aes(light_score, n)) +
  geom_point() +
  scale_x_continuous(labels = scales::percent_format()) +
  geom_smooth(method = "loess") +
  labs(x = "Average % of window bays illuminated", 
       y = "# of bird collisions",
       title = "More bird collisions occur when more windows are illuminated",
       subtitle = "McCormick Place Between 3/6/2000 and 5/26/2018")


## ------------------------------------------------------------------------
mod_pois <- glm(n ~ light_score, data = mp_bird_collisions, family = poisson)
summary(mod_pois)
pcount_pois <- colSums(pscl::predprob(mod_pois))[0:10]


## ------------------------------------------------------------------------
mod_negb <- MASS::glm.nb(n ~ light_score, data = mp_bird_collisions)
summary(mod_negb)


## ------------------------------------------------------------------------
ocount <- table(mp_bird_collisions$n)[0:10]
pcount_negb <- colSums(pscl::predprob(mod_negb))[0:10]
data.frame(ocount, pcount_pois, pcount_negb)

