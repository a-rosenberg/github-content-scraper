## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------
library(tidyverse)
theme_set(theme_light())

media_franchises <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-02/media_franchises.csv") %>%
  distinct()


## ------------------------------------------------------------------------
media_franchises %>%
  glimpse()


## ------------------------------------------------------------------------
top10_media_franchises <- media_franchises %>%
  group_by(franchise) %>%
  summarise(total_revenue = sum(revenue)) %>%
  top_n(10) %>%
  mutate(franchise = fct_reorder(franchise, total_revenue)) 

top10_media_franchises %>%
  ggplot(aes(franchise, total_revenue, fill = franchise)) +
  geom_col(show.legend = FALSE) + 
  scale_y_continuous(labels = scales::dollar_format()) +
  coord_flip() + 
  labs(x = "Franchise",
       y = "Revenue (in billions)",
       title = "Top 10 media franchises by revenue",
       caption = "Designer: Tony Galvan @gdatascience1  |  Source: Wikipedia")


## ------------------------------------------------------------------------
media_franchises %>%
  group_by(revenue_category) %>%
  summarise(total_revenue = sum(revenue)) %>%
  mutate(revenue_category = fct_reorder(revenue_category, total_revenue)) %>%
  ggplot(aes(revenue_category, total_revenue, fill = revenue_category)) +
  geom_col(show.legend = FALSE) + 
  scale_y_continuous(labels = scales::dollar_format()) +
  coord_flip() + 
  labs(x = "Category",
       y = "Revenue (in billions)",
       title = "Revenue by category",
       subtitle = "For media franchises",
       caption = "Designer: Tony Galvan @gdatascience1  |  Source: Wikipedia")


## ------------------------------------------------------------------------
top10_media_franchises %>%
  inner_join(media_franchises) %>%
  mutate(franchise = fct_reorder(franchise, total_revenue))  %>%
  ggplot(aes(franchise, revenue, fill = revenue_category)) +
  geom_col() + 
  scale_y_continuous(labels = scales::dollar_format()) +
  coord_flip() + 
  labs(x = "Franchise",
       y = "Revenue (in billions)",
       fill = "Category",
       title = "Most revenue comes from merchandise, licensing & retail",
       subtitle = "For 7 of top 10 media franchises by total revenue",
       caption = "Designer: Tony Galvan @gdatascience1  |  Source: Wikipedia")

