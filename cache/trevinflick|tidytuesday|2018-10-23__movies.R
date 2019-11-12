## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(scales)
library(zoo)
library(ggthemes)


## ------------------------------------------------------------------------
# read in the data
movies <- read_csv("movie_profit.csv")

# drop column of numbers from 1-3401
movies <- movies %>%
  select(-one_of("X1"))

# convert column to date type
movies$release_date <- mdy(movies$release_date)

# create new columns for year, month, day
movies <- movies %>%
  separate(release_date, c("year", "month", "day"), "-", remove = FALSE)

# filter out years with less than 100 movies and movies that made money in the US
filter_movies <- movies %>%
  filter(year >= 1998 & year <= 2016)



## ------------------------------------------------------------------------
by_year <- movies %>%
  group_by(year) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

movies %>%
  ggplot(aes(year)) +
  geom_bar() +
  theme(text = element_text(size = 10),
        axis.text.x = element_text(angle = 90, hjust = 1))

movies %>%
  ggplot(aes(month)) +
  geom_bar() +
  theme(text = element_text(size = 10),
        axis.text.x = element_text(angle = 90, hjust = 1))



## ------------------------------------------------------------------------
filter_movies %>%
  ggplot(aes(genre, worldwide_gross)) +
  geom_boxplot() +
  coord_flip() +
  scale_y_continuous(labels = dollar_format())


## ------------------------------------------------------------------------
filter_movies %>%
  ggplot(aes(month, worldwide_gross)) +
  geom_boxplot() +
  coord_flip() +
  scale_y_continuous(labels = dollar_format())


## ------------------------------------------------------------------------
filter_movies %>%
  group_by(month, genre) %>%
  summarize(total_gross = sum(worldwide_gross)) %>%
  ggplot(aes(month, total_gross, fill = genre)) +
  geom_col(position = position_dodge()) +
  scale_fill_manual("legend", values = c("Action" = "#F0E442",
                                         "Adventure" = "#009E73",
                                         "Comedy" = "#56B4E9",
                                         "Drama" = "#E69F00",
                                         "Horror" = "#000000")) +
  scale_y_continuous(labels = dollar_format())


## ------------------------------------------------------------------------
by_distributor <- filter_movies %>%
  group_by(distributor) %>%
  summarize(count = n(),
            total = sum(worldwide_gross),
            avg = median(worldwide_gross)) %>%
  arrange(desc(total)) %>%
  head(10)


## ------------------------------------------------------------------------
movies_top_dis <- inner_join(filter_movies, by_distributor, by = "distributor")


## ------------------------------------------------------------------------
top_by_dis <- movies_top_dis %>%
  group_by(distributor) %>%
  top_n(1, worldwide_gross)


## ------------------------------------------------------------------------
movies_top_dis %>%
  mutate(distributor = fct_reorder(distributor, worldwide_gross)) %>%
  ggplot(aes(distributor, worldwide_gross, fill = distributor, position = 'dodge')) +
  geom_boxplot() +
  coord_flip() +
  scale_y_continuous(labels = dollar_format()) +
  expand_limits(y = c(0, 1600000000)) +
  geom_text(data = top_by_dis, aes(distributor, worldwide_gross, label = movie), 
            check_overlap = TRUE,
            position = position_dodge(width = 0.75),
            inherit.aes = TRUE,
            size = 2.5, 
            hjust = -0.25) +
  labs(x = "", y = "",
       title = "Top 10 Film Distributors by Total Worldwide Gross Revenue",
       subtitle = "(from 1998-2016)",
       caption = "TidyTuesday 10/23/18, source:fivethirtyeight") +
  theme_fivethirtyeight() +
  guides(fill=FALSE) +
  theme(text = element_text(size = 8),
        axis.text.x = element_text(angle = 30, hjust = 1))

ggsave("movies.png")


## ------------------------------------------------------------------------
knitr::knit_exit()


## ------------------------------------------------------------------------
filter_movies %>%
  ggplot(aes(worldwide_gross, production_budget)) +
  geom_point() +
  scale_y_log10()


## ------------------------------------------------------------------------
filter_movies %>%
  group_by(month, genre) %>%
  summarize(avg_gross = median(worldwide_gross)) %>%
  ggplot(aes(month, avg_gross, fill = genre)) +
  geom_col(position = position_dodge()) +
  scale_fill_manual("legend", values = c("Action" = "#F0E442",
                                         "Adventure" = "#009E73",
                                         "Comedy" = "#56B4E9",
                                         "Drama" = "#E69F00",
                                         "Horror" = "#000000")) +
  scale_y_continuous(labels = dollar_format())

