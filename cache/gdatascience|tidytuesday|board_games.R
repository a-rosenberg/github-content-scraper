## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----ttsetup, echo=FALSE, warning=FALSE, message=FALSE-------------------
# Load libraries, set the default theme & caption, and grab the data
library(tidyverse)
theme_set(theme_light())

default_caption <- "Source: Board Game Geeks  |  Designer: Tony Galvan @gdatascience1"

board_games <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-12/board_games.csv")

glimpse(board_games)


## ------------------------------------------------------------------------
board_games %>%
  mutate(log_playtime = log(max_playtime+1)) %>%
  select(average_rating, log_playtime, min_age, min_players, 
         year_published, users_rated) %>%
  gather(variable, value, -average_rating) %>%
  ggplot(aes(average_rating, value)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~variable, scales = "free")


## ------------------------------------------------------------------------
board_games %>%
  top_n(10, wt = average_rating) %>%
  mutate(name = fct_reorder(name, average_rating)) %>%
  ggplot(aes(name, average_rating, fill = name)) + 
  geom_col(show.legend = FALSE) +
  coord_flip()


## ------------------------------------------------------------------------
board_games %>%
  ggplot(aes(year_published, average_rating)) +
  geom_point(alpha = 0.025) + 
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "",
       y = "Average user rating",
       title = "A Golden Age Of Board Games?",
       subtitle = "Average user ratings for games by original year of production",
       caption = "Source: Board Game Geeks  |  Inspired by: FiveThirtyEight")


## ------------------------------------------------------------------------
categories <- board_games %>%
  unnest(category = strsplit(category, ","))

glimpse(categories)


## ------------------------------------------------------------------------
categories %>%
  mutate(total_rating = average_rating * users_rated) %>%
  group_by(category) %>%
  summarise(n = n(),
            category_user_ratings = sum(users_rated),
            category_rating = sum(total_rating),
            category_avg_rating = category_rating / category_user_ratings) %>%
  ungroup() %>%
  top_n(10, wt = category_avg_rating) %>%
  mutate(category = fct_reorder(category, category_avg_rating)) %>%
  ggplot(aes(category, category_avg_rating, fill = category)) + 
  geom_col(show.legend = FALSE) + 
  coord_flip()


## ------------------------------------------------------------------------
library(tidytext)

board_games %>%
  unnest_tokens(tbl = ., output = word, input = description) %>%
  count(word, sort = TRUE) %>%
  mutate(word = str_to_lower(word)) %>%
  filter(is.na(as.numeric(word)) & !word %in% c("game", "player", "players")) %>%
  anti_join(get_stopwords()) %>%
  filter(n > 1000) %>%
  na.omit() %>%
  wordcloud2::wordcloud2(shape = "cardiod")


## ------------------------------------------------------------------------
library(sentimentr)

game_sentiment <- board_games %>%
  pull(description) %>%
  get_sentences() %>%
  sentiment_by()

glimpse(game_sentiment)


## ------------------------------------------------------------------------
board_games %>%
  inner_join(game_sentiment, by = c("game_id" = "element_id")) %>%
  ggplot(aes(average_rating, ave_sentiment)) +
  geom_point(alpha = 0.05) + 
  geom_smooth(method = "lm", se = FALSE)


## ------------------------------------------------------------------------
#split the data into test/training sets
n <- nrow(board_games)
set.seed(1874)
test_index <- sample.int(n,size=round(0.2*n))
games_train <- board_games[test_index,]
games_test <- board_games[-test_index,]

lmod1 <- lm(average_rating ~ max_players + log(max_playtime+1) + min_age + 
       min_players + log(min_playtime+1) + year_published + users_rated, 
     data = games_train)

summary(lmod1)


## ------------------------------------------------------------------------
lmod2 <- MASS::stepAIC(lmod1)
summary(lmod2)


## ------------------------------------------------------------------------
summary(cooks.distance(lmod2))


## ------------------------------------------------------------------------
ggplot(data = lmod2, aes(lmod2$fitted.values, lmod2$residuals)) + 
  geom_point() +
  scale_x_continuous(limits = c(0,10)) + 
  scale_y_continuous(limits = c(-5,5))


## ------------------------------------------------------------------------
games_test %>%
  mutate(pred = predict(lmod2, new = .),
         error = abs(pred - average_rating)) %>%
  ggplot(aes(average_rating, pred, color = error)) +
  geom_point(alpha = 0.75) + 
  geom_abline(slope = 1, intercept = 0, color = "darkblue", linetype = 2) +
  coord_equal() + 
  scale_color_gradient2(low = "darkorange", mid = "black", high = "black", 
                        midpoint = 2.5) + 
  theme(legend.position = c(0.3, 0.8),
        legend.direction = "horizontal") +
  labs(x = "Actual Rating",
       y = "Predicted Rating",
       color = "Error",
       title = "Predicting board game ratings: model performance",
       subtitle = "Model is based on minimum players, minimum age, number of user ratings, \nyear published, and log of maximum play time",
       caption = default_caption)

#ggsave("board_games.png", height = 5.25, width = 5.25)

