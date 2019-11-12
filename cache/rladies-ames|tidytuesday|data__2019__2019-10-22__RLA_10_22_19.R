## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)


## ------------------------------------------------------------------------
d <- read.csv('horror_movies.csv')


## ------------------------------------------------------------------------
summary(d)


## ------------------------------------------------------------------------
d %>% filter(language == "Japanese") %>% 
  ggplot(aes(review_rating)) +
  geom_histogram(binwidth=1)


## ------------------------------------------------------------------------
d2 <- separate(d,language, into = c("language1","language2"), extra = "merge")


## ------------------------------------------------------------------------
rating <- d2 %>%
  group_by(language1) %>%
  summarize(n=n(),
            mean_rating = mean(review_rating))
rating


## ------------------------------------------------------------------------
d2 %>% filter(language1=="English") %>%
  select(review_rating)


## ------------------------------------------------------------------------
rating %>% 
  ggplot(aes(language1,mean_rating,fill=mean_rating)) +
  geom_bar(stat="identity",position="dodge") + 
  coord_flip()

