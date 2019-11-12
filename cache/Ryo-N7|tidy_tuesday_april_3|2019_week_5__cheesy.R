## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------
pacman::p_load(tidyverse, scales)


## ------------------------------------------------------------------------
clean_cheese_raw <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-29/clean_cheese.csv")

glimpse(clean_cheese_raw)

clean_cheese_df <- clean_cheese_raw %>% 
  select(Year, contains("Total"))

clean_cheese_df %>% 
  gather(key = "cheese", value = "value", -Year) %>% 
  ggplot(aes(Year, value, color = cheese)) +
  geom_point()

