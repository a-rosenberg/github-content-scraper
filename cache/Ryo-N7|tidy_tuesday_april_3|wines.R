## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------
pacman::p_load(tidyverse, scales, janitor, rvest, polite, glue)


## ------------------------------------------------------------------------
wine_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv")


## ------------------------------------------------------------------------
glimpse(wine_ratings)


## ------------------------------------------------------------------------
wine_ratings %>% 
  ggplot(aes(price, points)) +
  geom_point()

cor(wine_ratings %>% 
      select(-X1) %>% 
      select_if(is.numeric), use = "complete.obs") -> wine_cor


corrr::as_cordf()

corrplot::corrplot(wine_cor)


## ------------------------------------------------------------------------
wine_ratings %>% 
  select(-X1, -description, -taster_name, -taster_twitter_handle,
         -designation, -province, -title) %>% 
  map(~unique(.))




