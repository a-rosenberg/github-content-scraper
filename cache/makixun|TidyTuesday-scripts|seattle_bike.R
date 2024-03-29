## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(tibble)
library(lubridate)
library(forcats)
library(scales)

seattle_bike <- as_tibble(read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-02/bike_traffic.csv")) 

seattle_bike <- seattle_bike %>%
  mutate(date = mdy_hms(date), time = chron::times(strftime(date,"%H:%M:%S", tz = "UTC")), date = date(date)) %>% subset(select = c(date, time, crossing, direction, bike_count, ped_count))
  
seattle_bike



## ------------------------------------------------------------------------

seattle_bike %>%
  filter(!is.na(bike_count)) %>%
  count(crossing, direction, bike_count, sort = TRUE) %>% 
  group_by(crossing) %>%
  summarise(total = sum(bike_count)) %>%
  ungroup() %>%
  mutate(crossing = fct_reorder(crossing, total)) %>%
  ggplot(aes(crossing, total)) +
  geom_col(fill = "blue") + 
  coord_flip() +
  labs(title = "Number of bikes per crossing")

seattle_bike %>%
  filter(bike_count != 0) %>%
  count(time, bike_count, sort = TRUE) %>%
  group_by(time) %>%
  summarise(total = sum(bike_count)) %>%
  ungroup() %>%
  ggplot(aes(time, total)) +
  geom_line() 




