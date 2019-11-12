## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---- warning=FALSE, message=FALSE---------------------------------------
pacman::p_load(tidyverse, rvest, polite, scales, lubridate, extrafont)
loadfonts()


## ------------------------------------------------------------------------
jobs_gender <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/jobs_gender.csv")


## ------------------------------------------------------------------------
glimpse(jobs_gender)


## ------------------------------------------------------------------------
skimr::skim_to_wide(jobs_gender) %>% knitr::kable()


## ------------------------------------------------------------------------
jobs_gender %>% View()


## ------------------------------------------------------------------------
jobs_gender %>% 
  ggplot(aes(log(total_earnings_male), log(total_earnings_female), color = major_category)) +
  geom_point()

jobs_gender %>% 
  select(major_category, year, percent_female) %>% 
  mutate(percent_male = (100 - percent_female) %>% round(digits = 1)) %>% 
  rename(male = percent_male, female = percent_female) %>% 
  filter(year == 2016) %>% 
  group_by(major_category) %>% 
  mutate(diff = male - female) %>% 
  gather("gender", value = "percentage", -year, -major_category, - diff) %>% 
  group_by(major_category, gender) %>% 
  summarise(perc = mean(percentage),
            diff = mean(diff)) %>% 
  ungroup() %>% 
  mutate(major_category = major_category %>% as_factor() %>% fct_reorder(diff)) %>% 
  ggplot(aes(x = perc, y = major_category, color = gender)) +
  geom_point() + 
  geom_segment(aes(xend = perc, yend = major_category)) +
  theme_minimal()


## ------------------------------------------------------------------------
jobs_gender %>% 
  select(major_category, year, percent_female) %>% 
  mutate(percent_male = (100 - percent_female) %>% round(digits = 1)) %>% 
  rename(male = percent_male, female = percent_female) %>% 
  group_by(major_category) %>% 
  mutate(diff = male - female) %>% 
  gather("gender", value = "percentage", -year, -major_category, - diff) %>% 
  group_by(major_category, gender, year) %>% 
  summarise(perc = mean(percentage),
            diff = mean(diff)) %>% 
  ggplot(aes(x = year, y = diff, group = major_category, color = major_category)) +
  geom_line()

