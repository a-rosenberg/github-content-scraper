## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------
pacman::p_load(tidyverse, scales, janitor, rvest, polite, glue)


## ------------------------------------------------------------------------
student_ratio <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-07/student_teacher_ratio.csv")


student_ratio <- student_ratio %>% 
  select(-edulit_ind, -flag_codes, -flags)


str(student_ratio)
glimpse(student_ratio)
student_ratio %>% select(-student_ratio, -country_code) %>% map(~unique(.))
student_ratio %>% select(indicator) %>% unique()

student_ratio %>% 
  filter(indicator == "Primary Education") %>% 
  ggplot(aes(x = year, y = student_ratio, group = country)) +
  geom_point() +
  geom_line() +
  facet_wrap(~region)

