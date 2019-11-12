## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------
library(tidyverse)
library(janitor)
library(stringr)
library(lubridate)
library(ggthemes)


## ------------------------------------------------------------------------
restaurants <- read_csv("NYC_Restaurant_Inspections.csv")


## ------------------------------------------------------------------------
nyc_restaurants <- restaurants %>%
  janitor::clean_names() %>%
  select(-phone, -grade_date, -record_date, -building, -street) %>%
  filter(!is.na(action))

nyc_restaurants$inspection_date <- mdy(nyc_restaurants$inspection_date)


## ------------------------------------------------------------------------
nyc_restaurants %>%
  count(year(inspection_date))


## ------------------------------------------------------------------------
by_date <- nyc_restaurants %>%
  count(camis, inspection_date) %>%
  group_by(camis)

by_date <- by_date[order(by_date$inspection_date, by_date$n),]


## ------------------------------------------------------------------------
n_visits <- by_date %>% count(camis, sort = TRUE)

grades <- nyc_restaurants %>%
  select(camis, inspection_date, grade, cuisine_description) %>%
  filter(!is.na(grade))




## ------------------------------------------------------------------------
by_date_grade <- left_join(by_date, grades, by = c("camis" = "camis", "inspection_date" = "inspection_date"))

by_date_grade <- filter(by_date_grade, !is.na(grade))

by_date_grade <- unique(by_date_grade)


## ------------------------------------------------------------------------
grade_2017 <- by_date_grade %>% 
  group_by(camis) %>% 
  filter(year(inspection_date) == 2017) %>%
  slice(which.max(inspection_date)) %>%
  data.frame()


## ------------------------------------------------------------------------
grade_2017 %>% count(cuisine_description, sort = TRUE) %>% View()


## ------------------------------------------------------------------------
grade_2017 <- grade_2017 %>%
  mutate(cuisine_description = fct_lump(cuisine_description, n = 19))

grade_2017 <- grade_2017 %>%
  mutate(grade = fct_lump(grade, n = 3))


## ------------------------------------------------------------------------
grade_2017 <- grade_2017 %>%
  mutate(cuisine_description = str_replace(cuisine_description, "CafÃ©/Coffee/Tea", "Coffee"))

grade_2017 <- grade_2017 %>%
  mutate(cuisine_description = str_replace(cuisine_description, "Latin \\(Cuban, Dominican, Puerto Rican, South & Central American\\)", 
                                           "Latin"))

grade_2017 <- grade_2017 %>%
  mutate(cuisine_description = str_replace(cuisine_description, "Juice, Smoothies, Fruit Salads", "Smoothies"))


## ------------------------------------------------------------------------
grade_2017 <- transform(grade_2017, cuisine_description = factor(cuisine_description,
                                                                 levels = c("American",
                                                                 "Other",
                                                                 "Chinese",
                                                                 "Coffee",
                                                                 "Pizza",
                                                                 "Italian",
                                                                 "Mexican",
                                                                 "Latin",
                                                                 "Japanese",
                                                                 "Bakery",
                                                                 "Caribbean",
                                                                 "Donuts",
                                                                 "Spanish",
                                                                 "Pizza/Italian",
                                                                 "Chicken",
                                                                 "Hamburgers",
                                                                 "Sandwiches",
                                                                 "Smoothies",
                                                                 "Asian",
                                                                 "Jewish/Kosher")))


## ------------------------------------------------------------------------
grade_2017 %>%
  ggplot(aes(cuisine_description, fill=grade)) + geom_bar() + coord_flip() + scale_y_log10() +
  labs(x="", y="", title="2017 NYC Restaurant Health Grades by Cuisine",
       subtitle="x-axis count of restaurants on log-scale",
       caption = "TidyTuesday 12/11/18, source:fivethirtyeight") +
  theme_fivethirtyeight()

ggsave("nyc_grades.png")


## ------------------------------------------------------------------------
by_date_grade %>% 
  filter(year(inspection_date) == 2017 & grade == "B") %>%
  ggplot(aes(inspection_date, n, group = camis)) + geom_step()


## ------------------------------------------------------------------------
by_date_grade %>%
  filter(grade == "A" | grade == "B" | grade == "C") %>%
  ggplot(aes(n, stat(count), fill=grade)) + geom_density(alpha = 0.6)

