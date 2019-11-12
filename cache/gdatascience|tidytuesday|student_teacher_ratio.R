## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------
library(tidyverse)

theme_set(theme_minimal())

student_ratio <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-07/student_teacher_ratio.csv")


## ------------------------------------------------------------------------
summary(student_ratio)


## ------------------------------------------------------------------------
library(WDI)

gdp_per_capita <- WDI(indicator = "NY.GDP.PCAP.CD", start = 2012, end = 2018, 
                      extra = TRUE) %>%
  tbl_df() %>%
  transmute(year, country_code = as.character(iso3c), 
           gdp_per_capita = NY.GDP.PCAP.CD)

student_ratio_GDP <- student_ratio %>%
  inner_join(gdp_per_capita, by = c("country_code", "year")) %>%
  filter(!is.na(student_ratio))


## ------------------------------------------------------------------------
student_ratio_GDP %>%
  ggplot(aes(student_ratio)) + 
  geom_histogram() + 
  scale_x_log10()


## ------------------------------------------------------------------------
student_ratio_GDP %>%
  ggplot(aes(gdp_per_capita, student_ratio, color = indicator)) +
  geom_point() + 
  scale_x_log10() + 
  scale_y_log10() + 
  geom_text(aes(label = paste(year, country, sep = ":")), vjust = 1, hjust = 1, check_overlap = TRUE) +
  labs(x = "GDP per capita",
       y = "Student/teacher ratio",
       title = "GDP per capita and student/teacher ratio are negatively correlated",
       subtitle = "2012 to 2018")

