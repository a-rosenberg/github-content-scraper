# ===== Introduction =====
# Project: Tidy Tuesday - Week 29 - Major and Income
# By: Matthew Hendrickson
# Twitter: @mjhendrickson
# GitHub: https://github.com/mjhendrickson/Tidy-Tuesday
# Article: https://fivethirtyeight.com/features/the-economic-guide-to-picking-a-college-major/
# Data Source: https://github.com/fivethirtyeight/data/tree/master/college-majors
# TidyTuesday: https://github.com/rfordatascience/tidytuesday/tree/master/data/2018-10-16

# ===== Load needed packages =====
library(tidyverse)
library(readr)
library(scales)
library(viridis)
library(DataExplorer)

# ===== Load & review data =====
major_income <- read_csv("major_income.csv")
View(major_income)
glimpse(major_income)


# ===== Explore data with DataExplorer =====
create_report(major_income)


# ===== Data prep =====
major_income$Gender_lean <- 
  ifelse(major_income$ShareWomen >= .5,
                        "More Female", 
                        "More Male")

major_income$Employed_size <- 
  ifelse(major_income$Employed > 250000, "> 250,000",
  ifelse(major_income$Employed > 200000, "200,000 - 250,000",
  ifelse(major_income$Employed > 150000, "150,000 - 200,000",
  ifelse(major_income$Employed > 100000, "100,000 - 150,000",
  ifelse(major_income$Employed >  50000, "50,000 - 150,000",
         "< 50,000"
         )))))


# ===== Create plots =====
major_income %>% 
  filter(Employed >= 50000) %>% 
  ggplot(aes(x = fct_reorder(Major, Employed), 
             y = Employed, 
             fill = ShareWomen)) +
  geom_bar(stat = "Identity") +
  geom_text(aes(label = dollar(round(Median))),
                #angle = 90),
            hjust = 1.1, 
            color = "white",
            size = 3) +
  coord_flip() +
  scale_fill_viridis() +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(#angle = 90, 
                                   vjust = 0.5)) +
  labs(x = "Major", 
       y = "Number Employed",
       fill = "% Female", 
       title = "Share of Employed by Major",
       caption = "\nDataSource: https://github.com/fivethirtyeight/data/tree/master/college-majors   |   Graphic: @mjhendrickson")


major_income %>% 
  filter(Employed >= 50000) %>% 
  ggplot(aes(x = fct_reorder(Major, Employed), 
             y = Employed, 
             fill = Unemployment_rate)) +
  geom_bar(stat = "Identity") +
  geom_text(aes(label = percent(round(Unemployment_rate,4))),
            #angle = 90),
            hjust = 1.1, 
            color = "white",
            size = 3) +
  facet_grid(. ~ Gender_lean) +
  coord_flip() +
  scale_fill_viridis() +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(#angle = 90, 
    vjust = 0.5)) +
  labs(x = "Major", 
       y = "Number Employed",
       fill = "Unemployment Rate", 
       title = "Share of Employed by Major",
       caption = "\nDataSource: https://github.com/fivethirtyeight/data/tree/master/college-majors   |   Graphic: @mjhendrickson")
