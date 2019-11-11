# ===== Introduction =====
# Project: Tidy Tuesday - Week 1 - Average Tuition
# By: Matthew Hendrickson
# Twitter: @mjhendrickson
# GitHub: https://github.com/mjhendrickson/Tidy-Tuesday
# Original source link: https://onlinembapage.com/average-tuition-and-educational-attainment-in-the-united-states/

# ===== Install needed packages =====
install.packages("tidyverse")
install.packages("readxl")
install.packages("scales")
install.packages("viridis")
install.packages("here")
#install.packages("RColorBrewer")


# ===== Load needed packages =====
library(tidyverse)
library(readxl)
library(scales)
library(viridis)
library(here)
#library(RColorBrewer)


# ===== Load & review data =====
average_tuition <- read_excel(here("Week 01 - US Tuition", "us_avg_tuition.xlsx"))
View(average_tuition)
glimpse(average_tuition)


# ===== Data prep =====
average_tuition <- average_tuition %>% # append new fields to df
  rename(state = State) %>%  # adhere to tidy naming convention
  mutate(state_abb = factor(state.abb))  # state abbreviation

avg_tuition <- average_tuition %>% # create new df
  gather(year, tuition, `2004-05`:`2015-16`) # reshape data wide to long

avg_tuit <- average_tuition %>% # append new fields to df
  mutate(tuition_5yr_chg = `2015-16` - `2010-11`) %>% # 5 year tuition change
  mutate(tuition_5yr_pct_chg = (`2015-16` - `2010-11`) / `2010-11` * 100) %>% # 5 year % change
  gather(year, tuition, `2004-05`:`2015-16`) %>% # reshape data wide to long
  select(state, state_abb, year, tuition, tuition_5yr_chg, tuition_5yr_pct_chg) %>% # select needed fields
  filter(year == "2015-16")  # keep only last year

# Create bins for % change
avg_tuit$tuition_5yr_pct_bin <- cut(avg_tuit$tuition_5yr_pct_chg,
                            breaks = c(-10, 0, 10, 20, 30, 40, 50, 60), 
                            right = FALSE,
                            include.highest = TRUE,
                            include.lowest = TRUE)

# Check bin counts
avg_tuit %>%
  group_by(tuition_5yr_pct_bin) %>%
  summarize(n = n())


# ===== Create plots =====
# Average Tuition - Box
avg_tuition %>% 
  ggplot(aes(x = fct_reorder(state, tuition), 
             y = tuition)) +
  geom_boxplot() +
  #coord_flip() +
  scale_y_continuous(labels = dollar) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "State", 
       y = "Tuition Range",
       title = "College Tuition Ranges by State - Academic Years 2004-2005 - 2015-2016",
       caption = "\nDataSource: https://trends.collegeboard.org/ | Graphic: @mjhendrickson")


# Average Tuition - Point
avg_tuition %>% 
  ggplot(aes(x = fct_reorder(state, tuition), 
             y = tuition, color = year)) +
  geom_point() +
  #coord_flip() +
  scale_y_continuous(labels = dollar) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "State", 
       y = "Tuition",
       color = "Academic Year", 
       title = "College Tuition by State",
       caption = "\nDataSource: https://trends.collegeboard.org/ | Graphic: @mjhendrickson")


# Average Tuition & 5 Year Change - Point
avg_tuit %>% 
  ggplot(aes(x = fct_reorder(state, tuition), 
             y = tuition, 
             color = tuition_5yr_pct_bin,
             shape = tuition_5yr_pct_bin)) +
  geom_point() +
  #coord_flip() +
  scale_y_continuous(labels = dollar) +
  #scale_color_viridis() +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5)) +
  labs(x = "State", 
       y = "Average Tuition",
       size = "5 yr change", 
       color = "", 
       title = "Average College Tuition by State - Academic Year 2015-2016",
       caption = "\nDataSource: https://trends.collegeboard.org/ | Graphic: @mjhendrickson")

# Plot just the outliers
avg_tuit %>% 
  subset(tuition_5yr_pct_chg < 0 | tuition_5yr_pct_chg > 30) %>%
  ggplot(aes(x = fct_reorder(state, tuition), 
             y = tuition, 
             color = tuition_5yr_pct_bin,
             shape = tuition_5yr_pct_bin)) +
  geom_point() +
  #coord_flip() +
  scale_y_continuous(labels = dollar) +
  #scale_color_viridis() +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5)) +
  labs(x = "State", 
       y = "Average Tuition",
       size = "5 yr change", 
       color = "", 
       title = "Average College Tuition by State - Academic Year 2015-2016",
       caption = "\nDataSource: https://trends.collegeboard.org/ | Graphic: @mjhendrickson")


# Average Tuition & 5 Year Change - Bar
# Jake Kaupp's updated graph.
avg_tuit %>% 
  ggplot(aes(x = fct_reorder(state, tuition), 
             y = tuition, 
             fill = tuition_5yr_pct_chg)) +
  geom_bar(stat = "Identity") +
  geom_text(aes(label = dollar(round(tuition)),
                angle = 0),
            hjust = 1.1, 
            color = "white",
            size = 3) +
  coord_flip() +
  scale_y_continuous(labels = dollar, 
                     expand = c(0, 0)) +
  scale_fill_viridis() +
  theme_minimal() +
  labs(x = NULL, 
       y = "Average Tuition",
       fill = "5 yr % change", 
       title = "Average College Tuition by State - Academic Year 2015-2016",
       caption = "\nDataSource: https://trends.collegeboard.org/ | Graphic: @mjhendrickson")

# Second take to appear closer to original, incorporating feedback
avg_tuit %>% 
  ggplot(aes(x = fct_reorder(state, tuition), 
             y = tuition, 
             fill = tuition_5yr_pct_chg)) +
  geom_bar(stat = "Identity") +
  geom_text(aes(label = dollar(round(tuition)),
                angle = 90),
            hjust = 1.1, 
            color = "white",
            size = 3) +
  #coord_flip() +
  scale_y_continuous(labels = dollar, 
                     expand = c(0, 0)) +
  scale_fill_viridis() +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5)) +
  labs(x = NULL, 
       y = "Average Tuition",
       fill = "5 yr % change", 
       title = "Average College Tuition by State - Academic Year 2015-2016",
       caption = "\nDataSource: https://trends.collegeboard.org/ | Graphic: @mjhendrickson")
