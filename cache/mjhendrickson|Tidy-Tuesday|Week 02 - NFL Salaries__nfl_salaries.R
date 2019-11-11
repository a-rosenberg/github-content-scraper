# ===== Introduction =====
# Project: Tidy Tuesday - Week 2 - NFL Salaries
# By: Matthew Hendrickson
# Twitter: @mjhendrickson
# GitHub: https://github.com/mjhendrickson/Tidy-Tuesday
# Original source links: http://www.spotrac.com/rankings/
  # https://fivethirtyeight.com/features/running-backs-are-finally-getting-paid-what-theyre-worth/

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
nfl_salaries <- read_excel(here("Week 02 - NFL Salaries", "nfl_salaries.xlsx"))
View(nfl_salaries)
glimpse(nfl_salaries)


# ===== Data prep =====
nfl <- nfl_salaries %>% # create new df
  gather(position, salary, 'Cornerback':'Wide Receiver') # reshape data wide to long

nfl %>% 
  mutate(salary = salary/10^6) # salary in millions

# ===== Create plots =====
nfl %>% 
  subset(!is.na(salary)) %>% # remove missing salaries
  mutate(salary = salary/10^6) %>% # salary in millions# remove cases with no salary
ggplot() +
  geom_line(mapping = aes(x = year, y = salary)) +
  facet_wrap(~position, nrow = 2) +
  scale_x_discrete() +
  scale_y_continuous(labels = dollar) +
  #theme(axis.title.x = element_blank()) +
  labs(x = NULL, 
       y = "Salary ($M)",
       #fill = "5 yr % change", 
       title = "NFL Salary by Position 2011-2018",
       caption = "\nDataSource: http://www.spotrac.com/rankings/ | Graphic: @mjhendrickson")
