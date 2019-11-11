# tidytuesday week 3
# 4-17-2018 kac

library(tidyverse)
library(readxl)
library(skimr)

dat <- read_excel("data/global_mortality.xlsx")

# make it long for grouping purposes
dat_long <- dat %>%
    gather(key = cause, value = pct_contribution, -country, -country_code, -year) %>%
    mutate(year = as.numeric(year)) 

dat_long %>%
    filter(country_code == 'USA') %>%
    ggplot(aes(x = year, y = pct_contribution)) +
        geom_point() +
    geom_line() +
    facet_wrap(~cause, scales = "free_y")
    

# things I want to do:
# get rid of (%) at the end of the cause names
# lump smaller mortality causes together into an "other" category
# select a given country in the plotting command