# tidy tuesday number 2
# (my first one)
# 2018-04-10 kac

library(tidyverse)
library(readxl)
library(skimr)

# read in data
dat <- read_excel("data/tidy_tuesday_week2.xlsx")
skim(dat)

# get it into long format
dat_long <- dat %>%
    gather(key = position, value = salary, -year) %>%
    mutate(salary_millions = salary/1000000) %>%
    group_by(position, year)

# violin plot to see all salaries
ggplot(dat_long, aes(x = year, y = salary_millions, col = position)) +
    geom_violin(size = 0.70, draw_quantiles = c(0.25, 0.5, 0.75)) +
    facet_wrap(~position, nrow = 3) +
    theme_bw() +
    theme(legend.position="none") +
    ggtitle("NFL salaries by position over time") +
    xlab("Year") +
    ylab("Salary, millions of $")

# subset to just the top 16 players in each position
dat_top <- dat %>%
    gather(key = position, value = salary, -year) %>%
    mutate(salary_millions = salary/1000000) %>%
    group_by(position, year) %>%
    top_n(16, wt = salary_millions) %>%
    ungroup()
    
# plot just the top ones, with a smoothing line
ggplot(dat_top, aes(x = year, y = salary_millions, col = position)) +
    geom_point(size = 2, alpha = 0.3) +
    geom_smooth(method = "loess", se = FALSE, size = 1.5) +
    facet_wrap(~position, nrow = 3) +
    theme_bw() +
    theme(legend.position="none") +
    ggtitle("Top 16 NFL salaries by position over time") +
    xlab("Year") +
    ylab("Salary, millions of $")
