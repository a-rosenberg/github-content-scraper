## file change for git purposes

# week 8
library(tidyverse)

dat <- read.csv("data/week8_honeyproduction.csv")
head(dat)

# which states were the top 10 producers in each year?
topbyyr <- dat %>%
    group_by(year) %>%
    top_n(10, totalprod) %>%
    ungroup()
n_distinct(topbyyr$year)
n_distinct(topbyyr$state)

# which states show up the most?
topstates <- topbyyr %>%
    group_by(state) %>%
    summarize(years = n_distinct(year)) %>%
    arrange(desc(years), state)

# seven states show up in the top 10 in all 15 years, and WI shows up in 14 of them. So I'll look at 8 states:
# CA, FL, MN, MT, ND, SD, TX, WI

statestouse <- topstates %>%
    filter(years >= 14) %>%
    select(state)

dat_top <- dat %>%
    filter(state %in% statestouse$state)

# what is price per lb by year in each of those states?

ggplot(dat_top, aes(x = year, y = priceperlb, color = state)) +
    geom_point() +
    geom_line(size = 1) +
    facet_wrap(~state, ncol = 1) +
    theme_bw()

# or don't facet
ggplot(dat_top, aes(x = year, y = priceperlb, color = state)) +
    geom_point() +
    geom_line(size = 1) +
    theme_bw()
## since the mid-2000s, WI seems to be getting higher price per pound than other states.
## I wonder what happens if I don't filter to top-producing states?

ggplot(dat, aes(x = year, y = priceperlb, color = state)) +
    geom_point() +
    geom_line(size = 1) +
    theme_bw()
## wow, there are some crazy things going on in some states, but it's hard to pick out what's what. Back to faceting!
ggplot(dat, aes(x = year, y = priceperlb, color = state)) +
    geom_point() +
    geom_line(size = 1) +
    facet_wrap(~state, ncol = 4) +
    theme_bw()

# what is total product value by year in each state?
# the ones that aren't high-producers are pretty interesting in terms of price per pound; I wonder if that holds here?

ggplot(dat, aes(x = year, y = prodvalue, color = state)) +
    geom_point() +
    geom_line(size = 1) +
    facet_wrap(~state, ncol = 4) +
    theme_bw()

# the Dakotas and California are pretty interesting


# what about just the top producers, what's the value?
ggplot(dat_top, aes(x = year, y = prodvalue, color = state)) +
    geom_point() +
    geom_line(size = 1) +
    theme_bw()

# still a little hard to tell what's going on, so I'm cutting it to interesting states:
dat_reduced <- dat %>%
    filter(state %in% c("CA", "ND", "SD", "FL", "WI"))
ggplot(dat_reduced, aes(x = year, y = priceperlb, color = state)) +
    geom_point() +
    geom_line(size = 1) +
    theme_bw()
ggplot(dat_reduced, aes(x = year, y = prodvalue, color = state)) +
    geom_point() +
    geom_line(size = 1) +
    theme_bw()

# what about overall average price per pound, and annual total production?
dat_summarized <- dat %>%
    group_by(year) %>%
    summarize(meanprice = mean(priceperlb, na.rm = TRUE),
              totalvalue = sum(prodvalue))

# add those in to the other graphics:
ggplot(dat_reduced, aes(x = year, y = priceperlb, color = state)) +
    geom_point() +
    geom_line(size = 1) +
    geom_line(data = dat_summarized, aes(x = year, y = meanprice), color = "darkslategray", size = 2, lty = 2, alpha = 0.7) +
    theme_bw()
ggplot(dat_reduced, aes(x = year, y = prodvalue, color = state)) +
    geom_point() +
    geom_line(size = 1) +
    geom_line(data = dat_summarized, aes(x = year, y = totalvalue/10), color = "darkslategray", size = 2, lty = 2, alpha = 0.7) +
    theme_bw()

# this might be a situation for stacked bars
ggplot(dat_reduced, aes(x = year, y = prodvalue/100000, fill = state)) +
    geom_col() +
    theme_bw()
# I kind of want to make a map
# with different color levels for... I don't know.