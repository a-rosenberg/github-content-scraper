# week 13

library(tidyverse)
library(skimr)

dat <- read.csv("data/week13_alcohol_global.csv")

skim(dat)
head(dat)


# reshape so I can group and make faceted plots
# top_n to pull out 40 countries with highest consumption
# make country a factor
# gather() to put it in long format
dat2 <- dat %>%
    rename(beer = beer_servings,
           wine = wine_servings,
           spirits = spirit_servings) %>%
    top_n(40, total_litres_of_pure_alcohol) %>%  
    mutate(country = as.factor(country)) %>%  
    gather(key = "alc_type", value = "value", 
           -country, -total_litres_of_pure_alcohol) 


# scatterplot matrix
# (everything against everything else)
plot(dat, main = "Scatterplot Matrix")


# exploratory bar chart
ggplot(dat2, aes(x = alc_type, y = value, fill = alc_type)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~country, ncol = 5) +
    scale_fill_brewer(type = "qual", palette = "Paired") +
    theme_bw() +
    labs(title = "Amount of alcohol consumed by type", 
         subtitle = "40 countries with highest total litres of pure alcohol", 
         x = "alcohol type", 
         y = "# servings")


# hm, how many countries have beer as top? spirits? wine?
# order within each country, and rank?
# not sure what to plot from this.... 

# thanks, stackoverflow, for dense_rank
# https://stackoverflow.com/questions/26106408/create-a-ranking-variable-with-dplyr

dat3 <- dat %>%
    rename(beer = beer_servings,
           wine = wine_servings,
           spirits = spirit_servings) %>%
    gather(key = "alc_type", value = "value", 
           -country, -total_litres_of_pure_alcohol) %>%
    group_by(country) %>%
    mutate(alc_rank = dense_rank(desc(value))) %>%
    arrange(country, alc_rank)

rank_summary <- dat3 %>%
    ungroup() %>%
    summarize()
