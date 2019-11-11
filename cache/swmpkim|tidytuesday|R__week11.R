# week 11 - FIFA
# original article https://fivethirtyeight.com/features/how-to-break-fifa/
# data source https://github.com/rudeboybert/fivethirtyeight
# https://github.com/rfordatascience/tidytuesday/blob/master/data/week11_fifa_audience.csv


library(tidyverse)
library(skimr)
library(ggrepel)


# read data
dat <- read.csv('data/week11_fifa_audience.csv')


# explore data
head(dat)
dim(dat)
skim(dat)
unique(dat$confederation)
unique(dat$country)


# group, pull out top 10 in each confederation by tv audience share, reorder factors for nicer exploratory plots
dat2 <- dat %>%
  group_by(confederation) %>%
  top_n(10, tv_audience_share) %>%
  ungroup() %>%
  mutate(country = fct_reorder(country, tv_audience_share, .desc=TRUE))


# rough exploratory plot
ggplot(dat2, aes(x = country, y = tv_audience_share)) +
  geom_col() +
  facet_wrap(~confederation, ncol = 1, scales = "free") +
  ggtitle("TV Audience Share", subtitle = "ordered by tv audience share") +
  theme_minimal()
ggsave("output/wk11_exploratoryplot.png", width = 6, height = 8, units = "in")



ggplot(dat2, aes(x = country, y = population_share)) +
  geom_col() +
  facet_wrap(~confederation, ncol = 1, scales = "free") +
  ggtitle("Population Share", subtitle = "ordered by tv audience share") +
  theme_minimal()

ggplot(dat2, aes(x = country, y = gdp_weighted_share)) +
  geom_col() +
  facet_wrap(~confederation, ncol = 1, scales = "free") +
  ggtitle("GDP-weighted Share", subtitle = "ordered by tv audience share") +
  theme_minimal()

ggplot(dat, aes(y = tv_audience_share, x = population_share, color = confederation)) +
  geom_point(size = 3, alpha = 0.5) +
  ggtitle("Do countries with higher populations watch more soccer?") +
  theme_minimal()




## do countries with higher populations watch more soccer?
ggplot(dat2, aes(y = tv_audience_share, x = population_share, 
                 color = confederation)) +
    geom_point(size = 2.5, alpha = 0.7) +
    geom_text_repel(data = subset(dat2, population_share >= 1.9), 
                    aes(label = country)) +
    labs(title = "Do countries with higher populations watch more soccer?", 
            subtitle = "top 10 countries from each confederation by tv audience share",
         x = "population share",
         y = "tv audience share") +
    theme_minimal()
ggsave("output/wk11_audshare-v-popshare.png")




## with log scales
ggplot(dat2, aes(y = tv_audience_share, x = population_share, 
                 color = confederation)) +
    geom_point(size = 2.5, alpha = 0.7) +
    geom_text_repel(data = subset(dat2, population_share > 1), 
                    aes(label = country)) +
    scale_y_log10() +
    scale_x_log10() +
    labs(title = "Do countries with higher populations watch more soccer?", 
         subtitle = "top 10 countries from each confederation by tv audience share \nlabels on countries where population share is > 1",
         x = "population share",
         y = "tv audience share") +
    theme_minimal()
ggsave("output/wk11_audshare-v-popshare_log.png")



## using RColorBrewer
## and a square root scale
ggplot(dat2, aes(y = tv_audience_share, x = population_share, 
                 color = confederation)) +
    geom_point(size = 2.5, alpha = 0.7) +
    geom_text_repel(data = subset(dat2, population_share > 1.9), 
                    aes(label = country)) +
    scale_y_sqrt() +
    scale_x_sqrt() +
    scale_color_brewer(type = "qual", palette = "Dark2") +
    labs(title = "Do countries with higher populations watch more soccer?", 
         subtitle = "top 10 countries from each confederation by tv audience share",
         x = "population share",
         y = "tv audience share") +
    theme_minimal()


## do that on unfiltered data
ggplot(dat, aes(y = tv_audience_share, x = population_share, 
                 color = confederation)) +
    geom_point(size = 2.5, alpha = 0.7) +
    geom_text_repel(data = subset(dat2, population_share > 1.5), 
                    aes(label = country)) +
    scale_y_sqrt() +
    scale_x_sqrt() +
    scale_color_brewer(type = "qual", palette = "Dark2") +
    labs(title = "Do countries with higher populations watch more soccer?", 
         x = "population share",
         y = "tv audience share") +
    theme_minimal()
