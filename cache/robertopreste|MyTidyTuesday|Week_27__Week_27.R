## ----global_options, echo = FALSE, include = FALSE-----------------------
options(width = 120)


## ---- results='hide', message=FALSE, warning=FALSE-----------------------
library(tidyverse)
library(magrittr)
library(lubridate)
library(skimr)
library(RColorBrewer)


## ----results='hide'------------------------------------------------------
df <- read_csv("data/us_births_2000-2014.csv")


## ------------------------------------------------------------------------
head(df)


## ------------------------------------------------------------------------
skim(df)


## ------------------------------------------------------------------------
df %<>% 
    mutate(date = as_date(paste(year, month, date_of_month, sep = "-")))


## ------------------------------------------------------------------------
df


## ------------------------------------------------------------------------
pastels <- brewer.pal(4, "Pastel1")


## ---- dpi=200------------------------------------------------------------
df %>% 
    ggplot(aes(x = year, y = births)) + 
    geom_boxplot(aes(group = year), fill = pastels[2]) + 
    scale_x_continuous(breaks = c(2000:2014)) + 
    labs(x = "Year", y = "Births", title = "Births distribution over years")


## ---- dpi=200------------------------------------------------------------
df %>% 
    ggplot(aes(x = month, y = births)) + 
    geom_boxplot(aes(group = month), fill = pastels[2]) + 
    scale_x_continuous(breaks = c(1:12), 
                       labels = month.abb) + 
    labs(x = "Month", y = "Births", title = "Births distribution over months")


## ---- dpi=200------------------------------------------------------------
day_colors <- c(rep(pastels[2], 12), pastels[1], rep(pastels[2], 18))
df %>% 
    ggplot(aes(x = date_of_month, y = births)) + 
    geom_boxplot(aes(group = date_of_month), fill = day_colors) + 
    scale_x_continuous(breaks = c(1, 10, 20, 30)) + 
    labs(x = "Day of month", y = "Births", title = "Births distribution over days of the month")


## ---- dpi=200, message=FALSE, warning=FALSE------------------------------
df %>% 
    ggplot(aes(x = date_of_month, y = births)) + 
    geom_boxplot(aes(group = date_of_month)) +
    facet_wrap(month ~ ., nrow = 4, ncol = 3, 
               labeller = function(variable, value) {return(month.abb[value])}) + 
    scale_x_continuous(breaks = c(1, 10, 20, 30)) + 
    labs(x = "Day of month", y = "Births", title = "Births distribution over days per each month")


## ---- dpi=200------------------------------------------------------------
week_colors <- c(rep(pastels[2], 5), rep(pastels[1], 2))
df %>% 
    ggplot(aes(x = day_of_week, y = births)) + 
    geom_boxplot(aes(group = day_of_week), fill = week_colors) + 
    scale_x_continuous(breaks = c(1:7), 
                       labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
    labs(x = "Day of the week", y = "Births", title = "Births distribution over the week")


## ---- dpi=200------------------------------------------------------------
df %>% 
    mutate(lucky = case_when(date_of_month == 13 & day_of_week == 5 ~ "Friday 13", 
                             date_of_month == 13 & day_of_week != 5 ~ "Regular 13",
                             date_of_month != 13 & day_of_week == 5 ~ "Regular Friday", 
                             TRUE ~ "Other")) %>% 
    filter(lucky != "Other") %>% 
    ggplot(aes(x = date, y = births, color = lucky)) + 
    geom_point() + 
    geom_smooth(method = "loess", se = F) + 
    labs(x = "Date", y = "Births", title = "Number of births on 13s, Fridays and Friday 13s") + 
    guides(color = guide_legend(title = NULL)) + 
    theme(legend.position = "bottom")


## ------------------------------------------------------------------------
sessionInfo()

