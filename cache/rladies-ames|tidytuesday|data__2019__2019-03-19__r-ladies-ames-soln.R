## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)


## ----getdat--------------------------------------------------------------
library(tidyverse)
combined_data <- read_csv("https://raw.githubusercontent.com/5harad/openpolicing/master/results/data_for_figures/combined_data.csv")
head(combined_data)


## ----iowadat-------------------------------------------------------------
iowa <- filter(combined_data, state == "IA")
iowa


## ----iowadat2------------------------------------------------------------
iowa <- read_rds("iowa.rds")
glimpse(iowa)
summary(iowa$date)
dim(iowa)


## ----last5---------------------------------------------------------------
library(lubridate)
iowa %>% mutate(year = year(date)) %>% 
  filter(year >= 2013, !is.na(date)) -> iowa
head(iowa)
dim(iowa)


## ----miss----------------------------------------------------------------
library(visdat)
vis_dat(iowa, warn_large_data = FALSE)


## ----location------------------------------------------------------------
count(iowa, location)
count(iowa, department_name)

