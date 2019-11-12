## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----warning=FALSE, message=FALSE----------------------------------------
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggimage)


## ------------------------------------------------------------------------
catdog_df <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-09-11/cats_vs_dogs.csv") %>% select(-X1)


## ------------------------------------------------------------------------
catdog_df %>% glimpse()

