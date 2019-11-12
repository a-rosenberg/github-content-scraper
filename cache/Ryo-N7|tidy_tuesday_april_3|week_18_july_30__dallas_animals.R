## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)
library(purrr)


## ------------------------------------------------------------------------
dallas_anim_raw <- read_xlsx("week18_dallas_animals.xlsx")

glimpse(dallas_anim_raw)



## ------------------------------------------------------------------------
dallas_anim_raw %>% 
  ggplot(aes(x = council_district, y = )) +
  geom_col()

