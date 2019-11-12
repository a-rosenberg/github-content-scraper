## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----message=FALSE-------------------------------------------------------
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(ggplot2)


## ----cars----------------------------------------------------------------
epl_wages_raw <- read.csv("https://raw.githubusercontent.com/ft-interactive/baseline/master/eplWages.csv")


## ----pressure, echo=FALSE------------------------------------------------
epl_wages_lfc <- epl_wages_raw %>% 
  filter(club == "Liverpool") %>% 
  mutate(season = season %>% str_replace_all("(.{4})(.*)", "\\1-\\2"))

y_labs <- epl_wages_lfc$season

epl_wages_lfc_plot <- epl_wages_raw %>% 
  filter(club == "Liverpool") %>% 
  mutate(season = season %>% str_replace_all("(.{4})(.*)", "\\1"))

df <- seq.Date(from = as.Date("1992-08-01"), 
                         to = as.Date("2017-07-01"), 
                         by = "month")

df %>% as_data_frame() %>% 
  mutate(date = format(value, "%m/%d"),
         year = format(value, "%Y")) %>% 
  group_by(year) %>% 
  mutate(season = case_when(
    date >= "08-01" ~ year,
    date <= "07-01" ~ glue::glue("{year - 1}")
  ))
  
  
  
  mutate(season = case_when(
    value %in% seq(as.Date("1992-08-01"), as.Date("1993-06-01"), by = "month") ~ "1992/1993",
    TRUE ~ NA
  ))



## ------------------------------------------------------------------------
epl_wages_lfc %>% 
  ggplot(aes(x = season, y = wages)) +
  geom_rect(aes(xmin = season, xmax = season), 
            ymin = -Inf, ymax = Inf, fill = "red") +
  geom_point()
  

