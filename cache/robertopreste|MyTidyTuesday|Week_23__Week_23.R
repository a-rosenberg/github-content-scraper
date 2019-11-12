## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
options(width = 120)


## ---- results='hide', message=FALSE, warning=FALSE-----------------------
library(tidyverse)
library(stringr)
library(magrittr)
library(rvest)
library(skimr)


## ---- eval=FALSE---------------------------------------------------------
## food_scrape <- function(restaurant, tbl_sel){
##     url <- glue::glue("https://fastfoodnutrition.org/{restaurant}/chart")
##     url %>%
##         read_html() %>%
##         html_table() %>%
##         .[tbl_sel] %>%
##         bind_rows() %>%
##         select(-X16) %>%
##         set_names(nm = c("item", "calories", "cal_fat", "total_fat", "sat_fat", "trans_fat",
##                          "cholesterol", "sodium", "total_carb", "fiber", "sugar", "protein",
##                          "vit_a", "vit_c", "calcium")) %>%
##         mutate(restaurant = str_replace(restaurant, "-", " "),
##                restaurant = str_to_title(restaurant)) # save restaurant name
## }


## ---- eval=FALSE---------------------------------------------------------
## mcd_df <- food_scrape("mcdonalds", c(1,2,3,9,19))
## cfa_df <- food_scrape("chick-fil-a", c(1,2,8,13))
## sonic_df <- food_scrape("sonic", c(1,2,17,18,20))
## arbys_df <- food_scrape("arbys", c(1:4,8))
## bk_df <- food_scrape("burger-king", c(1:3,7,11:12))
## dq_df <- food_scrape("dairy-queen", c(5, 7, 12, 25, 27))
## sub_df <- food_scrape("subway", c(1,2,3,4,5,6,7,8,9))
## taco_df <- food_scrape("taco-bell", c(1,2,3,4,5,15,18,19,20,22,23,24))
## 
## final_df <- bind_rows(mcd_df, cfa_df, sonic_df, arbys_df, bk_df, dq_df, sub_df, taco_df) %>%
##     select(restaurant, everything()) %>%
##     mutate(salad = case_when(str_detect(item, "salad") ~ "Salad",
##                              TRUE ~ "Other"))
## 
## final_df %>% write_csv("data/fastfood_calories.csv")


## ---- results="hide"-----------------------------------------------------
df <- read_csv("data/fastfood_calories.csv")


## ------------------------------------------------------------------------
head(df)


## ------------------------------------------------------------------------
skim(df)


## ------------------------------------------------------------------------
df %<>% select(-salad)


## ------------------------------------------------------------------------
fat_content <- df %>% 
    group_by(restaurant) %>% 
    summarise(Fat = mean(total_fat), 
              Saturated = mean(sat_fat), 
              Trans = mean(trans_fat)) %>% 
    gather(measure, value, Fat:Trans)


## ---- dpi=200------------------------------------------------------------
fat_content %>% 
    ggplot(aes(x = reorder(restaurant, value))) + 
    geom_col(aes(y = value, fill = measure), position = "dodge") +
    coord_flip() + 
    labs(x = "Franchise", y = "Fat (g)", title = "Mean fat content per franchise", fill = "", subtitle = "Chick-Fil-A seems to have low-fat food.") 


## ------------------------------------------------------------------------
sugar_content <- df %>% 
    group_by(restaurant) %>% 
    summarise(Sugar = mean(sugar))


## ---- dpi=200------------------------------------------------------------
sugar_content %>% 
    ggplot(aes(x = reorder(restaurant, Sugar), y = Sugar, fill = restaurant)) + 
    geom_col() + 
    coord_flip() + 
    labs(x = "Franchise", y = "Sugar (g)", title = "Mean sugar content per franchise", subtitle = "Taco Bell might have low-sugar food.") + 
    guides(fill = FALSE)


## ---- dpi=200------------------------------------------------------------
df %>% 
    ggplot(aes(x = sugar, fill = restaurant)) + 
    geom_histogram(bins = 60) + 
    facet_wrap(~ restaurant, nrow = 4, ncol = 2) + 
    labs(x = "Sugar (g)", title = "Sugar content distribution per franchise") +
    guides(fill = FALSE)


## ---- dpi=200------------------------------------------------------------
df %>% 
    filter(sugar <= 30) %>% 
    ggplot(aes(x = sugar, fill = restaurant)) + 
    geom_histogram(bins = 30) + 
    facet_wrap(~ restaurant, nrow = 4, ncol = 2) + 
    labs(x = "Sugar (g)", title = "Sugar content distribution per franchise [0-30 g]") +
    guides(fill = FALSE)


## ------------------------------------------------------------------------
sessionInfo()

