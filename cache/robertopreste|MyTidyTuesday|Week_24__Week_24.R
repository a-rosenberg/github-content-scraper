## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
options(width = 120)


## ---- results='hide', message=FALSE, warning=FALSE-----------------------
library(tidyverse)
library(magrittr)
library(readxl)
library(skimr)


## ------------------------------------------------------------------------
df <- read_excel("data/catsvdogs.xlsx", skip = 1, 
                 col_names = c("location", "households_1000", "perc_households_pets", 
                               "num_pet_households_1000", "perc_dog_owners", 
                               "dog_own_households_1000", "mean_num_dogs_per_household", 
                               "dog_population_1000", "perc_cat_owners", "cat_own_households_1000", 
                               "mean_num_cats_per_household", "cat_population_1000"))


## ------------------------------------------------------------------------
head(df)


## ------------------------------------------------------------------------
skim(df)


## ---- dpi=200------------------------------------------------------------
df %>% 
    ggplot(aes(x = reorder(location, -perc_households_pets), 
               y = perc_households_pets, fill = location)) + 
    geom_col() + 
    coord_flip() + 
    labs(x = "US State", y = "%", title = "Percentage of households with pets", subtitle = "District of Columbia seems to be not so pet-friendly.") + 
    guides(fill = FALSE)


## ---- dpi=200------------------------------------------------------------
df %>% 
    ggplot(aes(x = reorder(location, -perc_households_pets), fill = location)) + 
    geom_col(aes(y = dog_own_households_1000 - cat_own_households_1000)) + 
    coord_flip() + 
    labs(x = "US State", y = "Difference (in 1000s households)", 
         title = "Dog- vs cat-owning households", 
         subtitle = "Households with dogs definitely outnumber those hosting cats.") + 
    guides(fill = FALSE) + 
    scale_y_continuous(breaks = c(-250, 0, 250, 500, 750, 1000, 1250))


## ------------------------------------------------------------------------
gath_df <- df %>% 
    mutate(dogs = mean_num_dogs_per_household, 
           cats = mean_num_cats_per_household) %>% 
    select(location, num_pet_households_1000, dogs, cats) %>% 
    gather(key = "pet", value = "value", dogs, cats)


## ------------------------------------------------------------------------
gath_df


## ---- dpi=200, message=FALSE---------------------------------------------
gath_df %>% 
    ggplot(aes(x = num_pet_households_1000, y = value, color = pet)) + 
    geom_smooth() + 
    geom_point(alpha = 0.5) + 
    labs(x = "Households (in 1000s)", y = "Number of pets", 
         title = "Mean number of dogs/cats per household", 
         subtitle = "The number of pets per household seems to reach a plateau after 1M households with pets.")


## ------------------------------------------------------------------------
norm_df <- df %>% 
    mutate(norm_dogs = dog_own_households_1000 * mean_num_dogs_per_household, 
           norm_cats = cat_own_households_1000 * mean_num_cats_per_household) %>% 
    select(location, norm_dogs, norm_cats, perc_households_pets)


## ------------------------------------------------------------------------
norm_df


## ---- dpi=200------------------------------------------------------------
norm_df %>% 
    ggplot(aes(x = reorder(location, -perc_households_pets), fill = location)) + 
    geom_col(aes(y = norm_dogs - norm_cats)) + 
    coord_flip() + 
    labs(x = "US State", y = "Difference (in 1000s households)", 
         title = "Dog- vs cat-owning households (normalized)", 
         subtitle = "With normalized data, we see that cats win the fight.") + 
    guides(fill = FALSE) + 
    scale_y_continuous(breaks = c(-1000, -500, 0, 500, 1000, 1500))


## ---- dpi=200------------------------------------------------------------
df %>% 
    ggplot(aes(x = reorder(location, -perc_households_pets), fill = location)) + 
    geom_col(aes(y = dog_population_1000 - cat_population_1000)) + 
    coord_flip() + 
    labs(x = "US State", y = "Difference (in 1000s pets)", 
         title = "Difference of dog/cat population", 
         subtitle = "Most US States host cats, rather than dogs.") + 
    guides(fill = FALSE) + 
    scale_y_continuous(breaks = c(-1000, -500, 0, 500, 1000, 1500))


## ------------------------------------------------------------------------
sessionInfo()

