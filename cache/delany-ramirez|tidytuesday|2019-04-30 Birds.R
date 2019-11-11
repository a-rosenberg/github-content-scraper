library(tidyverse)
library(lubridate)
Sys.setlocale("LC_TIME", "C")

bird_collisions_raw <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-30/bird_collisions.csv")
mp_light_raw <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-30/mp_light.csv")

head(bird_collisions_raw)

## Tidy the Dates

bird_collisions <- bird_collisions_raw %>% 
  mutate_at(vars(-date), as_factor) %>% 
  mutate(month = months(ymd(date)),
         day = weekdays(ymd(date)))
  

library(FactoMineR)
library("factoextra")

## Correspondence analysis

bird_mca <- bird_collisions %>% 
  filter(flight_call != c("Rare", "No"),
         genus %in% c("Melospiza", "Zonotrichia", "Catharus", "Junco", "Setophaga")) %>% 
  select(genus, habitat, locality, month) %>% 
  MCA(ncp = 5, graph = FALSE)

fviz_mca_var(bird_mca, col.var = "cos2", 
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal()) + 
  labs(title = "Flight call birds Correspondence analysis by genus, habitat, locality and month \nfor Genus top 5 bird crashers", 
       caption = "Winger BM, Weeks BC, Farnsworth A, Jones AW, Hennen M, Willard DE (2019)\nNocturnal flight-calling behaviour predicts vulnerability to artificial light in migratory birds. \nProceedings of the Royal Society B 286(1900): 20190364.")



