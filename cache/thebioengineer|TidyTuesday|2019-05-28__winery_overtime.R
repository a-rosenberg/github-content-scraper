## ----load_libraries------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(janitor)

# devtools::install_github("ropensci/plotly") #the dev 
library(plotly)
library(htmlwidgets)

tt<-tt_load("2019-05-28")
tt


## ----transform-----------------------------------------------------------


wine_trends<-tt$`winemag-data-130k-v2` %>% 
  
  select(country,points,title,variety) %>% 
  
  mutate(year = gsub("(.*)([12][90]\\d{2})(.*)","\\2",title),
         year = as.numeric(year,format="%Y")) %>% 
  
  filter(!is.na(year)) %>% 
  
  group_by(variety) %>% 

  filter(n()>5000) %>%  #keep only the most common wines
  
  filter(year < 2019, year > 1995) %>% 
  
  ungroup



#dichotomize ratio by GDP?
  
wine_trend_plot <- ggplot(wine_trends)+
  geom_point(aes(x=year,
                 y=points,
                 color = variety,
                 text=title))+
  geom_smooth(aes(x=year,
                 y=points,
                 color = variety))



