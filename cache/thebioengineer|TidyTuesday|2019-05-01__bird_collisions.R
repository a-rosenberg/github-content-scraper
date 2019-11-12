## ----load_libraries------------------------------------------------------
# devtools::install_github("thebioengineer/tidytuesdayR")
library(tidytuesdayR)
library(tidyverse)

# devtools::install_github("ropensci/plotly") #the dev 
library(plotly)
library(htmlwidgets)

tt_data<-tt_load(2019,week=18)

tt_data


## ----transform-----------------------------------------------------------


#cluster groupings over the years

mp_birds<-tt_data$bird_collisions %>%
  filter(locality=="MP") %>% 
  left_join(tt_data$mp_light) %>% 
  filter(!is.na(light_score))


summarized_collisions<-mp_birds %>% 
  group_by(habitat,stratum,date,flight_call) %>% 
  summarise(ncollisions=n(),
            light_score=mean(light_score))

ggplot(summarized_collisions)+
  geom_point(data=summarized_collisions,
             aes(x=date,
                 y=light_score,
                 size=ncollisions), alpha=.1) + 
  geom_point(aes(x=date,
                 y=light_score,
                 colour=habitat,
                 size=ncollisions)) +
  facet_grid(habitat~stratum)
  


