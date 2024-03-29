## ----load_libraries------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(plotly)

tt<-tt_load("2019-09-03")
tt


## ----transform-----------------------------------------------------------

gpu_plot<-tt$gpu %>% 
  filter(!is.na(process)) %>% 
  mutate(details=paste("Year Released:",date_of_introduction,"<br>",
                       "Manufacturer:",manufacturer_s,"<br>",
                       "Processor:",processor)) %>% 
  ggplot() +
  scale_x_log10()+
  scale_y_log10()+
  geom_point(aes(x     = process,
                 y     = transistor_count,
                 color = designer_s,
                 label = details ))

ggplotly(gpu_plot)


roles_ot <- roles %>% 
  distinct(season,guest_star,role) %>% 
  group_by(season,guest_star) %>% 
  summarize(nroles = n()) %>% 
  ungroup()


ggplot(roles_ot)+
  geom_density(aes(x=nroles))+
  facet_wrap(season~.)

  


