## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------
library(ggplot2);
library(maptools);
library(forecast);
library(lubridate);
library(ggthemes);
library(gganimate);
library(tidyverse)


## ----loading_data--------------------------------------------------------
trains <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-26/full_trains.csv")


## ------------------------------------------------------------------------
# overview of my trains data
skimr::skim(trains)


## ------------------------------------------------------------------------
# gather cause for delay and create a more focused dataset
small_trains <- trains %>% 
  gather(delay_cause, delayed_number, delay_cause_external_cause:delay_cause_travelers) %>% 
  select(year:total_num_trips, avg_delay_all_departing, avg_delay_all_arriving, num_late_at_departure, num_arriving_late, delay_cause, delayed_number)


## ------------------------------------------------------------------------
#selecting departures from paris

paris_problems <- small_trains %>% 
  filter(str_detect(departure_station, "PARIS"))


## ------------------------------------------------------------------------
#Filter only national services

paris_problems <- filter(paris_problems, paris_problems$service == "National")


## ------------------------------------------------------------------------
# just one more peek at what is missing - nothing now!

colSums(is.na(paris_problems))


## ------------------------------------------------------------------------
#Summarise some variables

paris_problems <- paris_problems %>% 
  
  group_by(arrival_station, year) %>% 
  
  summarise(total_num_trips = sum(total_num_trips, na.rm=T),
            num_late_at_departure = sum(num_late_at_departure, na.rm=T)) %>% 
  
ungroup()


## ------------------------------------------------------------------------
#Make a new variable regarding percentage of delayed trips
paris_problems$percentage <- (paris_problems$num_late_at_departure/paris_problems$total_num_trips)*100


## ------------------------------------------------------------------------
# animated bar chart of % of trains departing late from paris, grouped by destination, between 2015 and 2018

p <- 

  ggplot(data=paris_problems, 
         aes(x=arrival_station,y=percentage)) + 
  geom_col(aes(fill=arrival_station), width = 0.8) + 
  coord_flip()+
  transition_time(year)+
  labs(title='SNCF Trains Departing From Paris:
         % of Late Departures by Destination', 
       subtitle="Year: {round(frame_time)}",
       x="", y="Trains Departing Late (%)", 
       caption="@LAM_MPH, Source: SNCF")+ 
  theme_minimal() +
  theme(legend.position = "none",
        text=element_text(family="Arial"),
        plot.title=element_text(size=13, hjust=0, face='bold'),
        plot.subtitle=element_text(size=12, hjust=0, face='italic'),
        plot.caption=element_text(size=8, hjust=1),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        axis.ticks.y = element_blank())

#Save GGplot animation
anim_save(filename = "latetrains.gif", animation = p)

