#First R submission 
library(tidyverse)
library(janitor)
library(lubridate)
library(extrafont)
library(ggdark)


        ufo_raw<- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv") 
        clean_names ()


        map_borders <-  map_data("state", region = NULL) %>% filter (region == "texas")  
        
        ufo <- ufo_raw %>% select (date_time, city_area, state, latitude, longitude, encounter_length) %>%
        
        filter (
                state == "tx",
                latitude > 25,      # remove borders erroneously listed as TX outside of state borders
                latitude < 38,      # remove borders erroneously listed as TX outside of state borders
                longitude < -90     # remove borders erroneously listed as TX outside of state borders
                )  %>%
        mutate(
                encounter_length = encounter_length/3600,              #convert seconds to hours
                date_time = as.Date(date_time, format = "%m/%d/%Y")
                ) 
                



        ggplot () +#plot Texas borders
                geom_polygon (data = map_borders, aes(x = long, y = lat, group = group), 
                color = "black", fill = "#303030", size = 1.15) +
                
        #plot UFO encounters
        geom_point (data = ufo, aes (x = longitude, y = latitude, size = encounter_length), color = "green") +
        
        #Clifton encounter annotation
        annotate("text",label = "42 day encounter\nin Clifton in 1966.",size = 3, hjust = 0, color = "magenta", family = "Rockwell",
               x = -92.57639, y = 31.78222, xmax = -83.5) +
        
        geom_curve(
                aes(x = -92.5, y = 31.7, xend = -97.57639, yend = 31.78222),
                arrow = arrow(length = unit(0.2, "cm")), 
                size = 0.4, color = "magenta", curvature = -0.3
        ) +
        coord_fixed(1.3) +
        scale_size_continuous(breaks = c(1, 10, 100)) +
        dark_mode(theme_minimal()) +
        theme(text = element_text(family = "Rockwell", color = "green"),
        plot.title = element_text(hjust = 0.5, size = 18),
        plot.caption = element_text(hjust = 0, size = 8),
        legend.title = element_text(size = 10, hjust = 0.5, vjust = 0.5),
        legend.text = element_text(size = 9, hjust = 0.5, vjust = 0.5),
        legend.position = c(0.82,0.18),
        legend.justification=c(0, 1), 
        legend.key.size = unit(0.1, 'lines'),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
        ) +
        labs(
        title = "UFOs over Texas\n",
        size = "Encounter (hrs)",
        caption = "\nEach dot represents a reported UFO sighting between 1910 and 2014.  
                \nSource: National UFO Reporting Center  | Visualization: Ethan Tenison @sassyStatistics"
        ) +
ggsave("ufo.png", height =3.85)

        