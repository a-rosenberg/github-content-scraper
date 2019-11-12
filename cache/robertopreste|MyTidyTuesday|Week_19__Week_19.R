## ------------------------------------------------------------------------
library(tidyverse)
library(fivethirtyeight)
data("airline_safety")


## ------------------------------------------------------------------------
head(airline_safety)


## ------------------------------------------------------------------------
airline_diff <- airline_safety %>% 
    mutate(fatal_accidents = fatal_accidents_00_14 - fatal_accidents_85_99, 
           fatalities = fatalities_00_14 - fatalities_85_99, 
           incidents = incidents_00_14 - incidents_85_99) %>% 
    gather(key = "event", value = "occurrences", fatal_accidents, fatalities, incidents) %>% 
    select(everything(), -c(fatal_accidents_85_99, fatal_accidents_00_14, fatalities_85_99, fatalities_00_14, incidents_85_99, incidents_00_14))


## ------------------------------------------------------------------------
head(airline_diff)


## ----dpi=200-------------------------------------------------------------
airline_diff %>% 
    filter(event == "fatalities", occurrences != 0) %>% 
    ggplot(aes(x = reorder(airline, occurrences), y = occurrences, fill = occurrences)) + 
    geom_col() + 
    coord_flip() + 
    scale_fill_gradientn(colors = c("darkgreen", "aquamarine3", "seagreen3", "yellow", "orange", "darkred")) +
    labs(x = "Airline", y = "Fatalities", fill = "", title = "Difference in number of fatalities", subtitle = "Years 1985-1999 vs 2000-2014")


## ----dpi=200-------------------------------------------------------------
airline_diff %>% 
    filter(event == "fatal_accidents", occurrences != 0) %>% 
    ggplot(aes(x = reorder(airline, occurrences), y = occurrences, fill = occurrences)) + 
    geom_col() + 
    coord_flip() +
    scale_fill_gradientn(colors = c("darkgreen", "aquamarine3", "seagreen3", "orange", "darkred"), values = c(0, 0.6, 0.7, 0.8, 1)) + 
    labs(x = "Airline", y = "Fatal Accidents", fill = "", title = "Difference in number of fatal accidents", subtitle = "Years 1985-1999 vs 2000-2014")


## ----dpi=200-------------------------------------------------------------
airline_diff %>% 
    filter(event == "incidents", occurrences != 0) %>% 
    ggplot(aes(x = reorder(airline, occurrences), y = occurrences, fill = occurrences)) + 
    geom_col() + 
    coord_flip() +
    scale_fill_gradientn(colors = c("darkgreen", "aquamarine3", "seagreen3", "orange", "darkred"), values = c(0, 0.7, 0.8, 0.9, 1)) + 
    labs(x = "Airline", y = "Incidents", fill = "", title = "Difference in number of incidents", subtitle = "Years 1985-1999 vs 2000-2014")


## ------------------------------------------------------------------------
sessionInfo()

