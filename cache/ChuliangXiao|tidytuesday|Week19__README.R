## ----message = F, warning = F--------------------------------------------
library(tidyverse)
library(knitr)
library(kableExtra)
options(knitr.table.format = "html")

sp_df <- read_csv("../data/week19_airline_safety.csv") %>% 
  select(-X1) %>% 
  unite(airline_km, c("airline", "avail_seat_km_per_week")) %>% 
  unite(year_type, c("year_range", "type_of_event")) %>% 
  spread(year_type, n_events) %>% 
  separate(airline_km, c("airline", "km"), sep = "_") %>% 
  mutate(km = as.numeric(km))

sp_df[, c(1, 2, 8, 6, 7, 5, 3, 4)] %>% 
  mutate(km = round(km / 1e6)) %>% 
  kable(col.names = c("AIRLINE", "AVAILABLE\nSEAT KM\nPER WEEK",
                      "INCIDENTS", "FATAL\nACCIDENTS", "FATALITIES",
                      "INCIDENTS", "FATAL\nACCIDENTS", "FATALITIES")) %>% 
  kable_styling("striped") %>% 
  add_header_above(c(" " = 1,
                     " " = 1,
                     "1985-1999" = 3,
                     "2000-2014" = 3))

