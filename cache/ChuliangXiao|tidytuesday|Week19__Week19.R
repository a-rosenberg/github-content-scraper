## ----message = F, warning = F--------------------------------------------
library(tidyverse)
library(ggthemes)


## ----message = F, warning = F--------------------------------------------
raw_df <- read_csv("../data/week19_airline_safety.csv") %>% 
  select(-X1)


## ------------------------------------------------------------------------
sp_df <- raw_df %>% 
  filter(type_of_event == "fatalities") %>% 
  select(airline, year_range, n_events) %>% 
  # spread the year_range column
  spread(year_range, n_events) %>% 
  rename(a = `85_99`, b = `00_14`) %>% 
  mutate(c = (a + b)/2) %>% 
  bind_cols(select(raw_df, avail_seat_km_per_week) %>% distinct()) 


## ------------------------------------------------------------------------
sp_df1 <- raw_df %>% 
  # First, unite two columns
  unite(year_type, c("year_range", "type_of_event")) %>% 
  # Second, spread the united columns
  select(airline, year_type, n_events) %>% 
  spread(year_type, n_events) %>% 
  bind_cols(select(raw_df, avail_seat_km_per_week) %>% distinct()) 
sp_df1


## ------------------------------------------------------------------------
sp_df2 <- raw_df %>% 
  # Since there are two `index` columns
  # Combine those two columns
  unite(airline_km, c("airline", "avail_seat_km_per_week")) %>% 
  unite(year_type, c("year_range", "type_of_event")) %>% 
  spread(year_type, n_events) %>% 
  separate(airline_km, c("airline", "km"), sep = "_") %>% 
  mutate(km = as.numeric(km))
sp_df2


## ------------------------------------------------------------------------
library(knitr)
library(kableExtra)
options(knitr.table.format = "html")

sp_df2[, c(1, 2, 8, 6, 7, 5, 3, 4)] %>% 
  mutate(km = round(km / 1e6)) %>% 
  kable(col.names = c("AIRLINE", "AVAILABLE\nSEAT KM\nPER WEEK",
                      "INCIDENTS", "FATAL\nACCIDENTS", "FATALITIES",
                      "INCIDENTS", "FATAL\nACCIDENTS", "FATALITIES")) %>% 
  kable_styling("striped") %>% 
  add_header_above(c(" " = 1,
                     " " = 1,
                     "1985-1999" = 3,
                     "2000-2014" = 3))

