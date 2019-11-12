## ----Color Table, echo = F, message = F, warning = F---------------------
library(tidyverse)
library(kableExtra)

read_csv("../data/week11_fifa_audience.csv") %>% 
  arrange(desc(gdp_weighted_share)) %>% 
#  rename(rank = X1) %>% 
  top_n(38) %>% 
  kable(format = "html",
        col.names = c("Rank", "COUNTRY", "BODY",
                      "GLOBAL\nPOPULATION",
                      "WORLD CUP TV\nAUDIENCE",
                      "GDP-WEIGHTED\nAUDIENCE")) %>% 
  add_header_above(c(" " = 3, "IN 2010, SHARE OF ..." = 3)) %>% 
  column_spec(6, background = "#F5F5F5") %>% 
  kable_styling()


