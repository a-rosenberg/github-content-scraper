## ------------------------------------------------------------------------
library(tidyverse)
library(plotly)

theme_set(theme_light())

fed_rd <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/fed_r_d_spending.csv")


## ------------------------------------------------------------------------
fed_rd %>% 
    summary()


## ------------------------------------------------------------------------
fed_rd_processed <- fed_rd %>%
    mutate(rd_pct_gdp = rd_budget / gdp, 
            rd_pct_tot = rd_budget / total_outlays, 
        rd_pct_dis = rd_budget / discretionary_outlays)


## ------------------------------------------------------------------------
p <- fed_rd_processed %>% 
    ggplot(aes(year, rd_pct_gdp, color = department)) +
    geom_line() + 
    scale_y_continuous(labels = scales::percent_format())
    
ggplotly(p) 


## ------------------------------------------------------------------------
p <- fed_rd_processed %>% 
    ggplot(aes(year, rd_pct_dis, color = department)) +
    geom_line() + 
    scale_y_continuous(labels = scales::percent_format())
    
ggplotly(p) 

