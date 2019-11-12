## ----echo=FALSE----------------------------------------------------------
library(tidyverse)
library(knitr)
library(ggbeeswarm)
library(ggforce)
library(ggthemes)
library(carbonate)

carbonate::

fed_rd <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/fed_r_d_spending.csv")

#save(... = fed_rd, file = "bd_fed_rd.RData")

summary(fed_rd)


## ------------------------------------------------------------------------

selected_deps <- c("DOD", "NASA", "DOE", "DHS")

fed_rd_by_decade <- fed_rd %>%
  mutate(decade = floor(year/10)*10) %>% 
  group_by(decade, department) %>% 
  summarize_at(vars(rd_budget, total_outlays, discretionary_outlays, gdp), sum)

fed_rd_by_decade %>%  
  filter(department %in% selected_deps) %>%
  ggplot(aes(x= decade, y = department, color = department)) +
    ggbeeswarm::geom_quasirandom(alpha=1,aes(size=rd_budget),groupOnX = FALSE, show.legend = FALSE) +
    theme_economist() +
    labs(title='Total Budget of R&D - by decade',
       subtitle='Displayed by four US departments',
       y='',
       x="Decade",
       caption='Data: New York Times')


## ------------------------------------------------------------------------
fed_rd %>% 
  filter(department == "DOD") %>%
    ggplot(aes(x= year, y =rd_budget)) +
    geom_line(aes(colour = "ff8080"), size = 1.5) +
    geom_point(aes(colour = department), size = 2.5) +
    theme_light() +
    geom_vline(xintercept=1989, color="orange", size=1) +
    geom_text(aes(x= 1989, y = 8e+10), label = "End of Cold War", color="orange") +
    geom_vline(xintercept=2001, color="lightblue", size=1.5) +
    geom_text(aes(x= 2004, y = 6e+10), label = "Nine\nEleven", color="blue") +
    labs(title='Total R&D Budget of the US Defense Department  - by year',
       subtitle='Some historical dates',
       y='',
       x="Decade",
       caption='Data: New York Times') +
    theme(legend.position = "none")
  


