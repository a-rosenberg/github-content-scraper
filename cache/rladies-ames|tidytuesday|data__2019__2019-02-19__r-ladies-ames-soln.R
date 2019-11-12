## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)


## ----dat-----------------------------------------------------------------
library(tidyverse)
library(plotly)
phd_field <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-19/phd_by_field.csv")
phd_field %>% count(year)
head(phd_field)


## ----alldegress----------------------------------------------------------
phd_field %>% group_by(year) %>% summarise(total = sum(n_phds, na.rm = T)) %>% 
  ggplot(aes(x = year,y = total)) + 
  geom_line() + 
  scale_x_continuous(breaks = 2008:2017)


## ----popular-------------------------------------------------------------
phd_field %>% group_by(broad_field) %>% 
  summarize(total = sum(n_phds, na.rm = T)) %>% 
  arrange(desc(total))


## ----popular2------------------------------------------------------------
phd_field %>% group_by(major_field) %>% 
  summarize(total = sum(n_phds, na.rm = T)) %>% 
  arrange(desc(total))


## ----popular3------------------------------------------------------------
phd_field %>%group_by(field) %>% 
  summarize(total = sum(n_phds, na.rm = T)) %>% 
  arrange(desc(total))


## ----algebra-------------------------------------------------------------
phd_field %>% filter(field == "Algebra") %>% 
  ggplot(aes(year, n_phds)) + 
  geom_line()


## ----stats---------------------------------------------------------------
phd_field %>% filter(str_detect(field, "Statistics"))  %>% 
  ggplot(aes(year, n_phds, color = field)) + 
  geom_line()


## ----alltime-------------------------------------------------------------
p <- phd_field %>% 
  ggplot(aes(year, n_phds, group = field)) + 
  geom_line(alpha = .3) + 
  facet_wrap(~broad_field)
p
#ggplotly(p)


## ----psych---------------------------------------------------------------
p <- phd_field %>% filter(broad_field == "Psychology and social sciences") %>% 
  ggplot(aes(year, n_phds, group = field)) + 
  geom_line(alpha = .5) + 
  facet_wrap(~major_field, scales = "free_y")
p
#ggplotly(p)


## ----byfield-------------------------------------------------------------
phd_field %>% group_by(year, broad_field) %>% summarise(total = sum(n_phds, na.rm = T)) %>% 
  ggplot(aes(x = year, y = total, color = broad_field)) +
  geom_line()


## ----byfield2------------------------------------------------------------
phd_field %>% group_by(year, broad_field) %>% filter(!is.na(n_phds)) %>% count() %>% 
  ggplot(aes(year, n, color = broad_field)) + 
  geom_line() + 
  ggtitle("How many subfields are included in the broad fields?")


## ----byfield3------------------------------------------------------------
p <- phd_field %>% filter(broad_field == "Other") %>% 
  ggplot(aes(year, n_phds, group = field)) + 
  geom_line() + 
  facet_wrap(~major_field)
p
#ggplotly(p)

