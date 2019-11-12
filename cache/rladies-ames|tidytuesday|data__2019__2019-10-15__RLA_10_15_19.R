## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)


## ----get_data------------------------------------------------------------
d <- read.csv('big_epa_cars.csv')


## ----basic_info----------------------------------------------------------
dim(d)
str(d)


## ----explore_makes_and_models--------------------------------------------

allmakes <- unique(d$make) %>% sort() 

d %>% 
  group_by(make,model,year) %>%
  select(make,model,year)



## ------------------------------------------------------------------------
t <- d %>% 
  filter(make=="Toyota", model=="Corolla") %>%
  select(year,make,model,trany,id,displ,youSaveSpend)

t_auto_1.6 <- t %>% filter(trany=="Automatic 4-spd",displ==1.6)


## ------------------------------------------------------------------------
t %>% ggplot(aes(year,youSaveSpend,group=interaction(trany,displ),color=interaction(trany,displ))) +
  geom_point() +
  geom_hline(yintercept=0, linetype="dashed", color = "red") +
  ylab("youSaveSpend ($)") +
  labs(color="transmition and engine displacement")


## ------------------------------------------------------------------------
t %>% 
  filter(trany %in% c("Automatic 3-spd","Automatic 4-spd","Automatic (AV-S10)","Automatic (S5)","Automatic (variable gear ratios)")) %>%
      ggplot(aes(year,youSaveSpend)) +
        geom_point() +
        geom_hline(yintercept=0, linetype="dashed", color = "red") +
        facet_wrap(trany~displ) +
        ylab("youSaveSpend ($)") +
        theme(legend.position = "none")

