## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)


## ------------------------------------------------------------------------
d <- read.csv('commute.csv')


## ------------------------------------------------------------------------
summary(d)


## ------------------------------------------------------------------------
d %>% filter(state=='Iowa',mode=='Bike') %>% 
  ggplot(aes(x=as.factor(city),y=percent,color=city)) +
  geom_point() +
  coord_flip()


## ------------------------------------------------------------------------
d %>% filter(state=='Iowa',mode=='Walk') %>% 
  ggplot(aes(x=as.factor(city),y=percent,color=city)) +
  geom_point() +
  coord_flip()

