#tidytuesday 29012019
rm(list=ls())
library(tidyverse)

importfile<-"tidytues2019/tt_jan19/tt_29012019/clean_cheese.csv"
exportfile<-"tidytues2019/tt_jan19/tt_29012019/29012019.png"

chdata <- read_csv(importfile)

chdata<-mutate(chdata,
       OtherCheese=rowSums(chdata[6:11],na.rm = TRUE)
       )

ggplot(data = chdata,aes(x=Year)) +
  geom_line(mapping = aes(y=Cheddar), colour="deepskyblue3")+
  geom_line(mapping = aes(y=Mozzarella), colour="deeppink3")+
  geom_line(mapping = aes(y=`American Other`), colour="yellow3")+
  geom_line(mapping = aes(y=`Italian other`), colour="forestgreen")+
  geom_line(mapping = aes(y=OtherCheese), colour="chocolate3")+
  ylab("Per capita consumption ofcheese / pounds")+
  geom_text(aes(x = 2011, y = 11.5, label = "Mozzarella"), color = "deeppink3") + 
  geom_text(aes(x = 2011, y = 9, label = "Cheddar"), color = "deepskyblue3")+
  geom_text(aes(x = 2011, y = 4.5, label = "Other American Type Cheeses"), color = "yellow3") + 
  geom_text(aes(x = 2011, y = 2, label = "Other Italian Type Cheeses"), color = "forestgreen")+
  geom_text(aes(x = 2011, y = 6.5, label = "Other Cheeses"), color = "chocolate3")+
  theme_bw()

ggsave(exportfile)
