## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,fig.width = 9,fig.height = 7)

library(readr)
library(tidyverse)
library(stringr)
library(ggthemr)
library(gganimate)
library(formattable)

ggthemr("flat dark")

bridges <- read_csv("baltimore_bridges.csv")
#View(bridges)

names(bridges)<-c("lat","long","County","Carries","Year Built","Condition","Average Daily Traffic","Total Improvement","Month","Year","Owner","Responsibility","Vehicles")
attach(bridges)


## ----County with Condition and Average Daily Traffic SHA-----------------
ggplot(subset(bridges,Owner=="State Highway Agency")
       ,aes(color=Condition,y=`Average Daily Traffic`,x=str_wrap(County,7)))+xlab("County")+
  ggtitle("Condition of Bridges owned by State Highway Agency \nand their Average Daily Traffic")+
  scale_y_continuous(labels =seq(0,230000,10000) ,breaks = seq(0,230000,10000))+
  geom_jitter()+transition_states(Condition,transition_length = 2,state_length = 3)+
  enter_fade()+exit_shrink()+ease_aes("back-in")


## ----County with Condition and Average Daily Traffic CHA-----------------
ggplot(subset(bridges,Owner=="County Highway Agency")
       ,aes(color=Condition,y=`Average Daily Traffic`,x=str_wrap(County,7)))+xlab("County")+
  ggtitle("Condition of Bridges owned by County Highway Agency \nand their Average Daily Traffic")+
  scale_y_continuous(labels =seq(0,40000,5000) ,breaks = seq(0,40000,5000))+
  geom_jitter()+transition_states(Condition,transition_length = 2,state_length = 3)+
  enter_fade()+exit_shrink()+ease_aes("back-in")


## ----County with Condition and Average Daily Traffic STA-----------------
ggplot(subset(bridges,Owner=="State Toll Authority")
       ,aes(color=Condition,y=`Average Daily Traffic`,x=str_wrap(County,7)))+xlab("County")+
  ggtitle("Condition of Bridges owned by State Toll Authority \nand their Average Daily Traffic")+
  scale_y_continuous(labels =seq(0,170000,10000) ,breaks = seq(0,170000,10000))+
  geom_jitter()+transition_states(Condition,transition_length = 2,state_length = 3)+
  enter_fade()+exit_shrink()+ease_aes("back-in")


## ----Year Built with Condition and Average Daily Traffic-----------------
ggplot(subset(bridges,`Year Built`=="1957" | `Year Built`=="1970" | `Year Built`=="1975" | `Year Built`=="1991" 
              | `Year Built`=="1963" | `Year Built`=="1961")
       ,aes(color=Condition,y=`Average Daily Traffic`,x=factor(`Year Built`)))+
  xlab("Year Built")+ylab("Average Daily Traffic")+
  ggtitle("Most amount of Bridges built based on Years \nand their Conditions")+
  geom_jitter()+legend_bottom()+transition_states(Condition,transition_length = 2,state_length = 3)+
  enter_fade()+exit_shrink()+ease_aes("back-in")


## ----Average Traffic less than 100000------------------------------------
ggplot(subset(bridges, `Average Daily Traffic` <= 100000 & County != "Baltimore city"),aes(x=County,y=`Average Daily Traffic`,color=Condition))+
  xlab("County")+ylab("Averag Daily Traffic")+ggtitle("Average Daily Traffic Less than 100,000 \nFor Counties")+
  scale_y_continuous(labels = seq(0,100000,5000),breaks = seq(0,100000,5000))+coord_flip()+
  theme(axis.text.x = element_text(angle = -90))+
  geom_jitter()+transition_states(Condition,transition_length = 2,state_length = 3)+
  enter_fade()+exit_shrink()+ease_aes("back-in")



## ----Average Traffic more than 100000------------------------------------
ggplot(subset(bridges, `Average Daily Traffic` > 100000 & County != "Baltimore city"),aes(x=County,y=`Average Daily Traffic`,color=Condition))+
  xlab("County")+ylab("Average Daily Traffic")+ggtitle("Average Daily Traffic More than 100,000 \nFor Counties")+
  scale_y_continuous(labels = seq(100000,230000,5000),breaks = seq(100000,230000,5000))+coord_flip()+
  theme(axis.text.x = element_text(angle = -90))+
  geom_jitter()+transition_states(Condition,transition_length = 2,state_length = 3)+
  enter_fade()+exit_shrink()+ease_aes("back-in")



## ----Bridge Condition and County with Improvement Next 7-----------------
Top10<-subset(bridges[,c(-1,-2,-9,-10,-11,-12,-13)], `Total Improvement` > 9999 & `Total Improvement` < 30000)

customRed0 = "#FF8080"
customRed = "#7F0000"

customyellow0 = "#FFFF80"
customyellow = "#BFBF00"

customblue0 = "#6060BF"
customblue =  "#00007F"

formattable(Top10,align=c("l","l","c","c","c","c"),
            list(
              County =formatter("span",style= ~style(color="grey")),
            `Total Improvement`=color_tile(customblue0,customblue),
            `Average Daily Traffic`=color_tile(customyellow0,customyellow),
            `Year Built`=color_tile(customRed0,customRed)
            ))


## ----Bridge Condition and County with Improvement Top 3------------------
Top3<-subset(bridges[,c(-1,-2,-9,-10,-11,-12,-13)], `Total Improvement` >= 30000)

customRed0 = "#FF8080"
customRed = "#7F0000"

customyellow0 = "#FFFF80"
customyellow = "#BFBF00"

customblue0 = "#6060BF"
customblue =  "#00007F"

formattable(Top3,align=c("l","l","c","c","c","c"),
            list(
              County =formatter("span",style= ~style(color="black")),
            `Total Improvement`=color_tile(customblue0,customblue),
            `Average Daily Traffic`=color_tile(customyellow0,customyellow),
            `Year Built`=color_tile(customRed0,customRed)
            ))

