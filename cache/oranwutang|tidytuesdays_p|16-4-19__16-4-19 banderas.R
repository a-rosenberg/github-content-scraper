library(tidyverse)
library(magrittr)
library(ggthemes)
library(devtools)
#install_github("rensa/ggflags")
library(ggflags)

women_research <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/women_research.csv")
women_research$country[grepl("United Kingdom",women_research$country)] <- "UK"
women_research$country[grepl("United States",women_research$country)] <- "US"
women_research$field[grepl("Women inventores",women_research$field)] <- "Inventors"

wr<-women_research
wr$country[grepl("Australia", wr$country)]<-"au"
wr$country[grepl("Chile", wr$country)]<-"cl"
wr$country[grepl("France", wr$country)]<-"fr"
wr$country[grepl("Portugal", wr$country)]<-"pt"
wr$country[grepl("Brazil", wr$country)]<-"br"
wr$country[grepl("Denmark", wr$country)]<-"dk"
wr$country[grepl("Japan", wr$country)]<-"jp"
wr$country[grepl("UK", wr$country)]<-"gb"
wr$country[grepl("Canada", wr$country)]<-"ca"
wr$country[grepl("EU28", wr$country)]<-"eu"
wr$country[grepl("Mexico", wr$country)]<-"mx"
wr$country[grepl("US", wr$country)]<-"us"
wr$field[grepl("Computer science, maths", wr$field)]<-"Computer science & Maths"


wr %>% ggplot(aes(x=reorder(field, percent_women), y=percent_women*100, country=country), stringsAsFactors = FALSE)+
  geom_flag(size=8)+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y = element_blank(),
        legend.position = "NA",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 55, hjust = 1, size = 11),
        panel.background = element_rect(fill = "#6D9EC1"),
        plot.title = element_text( size=16, face="bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank())+
  geom_hline(yintercept = 50, linetype="dashed", color = "red")+
  labs(title = "Still a Man's World",
       subtitle = "Women among researchers\nwith papers published\n2011-15; % of total",
       caption="\"Inventors\": Who filled patent applications\nSources: Gender in the Global Research Landscape by Elsevier;\nThe Economist")
  
  
