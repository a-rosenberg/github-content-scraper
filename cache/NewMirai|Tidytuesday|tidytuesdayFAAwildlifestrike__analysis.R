## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------
library(tidyverse)
library(visdat)
library(skimr)
library(RColorBrewer)
library(glue)
library(cowplot)
library(magick)
library(lubridate)
library(paletteer)
library(ggforce)


## ----message=FALSE, warning=FALSE, include=FALSE-------------------------
link <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-23/wildlife_impacts.csv"


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------
wildlife_impacts <- readr::read_csv(link)


## ----echo=FALSE, fig.height=5, fig.width=10, message=FALSE, warning=FALSE----
visdat::vis_miss(wildlife_impacts,warn_large_data = F)


## ----message=FALSE, warning=FALSE, include=FALSE-------------------------
skim_to_list(wildlife_impacts)




## ----echo=FALSE, fig.height=9, fig.width=16, message=FALSE, warning=FALSE----
proportion <- wildlife_impacts %>% 
  select(incident_date,operator,state,damage,time_of_day,atype,time_of_day,phase_of_flt) %>% 
  mutate(time_of_day=if_else(is.na(time_of_day),"Unknown",time_of_day),
         damage=case_when(
           damage=="M"~ "Minor",
           damage=="M?"~ "Minor Uncertain",
           damage=="N"~ "None",
           damage=="S"~ "Substantial",
           damage=="D"~ "Destroyed",
           is.na(damage)~ "Unknown Damage Category"
         )) %>% 
  group_by(time_of_day,damage) %>% 
  tally()

nonedmg <- 1.83+2.53+19.63+24.15+37.25

p1 <- proportion %>% 
  ggplot(aes(x=time_of_day,y=n,color=time_of_day))+
  geom_segment(aes(y=0,yend=n,x=time_of_day,xend=time_of_day),colour="black")+
  geom_point(size=6)+
  geom_mark_circle(aes(label = "Highest frequency", description = glue("{nonedmg} % of incident implies none damage and 37,25 % of incident happens during the day and implied none damage!"),color=time_of_day,
                       filter = time_of_day == 'Day' & damage=="None"))+
  labs(x="",
       y="# of incident",
       title = "Incident per period of day and damage category\n",
       color="Period of the day",
       caption = "Author: @alangel12407606 | #TidyTuesday")+
  paletteer::scale_colour_paletteer_d(package = "ggsci",palette = "nrc_npg")+
  scale_y_continuous(breaks = seq(0,25000,1000),limits = c(0,25000))+
  theme_minimal()+
  facet_wrap(~damage)+
  theme(legend.position = c(0.85,0.25),
        plot.title = element_text(size=20,hjust = .5),
        plot.subtitle = element_text(size=12,hjust = .5),
        legend.title = element_text(size = 16),
        legend.title.align = 0.5,
        strip.text.x = element_text(size = 14),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = NA,colour = "black"),
        panel.grid.major = element_line(size=.25,color="#230699",linetype = "dotted"),
        axis.title.x = element_text(size = 14,vjust = -4),
        axis.title.y = element_text(size=14,vjust = 2),
        plot.caption = element_text(size=10),
        legend.text = element_text(size = 13,vjust = .5),
        legend.spacing.x = unit(4,"mm") ,
        legend.background = element_rect(colour = "#1D0D6E",size=.6),
        strip.background = element_rect(fill = "#f2f2f2"))

p1


## ----include=FALSE-------------------------------------------------------

ggsave("p1.png",dpi = 400,width = 16,height = 9)

