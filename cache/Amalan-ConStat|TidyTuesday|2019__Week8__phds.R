## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,fig.width = 12,fig.height = 12,warning = FALSE,message = FALSE)


## ----load the packages and data------------------------------------------
# load the packages
library(tidyverse)
library(ggthemr)
library(readr)
library(gganimate)
library(ggridges)
library(ggalluvial)

ggthemr("flat")

#load the data
phdlist <- read_csv("phd_by_field.csv")


## ----Broad field boxplot-------------------------------------------------
p<-ggplot(phdlist,aes(x=str_wrap(broad_field,20),y=n_phds))+
          geom_boxplot()+
          xlab("Broad Field")+ylab("No of Phds")+
          transition_time(year)+ease_aes("linear")+
          ggtitle("Boxplot to Number of Phds in Broad Field",
              subtitle = "Year : {round(frame_time)}")+
          theme(axis.text.x = element_text(hjust=1,angle = 90))

animate(p,nframes=9,fps=1)


## ----Broad field boxplot without psy and soc sciences--------------------
p<-ggplot(subset(phdlist,broad_field != "Psychology and social sciences"),
          aes(x=str_wrap(broad_field,20),y=n_phds))+
          geom_boxplot()+
          xlab("Broad Field")+ylab("No of Phds")+
          transition_time(year)+ease_aes("linear")+
          ggtitle("Boxplot to Number of Phds in Broad Field",
              subtitle = "Year : {round(frame_time)}")+
          theme(axis.text.x = element_text(hjust=1,angle = 90))

animate(p,nframes=9,fps=1)



## ----major field boxplot-------------------------------------------------
p<-ggplot(phdlist,aes(x=str_wrap(major_field,20),y=n_phds,fill=broad_field))+
          geom_boxplot()+coord_flip()+
          xlab("Major Field")+ylab("No of Phds")+
          transition_time(year)+ease_aes("linear")+
          ggtitle("Boxplot to Number of Phds in Major Field",
              subtitle = "Year : {round(frame_time)}")+
          theme(axis.text.x = element_text(hjust=1,angle = 90),
                legend.position = "bottom")+
          labs(fill="Broad Field")

animate(p,nframes=9,fps=1)


## ----Major field without psy---------------------------------------------
q<-ggplot(subset(phdlist,major_field != "Psychology"),
          aes(x=str_wrap(major_field,20),y=n_phds,fill=broad_field))+
          geom_boxplot()+coord_flip()+
          xlab("Major Field")+ylab("No of Phds")+
          transition_time(year)+ease_aes("linear")+
          ggtitle("Boxplot to Number of Phds in Major Field",
              subtitle = "Year : {round(frame_time)}")+
          theme(axis.text.x = element_text(hjust=1,angle = 90),
                legend.position = "bottom")+
          labs(fill="Broad Field")

animate(q,nframes=9,fps=1)


## ----mathematics and cs bar chart----------------------------------------
subset(phdlist,broad_field == "Mathematics and computer sciences") %>%
      
ggplot(.,aes(x=factor(year),y=n_phds,fill=major_field))+
       geom_bar(stat="identity",position = "dodge")+
       theme(legend.position = "bottom")+
       xlab("Major Field")+ylab("Number of Phds")+
       ggtitle("Number of Phds awarded under Mathematics and CS",
               subtitle = "Year : 2008 to 2017")+
      scale_y_continuous(breaks=seq(0,1700,100),labels=seq(0,1700,100))+
          labs(fill="Major Field")
       


## ----major field boxplot with maths and cs-------------------------------
p<-ggplot(subset(phdlist,broad_field == "Mathematics and computer sciences"),
          aes(x=str_wrap(major_field,20),y=n_phds))+
          geom_boxplot()+
          xlab("Major Field")+ylab("No of Phds")+
          transition_time(year)+ease_aes("linear")+
          ggtitle("Boxplot to Number of Phds in Major Field",
              subtitle = "Year : {round(frame_time)}")

animate(p,nframes=9,fps=1)


## ----ridge major fields--------------------------------------------------
ggplot(subset(phdlist,broad_field == "Mathematics and computer sciences"),
          aes(y=str_wrap(major_field,20),x=n_phds))+
          geom_density_ridges()+
          xlab("No of Phds")+ ylab("Major Field")+
          theme(legend.position = "bottom")+
          ggtitle("Ridge plot for Major Fields in Mathematics and Computer Sciences",
                  subtitle = "Year : 2008 to 2017")


## ----field boxplot with maths and cs-------------------------------------
p<-ggplot(subset(phdlist,broad_field == "Mathematics and computer sciences"),
          aes(x=str_wrap(field,20),y=n_phds,fill=major_field))+
          geom_boxplot()+coord_flip()+
          xlab("Field")+ylab("No of Phds")+
          transition_time(year)+ease_aes("linear")+
          ggtitle("Boxplot to Number of Phds in Field",
              subtitle = "Year : {round(frame_time)}")+
          theme(legend.position = "bottom")+
          labs(fill="Major Field")

animate(p,nframes=9,fps=1)



## ----ridge plot fields---------------------------------------------------
ggplot(subset(phdlist,broad_field == "Mathematics and computer sciences"),
          aes(y=str_wrap(field,20),x=n_phds,fill=major_field))+
          geom_density_ridges()+
          xlab("No of Phds")+ ylab("Field")+
          theme(legend.position = "bottom")+
          ggtitle("Ridge plot for Fields in Mathematics and Computer Sciences",
                  subtitle = "Year : 2008 to 2017")+
          labs(fill="Major Field")


## ----broad and major and field-------------------------------------------
data.frame(subset(phdlist,broad_field=="Mathematics and computer sciences")) %>%
           na.omit() %>%
ggplot(.,aes(axis2=factor(str_wrap(year,10)), axis1= factor(str_wrap(major_field,10)), 
             axis3= factor(field), y=as.numeric(n_phds)))+
       scale_x_discrete(limits=c("Major Field","Year","Field"),expand = c(.05, .05))+
       geom_alluvium(aes(fill=factor(major_field)),width = 1/2)+
       geom_stratum(width=1/2,fill="white",color="grey")+
       geom_text(stat = "stratum", label.strata = TRUE)+
       theme(legend.position = "bottom")+ylab("No of Phds")+
       ggtitle("Major Field and Fields For Years 2008 to 2017",
               subtitle="Mathematics and Computer Science")+
          labs(fill="Major Field")

