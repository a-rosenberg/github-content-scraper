## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,message=FALSE,warning = FALSE,fig.height = 9,fig.width = 9)


## ----Prepare for TidyTuesday---------------------------------------------
# load the packages
library(readr)
library(tidyverse)
library(magrittr)
library(ggthemr)
library(lubridate)
library(stringr)
library(kableExtra)

#using theme
ggthemr("fresh")

#load data
NYC<-read_csv("nyc_restaurants.csv", 
    col_types = cols(inspection_date = 
                       col_date(format = "%m/%d/%Y")))
attach(NYC)


## ----Inspection Type,fig.height=12,fig.width=13--------------------------
#summary.factor(inspection_type)  %>%
#  sort()

# Bar plot for Insepction type
ggplot(NYC,aes(x=fct_infreq(str_wrap(inspection_type,35))))+
  geom_bar()+coord_flip()+
  scale_y_continuous(breaks = seq(0,200000,25000),labels = seq(0,200000,25000))+
  geom_text(stat = "count",aes(label=..count..),hjust=-0.005)+
  ylab("Frequency")+xlab("Type of Insepction")+
  ggtitle("Types of Inspection")


## ----Critical flag-------------------------------------------------------
#summary for critical flag
#NYC$critical_flag %>%
#      summary.factor() %>%
#      sum()
value=c(164623,6029,129348)

# creating data frame for Critical flag
CF<-data.frame(
              group=c("Critical","Not Applicable","Not Critical"),
              value=c(164623,6029,129348),
              per=round(value*100/300000,4)
              )

# pie chart for percentages
P1<-ggplot(CF,aes(x="",y=per,fill=group))+
    geom_col()+ theme_void()+
    geom_text(aes(label = scales::percent(per/100)), position = position_stack(vjust = 0.5)) +
    labs(title = "Critical Flag \nDistribution",fill="Type")+
    coord_polar(theta = "y",start = 0)

# pie chart for counts
P2<-ggplot(CF,aes(x="",y=value,fill=group))+
    geom_col()+theme_void()+
    labs(title = "Critical Flag\n Distribution",fill="Type")+ 
    geom_text(aes(label = value), position = position_stack(vjust = 0.5)) +
    coord_polar(theta = "y",start = 0)

gridExtra::grid.arrange(P1,P2,nrow=2)


## ----Cycle Inspection----------------------------------------------------
# subsetting data 
# Specific insepction type, critical flag and year with bar plot 
subset(NYC,inspection_type=="Cycle Inspection / Initial Inspection" |
           inspection_type=="Cycle Inspection / Re-inspection") %>%
ggplot(.,mapping=aes(x=str_wrap(inspection_type,8),fill=critical_flag))+
      geom_bar(position = "dodge",stat = "count")+
      facet_wrap(~year(inspection_date)) +
      xlab("Cycle Inspection")+
      ylab("Frequency")+
      ggtitle("Cycle Inspection over the years for Critical Flag")+
      labs(fill="Critical Flag")+
       geom_text(stat = "count",aes(label=..count..),
                 position = position_dodge(width = 1), vjust = -0.05)


## ----Pre-permit (operational)--------------------------------------------
# subsetting data 
# Specific insepction type, critical flag and year with bar plot 
subset(NYC,inspection_type=="Pre-permit (Operational) / Initial Inspection" |
           inspection_type=="Pre-permit (Operational) / Re-inspection") %>%
ggplot(.,aes(x=str_wrap(inspection_type,8),fill=critical_flag))+
       geom_bar(position = "dodge",stat = "count")+
      facet_wrap(~year(inspection_date)) +
      xlab("Pre-permit (Operational)")+
      ylab("Frequency")+
      ggtitle("Pre-permit (Operational) over the years for Critical Flag")+
      labs(fill="Critical Flag")+
      facet_wrap(~year(inspection_date)) +
       geom_text(stat = "count",aes(label=..count..),
                 position = position_dodge(width = 1), vjust = -0.05)


## ----Administrative Miscellaneous----------------------------------------
# subsetting data 
# Specific insepction type, critical flag and year with bar plot 
subset(NYC,inspection_type=="Administrative Miscellaneous / Initial Inspection" |
           inspection_type=="Administrative Miscellaneous / Re-inspection") %>%
ggplot(.,aes(x=str_wrap(inspection_type,8),fill=critical_flag))+
     geom_bar(position = "dodge",stat = "count")+
   facet_wrap(~year(inspection_date)) +
      xlab("Administrative Miscellaneous")+
      ylab("Frequency")+
      ggtitle("Administrative Miscellaneous over the years for Critical Flag")+
      labs(fill="Critical Flag")+
      facet_wrap(~year(inspection_date)) +
       geom_text(stat = "count",aes(label=..count..),
                 position = position_dodge(width = 1), vjust = -0.05)


## ----Most 5 Inspected Restaurants----------------------------------------
# Most 5 restaurants which were inspected
#kable(summary.factor(dba) %>%
#              sort() %>%
#              tail(5)
#      ,col.names = c("Frequency"),align = 'c') 


## ----Dunkin Donuts,out.height='70%',out.width='70%'----------------------
# subsetting Dunkin Donuts with boro
subset(NYC, dba=="DUNKIN' DONUTS") %>%
  ggplot(.,aes(x=boro))+
  geom_bar(position = "dodge",stat = "count")+
  geom_text(stat = "count",aes(label=..count..), vjust = -0.05)+
  ggtitle("How many Dunkin Donuts in Boro")+
  xlab("Boro")+ylab("Frequency")


## ----Dunkin Donuts, score and critical flag with cuisine description over years----
# Dunkin Donuts and scores with critical flag
subset(NYC, dba=="DUNKIN' DONUTS") %>%
ggplot(.,mapping=aes(y=score,color=critical_flag,x=factor(year(inspection_date))))+
      geom_jitter(alpha=0.3)+labs(color="Critical Flag")+    
      ggtitle("Dunkin Donuts score changes with Critical Flag for Cuisines")+
      xlab("Critical Flag")+ylab("Score")+
      scale_y_continuous(breaks = seq(0,60,5),labels =seq(0,60,5))+
      facet_wrap(~cuisine_description) 


## ----Subway,out.height='70%',out.width='70%'-----------------------------
# subsetting Subway with boro
subset(NYC, dba=="SUBWAY") %>% 
  ggplot(.,aes(x=boro))+
  geom_bar(position = "dodge",stat = "count")+
  geom_text(stat = "count",aes(label=..count..), vjust = -0.05)+
  ggtitle("How many Subways in Boro")+
  xlab("Boro")+ylab("Frequency")


## ----Subway, score and critical flag with cuisine description over years----
# Subway and scores with critical flag
subset(NYC, dba=="SUBWAY") %>%
ggplot(.,mapping=aes(y=score,color=critical_flag,x=factor(year(inspection_date))))+
      geom_jitter(alpha=0.3)+ labs(color="Critical Flag")+    
      ggtitle("Subway score changes with Critical Flag for Cuisines")+
      xlab("Critical Flag")+ylab("Score")+
      scale_y_continuous(breaks = seq(0,80,5),labels =seq(0,80,5))+
      facet_wrap(~cuisine_description) 


## ----McDonalds,out.height='70%',out.width='70%'--------------------------
# subsetting McDonalds with boro
subset(NYC, dba=="MCDONALD'S") %>% 
  ggplot(.,aes(x=boro))+
  geom_bar(position = "dodge",stat = "count")+
  geom_text(stat = "count",aes(label=..count..), vjust = -0.05)+
  ggtitle("How many McDonalds in Boro")+
  xlab("Boro")+ylab("Frequency")


## ----McDonalds scores, score and critical flag with cuisine description over years----
# McDonalds and scores with critical flag
subset(NYC, dba=="MCDONALD'S") %>%
ggplot(.,mapping=aes(y=score,color=critical_flag,x=factor(year(inspection_date))))+
      geom_jitter(alpha=0.3)+      
      labs(color="Critical Flag")+    
      ggtitle("McDonalds score changes with Critical Flag for Cuisines")+
      xlab("Critical Flag")+ylab("Score")+
      scale_y_continuous(breaks = seq(0,80,5),labels =seq(0,80,5))+
      facet_wrap(~cuisine_description) 


## ----Starbucks,out.height='70%',out.width='70%'--------------------------
#subsetting Starbucks with boro
subset(NYC, dba=="STARBUCKS") %>% 
  ggplot(.,aes(x=boro))+
  geom_bar(position = "dodge",stat = "count")+
  geom_text(stat = "count",aes(label=..count..), vjust = -0.05)+
  ggtitle("How many Starbucks in Boro")+
  xlab("Boro")+ylab("Frequency")


## ----Starbucks, score and critical flag with cuisine description over years----
# Starbucks and scores with critical flag
subset(NYC, dba=="STARBUCKS") %>%
ggplot(.,mapping=aes(y=score,color=critical_flag,x=factor(year(inspection_date))))+
      geom_jitter(alpha=0.3)+
      labs(color="Critical Flag")+    
      ggtitle("Starbucks score changes with Critical Flag for Cuisines")+
      xlab("Critical Flag")+ylab("Score")+
      scale_y_continuous(breaks = seq(0,80,5),labels =seq(0,80,5))+
      facet_wrap(~cuisine_description) 


## ----Kennedy Fried Chicken,out.height='70%',out.width='70%'--------------
#subsetting Kennedy Fried Chicken with boro
subset(NYC, dba=="KENNEDY FRIED CHICKEN") %>% 
  ggplot(.,aes(x=boro))+
  geom_bar(position = "dodge",stat = "count")+
  geom_text(stat = "count",aes(label=..count..), vjust = -0.05)+
  ggtitle("How many Kennedy Fried Chicken's in Boro")+
  xlab("Boro")+ylab("Frequency")


## ----Kennedy Fried Chicken, score and critical flag with cuisine description over years----
# Kennedy Fried Chicken and scores with critical flag
subset(NYC, dba=="KENNEDY FRIED CHICKEN") %>%
ggplot(.,mapping=aes(y=score,color=critical_flag,x=factor(year(inspection_date))))+
      geom_jitter(alpha=0.3)+     
      labs(color="Critical Flag")+    
      ggtitle("Kennedy Fried Chicken score changes with Critical Flag")+
      xlab("Critical Flag")+ylab("Score")+
      scale_y_continuous(breaks = seq(0,80,5),labels =seq(0,80,5))+
      facet_wrap(~cuisine_description) 

