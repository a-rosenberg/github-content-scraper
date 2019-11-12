## ----dl_dataset, include=FALSE, echo=TRUE--------------------------------
library(tidyverse)
library(openxlsx)
library(cowplot)

Dallas_Shelter<-read.xlsx("https://github.com/rfordatascience/tidytuesday/raw/master/data/week18_dallas_animals.xlsx",sheet = 2)


## ----dataset_str---------------------------------------------------------
str(Dallas_Shelter)


## ----Recidivism----------------------------------------------------------
MultipleVisits<-Dallas_Shelter%>%
  group_by(animal_id)%>%
  filter(n()>1)%>%
  ungroup()

singleVisit<-Dallas_Shelter%>%
  group_by(animal_id)%>%
  filter(n()==1)%>%
  ungroup()

Recidivism_first<-MultipleVisits%>%
  group_by(animal_id)%>%
  filter(intake_date==min(intake_date))%>%
  filter(row_number()==1 )%>%
  filter(!(outcome_type%in%c('EUTHANIZED','DEAD ON ARRIVAL')))%>% # Some issues with multiple rows for the same animal
  ungroup()

SummarizeRecidivism_first<-Recidivism_first%>%
  group_by(intake_type,outcome_type)%>%
  summarise(count=n())%>%
  ungroup()%>%
  mutate(outcome_type=as.factor(outcome_type))

SummarizeRecidivism_last<-MultipleVisits%>%
  group_by(animal_id)%>%
  filter(intake_date==max(intake_date))%>%
  filter(row_number()==1 )%>%
  ungroup()%>%
  group_by(intake_type,outcome_type)%>%
  summarise(count=n())%>%
  ungroup()%>%
  mutate(outcome_type=as.factor(outcome_type))

intake_typeLevels<-unique(c(SummarizeRecidivism_first$intake_type,SummarizeRecidivism_last$intake_type))


## ----plot_recidivism-----------------------------------------------------
first_record<-ggplot(SummarizeRecidivism_first,
                    aes(x=outcome_type,y=factor(intake_type,levels=intake_typeLevels)))+
  geom_tile(aes(fill=count),show.legend = FALSE)+
  geom_text(aes(label=count,color=ifelse(count>400,0,1)),show.legend = FALSE)+
  scale_x_discrete(position = "top")+
  theme(axis.text.x = element_text(angle = -40, hjust = 1))+
  theme(axis.text.y = element_text(angle = -40, hjust = 1, vjust = 0))+
  xlab("First Visit Outcome")+
  ylab("In Status")
print(first_record)

second_record<-ggplot(SummarizeRecidivism_last,
                   aes(x=outcome_type,y=factor(intake_type,levels=intake_typeLevels)))+
  geom_tile(aes(fill=count),show.legend = FALSE)+
  geom_text(aes(label=count,color=ifelse(count>400,0,1)),show.legend = FALSE)+
  scale_x_discrete(position = "top")+
  theme(axis.text.x = element_text(angle = -40, hjust = 1))+
  theme(axis.text.y = element_text(angle = -40, hjust = 1, vjust = 0))+
  xlab('Last Recorded Outcome')+
  ylab("In Status")
print(second_record)




## ----plot_onevisit,fig.height=9,fig.width=12-----------------------------
SummarizeSingleVisit<-singleVisit%>%
  filter(intake_type!="WILDLIFE")%>%
  filter(!animal_type%in%c("LIVESTOCK","WILDLIFE"))%>%
  group_by(animal_id)%>%
  filter(intake_date==max(intake_date))%>%
  filter(row_number()==1 )%>%
  ungroup()%>%
  group_by(intake_type,outcome_type,animal_type)%>%
  summarise(count=n())%>%
  ungroup()%>%
  mutate(outcome_type=as.factor(outcome_type))

single_visit<-ggplot(SummarizeSingleVisit,
                   aes(x=outcome_type,y=factor(intake_type,levels=intake_typeLevels)))+
  geom_tile(aes(fill=count),show.legend = FALSE)+
  geom_text(aes(label=count,color=ifelse(count>1000,0,1)),show.legend = FALSE)+
  scale_x_discrete(position = "top")+
  theme(axis.text.x = element_text(angle = -40, hjust = 1))+
  xlab('Outcome')+
  ylab("In Status")+
  facet_grid(.~animal_type)
print(single_visit)




## ----recidivism_cowplot, echo=FALSE--------------------------------------


second_record_noY<-ggplot(SummarizeRecidivism_last,
                   aes(x=outcome_type,y=factor(intake_type,levels=intake_typeLevels)))+
  geom_tile(aes(fill=count),show.legend = FALSE)+
  geom_text(aes(label=count,color=ifelse(count>400,0,1)),show.legend = FALSE)+
  scale_x_discrete(position = "top")+
  theme(axis.text.x = element_text(angle = -40, hjust = 1),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())+
  xlab('Last Recorded Outcome')+
  ylab("In Status")

# now add the title
title <- ggdraw() + draw_label("Dallas Animal Shelter Statistics", fontface='bold')

gg<-plot_grid(title, 
              plot_grid(first_record, second_record_noY),
              ncol=1, rel_heights=c(0.1, 1)) # rel_heights values control title margins
ggsave(gg,filename = "Animal_Recidivism.PNG",height = 9,width = 13)

