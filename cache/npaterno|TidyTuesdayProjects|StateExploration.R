## ------------------------------------------------------------------------
library(tidyverse)


## ------------------------------------------------------------------------
wa<-readr::read_csv("wa_statewide.csv")


## ------------------------------------------------------------------------
wa_clean<-wa%>%
  select(c(subject_race,subject_sex,county_name,search_conducted))%>%
           na.omit()


## ------------------------------------------------------------------------
wa_women_searched<-wa_clean%>%
  filter(subject_sex=="female")%>%
  group_by(county_name)%>%
  mutate(n=n())%>%
  filter(search_conducted==TRUE)%>%
  mutate(n_search=n(),search_rate=n_search/n)%>%
  unique()


## ------------------------------------------------------------------------
wa_men_searched<-wa_clean%>%
  filter(subject_sex=="male")%>%
  group_by(county_name)%>%
  mutate(n=n())%>%
  filter(search_conducted==TRUE)%>%
  mutate(n_search=n(),search_rate=n_search/n)%>%
  unique()


## ------------------------------------------------------------------------
wa_searched<-wa_men_searched%>%
  full_join(wa_women_searched)


## ------------------------------------------------------------------------
ggplot()+
  geom_col(data=wa_women_searched,mapping=aes(x=county_name,y=search_rate,fill=n))+
  coord_flip()+
  labs(title="Search Rates of Female Drivers Stopped by County",x="County",y="Search Rate",fill="Drivers Stopped",caption="Sourse: Stanford Open Policing Project")+
  theme_bw()+
    theme(text=element_text(size=8))

ggplot()+
  geom_col(data=wa_men_searched,mapping=aes(x=county_name,y=search_rate,fill=n))+
  coord_flip()+
  labs(title="Search Rates of Male Drivers Stopped by County",x="County",y="Search Rate",fill="Drivers Stopped",caption="Source: Stanford Open Policing Project")+
  theme_bw()+
    theme(text=element_text(size=8))

## ------------------------------------------------------------------------
ggplot()+
  geom_col(data=wa_women_searched,mapping=aes(x=county_name,y=search_rate,fill=n))+
  geom_col(data=wa_men_searched,mapping=aes(x=county_name,y=search_rate,fill=n),alpha=0.7)+
  coord_flip()+
  labs(title="Search Rates of Drivers Stopped by County",subtitle="(Faded Bars for Men)",x="County",y="Search Rate",fill="Drivers Stopped",caption="Source: Stanford Open Policing Project")+
  theme_bw()+
  theme(text=element_text(size=8))

