## ------------------------------------------------------------------------
library(tidyverse)

combined_data <- readr::read_csv("https://raw.githubusercontent.com/5harad/openpolicing/master/results/data_for_figures/combined_data.csv")


## ------------------------------------------------------------------------
white<-combined_data%>%
  filter(driver_race=="White")%>%
  select(-c(consent_search_rate,citation_rate_speeding_stops,inferred_threshold))

hispanic<-combined_data%>%
  filter(driver_race=="Hispanic")%>%
  select(-c(consent_search_rate,citation_rate_speeding_stops,inferred_threshold))

black<-combined_data%>%
  filter(driver_race=="Black")%>%
  select(-c(consent_search_rate,citation_rate_speeding_stops,inferred_threshold))


## ------------------------------------------------------------------------
hispanic<-hispanic%>%
  mutate(white_stop_rate=white$stop_rate,white_search_rate=white$search_rate,white_spy=white$stops_per_year, war=white$arrest_rate)

black<-black%>%
  mutate(white_stop_rate=white$stop_rate,white_search_rate=white$search_rate,white_spy=white$stops_per_year, war=white$arrest_rate)

minority<-black%>%
  full_join(hispanic)


## ------------------------------------------------------------------------
ggplot()+
  geom_point(data=minority,mapping=aes(x=white_stop_rate,y=stop_rate),alpha=0.5,size=(minority$stops_per_year+minority$white_spy)/50000,na.rm=TRUE)+
  geom_abline(slope=1, intercept=0,linetype=2)+
  xlim(0,0.3)+
  ylim(0,0.3)+
  labs(title="Stop Rate Comparison of Driver Race by County", x="White Stop Rate",y="Minoroty Stop Rate",caption = "Source: Stanford Open Policing Project")+
  theme_bw()+
  facet_wrap(~driver_race)


## ------------------------------------------------------------------------
ggplot()+
         geom_point(data=combined_data,mapping=aes(x=search_rate,y=hit_rate,color=driver_race),alpha=0.5,size=combined_data$stops_per_year/10000,na.rm=TRUE)+
         xlim(0,0.1)+
         ylim(0,1)+
         labs(title="Search V Hit Rates",x="Search Rate",y="Hit Rate")+
         theme_bw()


## ------------------------------------------------------------------------
ggplot()+
  geom_point(data=minority,mapping=aes(x=stop_rate,y=arrest_rate,color=driver_race),alpha=0.3,size=(minority$stops_per_year)/15000,na.rm=TRUE)+
  labs(title="Stop Rate V Arrest Rate by County", x="Stop Rate",y="Arrest Rate",caption = "Source: Stanford Open Policing Project")+
  xlim(0,1)+
  theme_bw()


## ------------------------------------------------------------------------
ggplot()+
  geom_point(data=minority,mapping=aes(x=white_search_rate,y=search_rate),alpha=0.5,size=(minority$stops_per_year+minority$white_spy)/50000,na.rm=TRUE)+
  geom_abline(slope=1, intercept=0,linetype=2)+
  xlim(0,0.1)+
  ylim(0,0.15)+
  labs(title="Search Rate Comparison of Driver Race by County", x="White Search Rate",y="Minoroty Search Rate",caption = "Source: Stanford Open Policing Project")+
  theme_bw()+
  facet_wrap(~driver_race)


## ------------------------------------------------------------------------
ggplot()+
  geom_point(data=minority,mapping=aes(x=war,y=arrest_rate),alpha=0.5,size=(minority$stops_per_year+minority$white_spy)/50000,na.rm=TRUE)+
  geom_abline(slope=1, intercept=0,linetype=2)+
  xlim(0,0.05)+
  ylim(0,0.2)+
  labs(title="Arrest Rate Comparison of Driver Race by County", x="White Arrest Rate",y="Minoroty Arrest Rate",caption = "Source: Stanford Open Policing Project")+
  theme_bw()+
  facet_wrap(~driver_race)


## ------------------------------------------------------------------------
intra<-black%>%
  mutate(har=hispanic$arrest_rate,hsr=hispanic$stop_rate,hspy=hispanic$stops_per_year)
  
#plot to compare arrest rates among minorities
ggplot()+
  geom_point(data=intra,mapping=aes(x=har,y=arrest_rate),alpha=0.5,size=(intra$stops_per_year+intra$hspy)/50000,na.rm=TRUE)+
  geom_abline(slope=1, intercept=0,linetype=2)+
  xlim(0,0.2)+
  ylim(0,0.2)+
  labs(title="Arrest Rate Comparison Among Minorities", x="Hispanic Arrest Rate",y="Black Arrest Rate",caption = "Source: Stanford Open Policing Project")+
  theme_bw()

#plot to compare stop rates among minorities
ggplot()+
  geom_point(data=intra,mapping=aes(x=hsr,y=stop_rate),alpha=0.5,size=(intra$stops_per_year+intra$hspy)/50000,na.rm=TRUE)+
  geom_abline(slope=1, intercept=0,linetype=2)+
  xlim(0,0.2)+
  ylim(0,0.2)+
  labs(title="Stop Rate Comparison Among Minorities", x="Hispanic Stop Rate",y="Black Stop Rate",caption = "Source: Stanford Open Policing Project")+
  theme_bw()

