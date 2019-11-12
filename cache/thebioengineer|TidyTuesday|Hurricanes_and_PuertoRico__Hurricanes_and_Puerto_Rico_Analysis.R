## ----dl_dataset, include=FALSE-------------------------------------------
library(tidyverse)

Hurricane_Coverage_Online<-read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/week12_mediacloud_hurricanes.csv")%>%
   mutate(Date=as.Date(Date,"%m/%d/%y"))

Hurricane_Google_Trends<-read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/week12_google_trends.csv",skip = 2)%>%
  rename(Date="Day")%>%
   mutate(Date=as.Date(Date,"%m/%d/%y"))



## ----initial_look--------------------------------------------------------

coverage<-merge(Hurricane_Coverage_Online,Hurricane_Google_Trends,by="Date")

head(coverage)



## ----Plotting, echo=FALSE, warning=FALSE---------------------------------

coveragePlot<-coverage%>%
  {lapply(c("Harvey","Irma","Maria","Jose"),
         function(Hurricane,data){
           data[,c(1,grep(Hurricane,colnames(data)))]%>%
             set_names(c("Date","Media_Coverage","Google_Trends"))%>%
             mutate(Hurricane=Hurricane)
         }
         ,.)%>%{do.call('rbind',.)}}%>%
  ggplot(aes(x=Date))+
  geom_point(aes(y=Media_Coverage,size=Google_Trends,color=Hurricane))+
  ggthemes::theme_fivethirtyeight()+
  ggtitle("Media Coverage over time,\ncompared to Google Search Trends")

coveragePlot


## ----save_vis, echo=FALSE------------------------------------------------
png("Hurricane_Media_and_GoogleTrends.png",width = 500,height=500)
coveragePlot
dev.off()
  

