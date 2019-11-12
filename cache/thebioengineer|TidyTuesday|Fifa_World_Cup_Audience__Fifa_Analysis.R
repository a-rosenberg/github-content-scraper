## ----dl_dataset, include=FALSE-------------------------------------------
library(tidyverse)
fifa_audience<-read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/week11_fifa_audience.csv")


## ----initial_look--------------------------------------------------------
str(fifa_audience)


## ----Rankings, echo=TRUE, warning=FALSE----------------------------------
library(rvest)

extractRankings<-function(url,country){
  
  urlhtml<-read_html(url)
  
  Rankings <- urlhtml%>%
    html_nodes(".col-xs-12")%>%
    html_nodes(".tbl-ranking")%>%
    html_table()%>%as.data.frame()%>%
    select(Rank,Date)
  
  Recent_Rankings<-Rankings%>%
    filter(Date<=2010, Date >= 2005)%>%
    {
      tmp<-rep(NA,6)
      names(tmp)<-paste0("Ranking_",2005:2010)
      if(nrow(.)>0){
        tmp[which(names(tmp)%in%paste0("Ranking_",.$Date))]<-.$Rank
      }
      return(tmp)
      }%>%
    as.data.frame%>%t
  
  Historical_Ranking<-Rankings%>%
    filter(Date<=2010)%>%
    data.frame%>%
    `$`("Rank")%>%
    as.numeric%>%
    mean(na.rm = TRUE)
  
  data.frame(Country=country,Recent_Rankings,Historical_Ranking=Historical_Ranking)
}

available_countries<-read_html("https://www.fifa.com/fifa-world-ranking/associations/")%>%
  html_nodes(".ranking-teamlist") %>%
  html_nodes("a") %>%
  {data.frame(Country=html_text(.),link=html_attr(.,"href"), stringsAsFactors = FALSE)}%>%
  mutate(url=paste0("https://www.fifa.com",link))%>%
  select(Country,url)%>%
  mutate(Country=dplyr::recode(Country,
                               USA="United States",
                               England="United Kingdom",
                               "China PR"="China",
                               "Korea Republic"="South Korea"
                               ))

available_countries%>%
  filter(Country %in% fifa_audience$country)%>%dim

rankings<-available_countries%>%
  filter(Country %in% fifa_audience$country)%>%
  split(., seq(nrow(.)))%>%
  map_df(function(.x){
    rankings<-try(extractRankings(.x$url,.x$Country))
    if(inherits(rankings,"try-error")){
      data.frame(Country=.x$Country,NA,NA,NA,NA,NA,NA,NA)
    }else{
      rankings
    }})%>%
  rename(country=Country)

str(rankings)



## ----Modeling, echo=FALSE, warning=FALSE---------------------------------
library(car)

FIFA<-merge(fifa_audience,rankings,by="country")

rankingsLM<-lm(tv_audience_share~Historical_Ranking+Ranking_2010+Ranking_2009+Ranking_2008+Ranking_2007+Ranking_2006+Ranking_2005, data=FIFA)

Anova(rankingsLM, type="III")



## ----visualization_1-----------------------------------------------------

library(ggrepel)


FIFA_PLOT<-ggplot(FIFA)+
  geom_point(aes(x=Historical_Ranking,y=Ranking_2009,color=tv_audience_share,size=population_share))+
  geom_abline(slope=1, intercept=0, color="red")+
  scale_x_reverse()+scale_y_reverse()+
  geom_label_repel(aes(label=country,x=Historical_Ranking,y=Ranking_2009),
                   data=FIFA[which(FIFA$tv_audience_share>3 | FIFA$population_share>5),],
                   arrow = arrow(length = unit(0.03, "npc"), type = "closed", ends = "last"),
                   force = 100,
                   nudge_x = 40)+
  ggtitle("FIFA TV Viewer Share by Country")


FIFA_PLOT


## ----save_vis, echo=FALSE------------------------------------------------
png("FIFA_Viewship.PNG",width = 1000,height=1000)
FIFA_PLOT
dev.off()
  

