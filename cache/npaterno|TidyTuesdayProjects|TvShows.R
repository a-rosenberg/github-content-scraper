## ------------------------------------------------------------------------
library(tidyverse)
tv_shows<-readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-08/IMDb_Economist_tv_ratings.csv")


## ------------------------------------------------------------------------
colnames(tv_shows)[colnames(tv_shows)=="title"] <- "Program"


## ------------------------------------------------------------------------
tv_seasons<-tv_shows%>%
     arrange(desc(seasonNumber))


## ------------------------------------------------------------------------
tv_longrun<-tv_seasons%>%
     filter(seasonNumber>=10)%>%
    group_by(Program)


## ------------------------------------------------------------------------
tv_longrun_unique<-distinct(tv_longrun,titleId)


## ------------------------------------------------------------------------
tv_lr<-tv_shows%>%
     filter(Program %in% tv_longrun_unique$Program)


## ------------------------------------------------------------------------
ggplot()+
     geom_line(data=tv_lr,mapping=aes(x=seasonNumber,y=av_rating,color=Program))+
      theme_bw()+
      xlab("Season")+
      ylab("Average Rating")


## ------------------------------------------------------------------------
comedy<-tv_lr%>%
     filter(str_detect(tv_lr$genres,"Comedy"))


## ------------------------------------------------------------------------
ggplot()+
     geom_line(data=comedy,mapping=aes(x=seasonNumber,y=av_rating,color=Program),na.rm=TRUE)+
      theme_bw()+
      xlim(0,15)+
      ylim(5,9)+
      xlab("Season")+
      ylab("Average Rating")


## ------------------------------------------------------------------------
genre<-separate_rows(tv_shows,genres,sep=",")%>%
  arrange(genres)%>%
  group_by(genres, seasonNumber)%>%
  summarize(mean=mean(av_rating))


## ------------------------------------------------------------------------
ggplot()+
  geom_line(data=genre,mapping=aes(x=seasonNumber,y=mean,color=genres))+
  theme_bw()+
  labs(title="Average Rating per Season by Genre",x="Season",y="Average Rating")


## ------------------------------------------------------------------------
genre_short<-genre%>%
  filter(seasonNumber<=10)%>%
  filter(!str_detect(genres,"Reality"))


## ------------------------------------------------------------------------
ggplot()+
  geom_line(data=genre_short,mapping=aes(x=seasonNumber,y=mean,color=genres))+
  theme_bw()+
  labs(title="Average Rating per Season (<10) by Genre",x="Season",y="Average Rating")

