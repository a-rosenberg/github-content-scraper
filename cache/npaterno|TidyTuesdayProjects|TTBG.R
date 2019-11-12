## ------------------------------------------------------------------------
library(tidyverse)

board_games <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-12/board_games.csv")


## ------------------------------------------------------------------------
bg_fam<-board_games %>%
  filter(max_playtime<=180 & max_players<=8)


## ------------------------------------------------------------------------
colnames(bg_fam)[colnames(bg_fam)=="max_players"] <- "Players"


## ------------------------------------------------------------------------
ggplot(bg_fam,aes(x=year_published,y=average_rating,color=playing_time))+
  geom_point(alpha=0.2)+
  facet_wrap(~ Players, labeller=label_both)+
  scale_color_gradient(name="Playing Time",low="blue",high="red")+
  theme_bw()+
  xlab("Year")+
  ylab("Average Rating")


## ------------------------------------------------------------------------
bg_year<-board_games %>%
  group_by(year_published)%>%
  summarize(mean=mean(average_rating, na.rm=TRUE))


## ------------------------------------------------------------------------
ggplot()+
  geom_point(data=board_games, mapping=aes(x=year_published,y=average_rating,color=min_age),alpha=0.2)+
  geom_line(data=bg_year,mapping=aes(x=year_published,y=mean))+
  xlab("Year")+
  ylab("Average Rating")+
  theme_bw()+
  scale_color_gradient(name="Minimum Age", low="blue", high="red")


## ------------------------------------------------------------------------
bg_card<-board_games %>%
  filter(str_detect(category, "Card"))


## ------------------------------------------------------------------------
bg_card_year <-bg_card%>%
  group_by(year_published)%>%
  summarize(mean=mean(average_rating),na.rm=TRUE)


## ------------------------------------------------------------------------
ggplot()+
  geom_hex(data=bg_card,aes(x=year_published,y=average_rating))+
  geom_line(data=bg_card_year,mapping=aes(x=year_published,y=mean))+
  xlab("Year")+
  ylab("Average Rating")+
  theme_bw()


## ------------------------------------------------------------------------
bg_dice<-board_games %>%
  filter(str_detect(category,"Dice"))


## ------------------------------------------------------------------------
bg_dice_year<-bg_dice %>%
  group_by(year_published)%>%
  summarize(mean=mean(average_rating),na.rm=TRUE)


## ------------------------------------------------------------------------
ggplot()+
  geom_hex(bg_dice,mapping=aes(x=year_published,y=average_rating))+
  geom_line(bg_dice_year,mapping=aes(x=year_published,y=mean))+
  xlab("Year")+
  ylab("Average Rating")+
  theme_bw()

