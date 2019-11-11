#tidytuesday 12032019

# Load Packages -----------------------------------------------------------
library(tidyverse)
library(reshape2)
library(gridExtra)


# Read in data ------------------------------------------------------------

importfile<-"tidytues2019/tt_mar19/tt_12032019/board_games.csv"

bgdata <- read_csv(importfile)

bgcategories<-str_split(bgdata$category,",")%>%
  unlist()%>%
  unique()

#Create new columns for each category  ------------------------------------------------------------------------
for (i in 1:length(bgcategories)){
  categoryis<-bgcategories[i]
  
  for (j in 1:nrow(bgdata)){
    cattrue<-grepl(categoryis, bgdata$category[j])
    if (isTRUE(cattrue)) {
      myval<-1
    } else {
      myval<-0
    }
    
    if(j==1){
      mycol<-myval
    }
    else{
      mycol<-rbind(mycol,myval)
    }
  }
  if(i==1){
    catcols<-as.data.frame(mycol)
  }
  else{
    catcols2<-as.data.frame(mycol)
    catcols<-cbind(catcols,catcols2)
  }
  
}
names(catcols)<-bgcategories
bgdata<-cbind(bgdata,catcols)


# create a new data frame with the game idsand a list of catergori --------

catsid <- cbind(catcols,bgdata$game_id)
long_cats <- melt(catsid, id="bgdata$game_id")
long_cats <- filter(long_cats,value=="1")
names(long_cats)[1]<-"game_id"

valid_column_names <- make.names(names=names(bgdata), unique=TRUE, allow_ = TRUE)
names(bgdata) <- valid_column_names

bgratings<-select(bgdata,game_id,name,year_published,average_rating)

bg_catdata<-left_join(long_cats, bgratings, by = "game_id")

# Create graphics ---------------------------------------------------------

exportfile<-"tidytues2019/tt_mar19/tt_12032019/12032019.png"

a<-ggplot(data = bg_catdata%>%filter(variable=="Religious"|variable=="Political"),aes(x=year_published,y=average_rating,colour=variable)) +
  geom_point()+
  geom_smooth(se=FALSE)+
  ylab("Average Rating")+
  theme_bw()
b<-ggplot(data = bg_catdata%>%filter(variable=="Word Game"|variable=="Number"),aes(x=year_published,y=average_rating,colour=variable)) +
  geom_point()+
  geom_smooth(se=FALSE)+
  ylab("Average Rating")+
  theme_bw()
c<-ggplot(data = bg_catdata%>%filter(variable=="Pirates"|variable=="Science Fiction"),aes(x=year_published,y=average_rating,colour=variable)) +
  geom_point()+
  geom_smooth(se=FALSE)+
  ylab("Average Rating")+
  theme_bw()
d<-ggplot(data = bg_catdata%>%filter(variable=="Humor"|variable=="Horror"),aes(x=year_published,y=average_rating,colour=variable)) +
  geom_point()+
  geom_smooth(se=FALSE)+
  ylab("Average Rating")+
  theme_bw()
p<-grid.arrange(a,b,c,d)

ggsave(exportfile,p)
