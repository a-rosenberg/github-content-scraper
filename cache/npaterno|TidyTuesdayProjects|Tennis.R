#load libraries
library(tidyverse)
library(gganimate)

#load data
grand_slam_timeline <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slam_timeline.csv")

#Clean data to analyze number of tournaments played per player by year
gst_clean <- grand_slam_timeline %>%
  mutate(player=str_replace_all(player,"[[:punct:]+]","")) %>%
  #Above line: some players were entered with a / or // before there name.
  #This replaces those with a space.
  mutate(player=str_remove(player,"^\\s")) %>%
  #Above line: removes the space added above.
  mutate(player=iconv(player, from = 'UTF-8', to = 'ASCII//TRANSLIT')) %>%
  #Above line: converts all letters to Latin Alphabet
  filter(outcome!="Absent") %>%
  select(-tournament) %>%
  group_by(player,year,gender) %>%
  summarize(n=n())

#select the top 20 appearing players
top_players_all <-  gst_clean %>%
  group_by(player) %>%
  summarize(total=sum(n)) %>%
  ungroup() %>%
  top_n(20,total)

#select the top 20 players and calculate cumulative appearances by year
top_appearing <- gst_clean %>%
  filter(player %in% top_players_all$player) %>%
  group_by(player) %>%
  mutate(csum= cumsum(n))

#create base plot for animation
base_plot <- ggplot(top_appearing,aes(player,csum,fill=gender))+
  geom_col()+
  coord_flip()+
  theme_bw()

#create animation
base_plot+geom_text(aes(label = csum, hjust = -1))+
  transition_reveal(year,keep_last=TRUE)+
  labs(title="Number of Grand Slam Appearances: Top 20* Players", subtitle="Year: {round(frame_along,0)}",y="Number of Appearances",x="",caption="*includes ties| Source: Wikipedia| Graphic: @Mathl3t3")

#save animation
anim_save("top.gif")
