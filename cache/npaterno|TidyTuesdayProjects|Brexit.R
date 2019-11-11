library(tidyverse)
library(scales)
library(lubridate)

#load data
brexit <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/brexit.csv")

#clean data to calculate boxplots for right/wrong votes
brexit_new <- brexit %>% 
  rename(Right = percent_responding_right) %>% 
  rename(Wrong = percent_responding_wrong) %>% 
  gather(Right,Wrong,key = "Vote", value="Percent") %>% 
  mutate(Date=dmy(date))

#plot right v wrong violin plots
ggplot(brexit_new,aes(x=Vote,y=Percent/100))+
  geom_violin(color="black",
              fill="light blue")+
  geom_jitter(width=0.2,aes(color=Date))+
  scale_y_continuous(labels = percent_format())+
  theme_bw()+
  labs(title="Was Britian Right in Voting to Leave the EU?",
       subtitle="(Polls represented by dots)",
       x="Vote",
       y="Percent",
       caption="Source: The Economist | Graphic: @Mathl3t3")
