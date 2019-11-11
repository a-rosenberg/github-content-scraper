library(tidyverse)
library(lubridate)
library(openintro) #convert state abb to full


wildlife_impacts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-23/wildlife_impacts.csv")


wildlife <- wildlife_impacts %>%
  filter(state != "N/A") %>% 
  filter(state != "NA") %>%
  filter(state != "AC") %>%
  mutate(year  = year(incident_date)) %>%
  mutate(state = abbr2state(state)) %>%
  select(state, year, operator)
#American Airlines

AA <- wildlife %>%
  filter(operator == "AMERICAN AIRLINES") %>%
  filter(state != "NA") %>%
  group_by(year, state) %>%
  tally()
base_size <- 9
#plot american
ggplot(data = AA, mapping = aes(x = year, y = state,  fill = n)) +
  geom_tile() + 
  theme_grey(base_size = base_size) + 
  scale_fill_gradient(low="#ece7f2", high="#2b8cbe", limits = c(1,250)) +
  labs(x = "Year", y = "State", title = "American Airlines Birdstrikes") + 
  theme(panel.grid.major.x=element_blank(), #no gridlines
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.y=element_blank(), 
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"), # background=white
        axis.text.x = element_text(angle=45, hjust = 1,vjust=1,size = 9,face = "bold"),
        plot.title = element_text(size=16,face="bold"),
        axis.text.y = element_text(size = 8)) +
  labs(fill="Birdstrikes") 

ggsave("2019-07-23/american.png", width = 7, height = 7)

#United Airlines
united <- wildlife %>%
  filter(operator == "UNITED AIRLINES") %>%
  filter(state != "NA") %>%
  group_by(year, state) %>%
  tally()

#plot united
ggplot(data = united, mapping = aes(x = year, y = state,  fill = n)) +
  geom_tile() + 
  theme_grey(base_size = base_size) + 
  scale_fill_gradient(low="#ffeda0", high="#f03b20", limits = c(1,250)) +
  labs(x = "Year", y = "State", title = "United Airlines Birdstrikes") + 
  theme(panel.grid.major.x=element_blank(), #no gridlines
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.y=element_blank(), 
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"), # background=white
        axis.text.x = element_text(angle=45, hjust = 1,vjust=1,size = 9,face = "bold"),
        plot.title = element_text(size=16,face="bold"),
        axis.text.y = element_text(size = 8)) +
  labs(fill="Birdstrikes") 
ggsave("2019-07-23/united.png", width = 7, height = 7)

#Delta
delta <- wildlife %>%
  filter(operator == "DELTA AIR LINES") %>%
  filter(state != "NA") %>%
  group_by(year, state) %>%
  tally()

#plot delta
ggplot(data = delta, mapping = aes(x = year, y = state,  fill = n)) +
  geom_tile() + 
  theme_grey(base_size = base_size) + 
  scale_fill_gradient(low="#e5f5e0", high="#31a354", limits = c(1,250)) +
  labs(x = "Year", y = "State", title = "Delta Airlines Birdstrikes") + 
  theme(panel.grid.major.x=element_blank(), #no gridlines
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.y=element_blank(), 
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"), # background=white
        axis.text.x = element_text(angle=45, hjust = 1,vjust=1,size = 9,face = "bold"),
        plot.title = element_text(size=16,face="bold"),
        axis.text.y = element_text(size = 8)) +
  labs(fill="Birdstrikes") 
ggsave("2019-07-23/delta.png", width = 7, height = 7)

#Southwest
southwest<- wildlife %>%
  filter(operator == "SOUTHWEST AIRLINES") %>%
  filter(state != "NA") %>%
  group_by(year, state) %>%
  tally()

#plot southwest
ggplot(data = southwest, mapping = aes(x = year, y = state,  fill = n)) +
  geom_tile() + 
  theme_grey(base_size = base_size) + 
  scale_fill_gradient(low="#e7e1ef", high="#dd1c77", limits = c(1,250)) +
  labs(x = "Year", y = "State", title = "Southwest Airlines Birdstrikes") + 
  theme(panel.grid.major.x=element_blank(), #no gridlines
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.y=element_blank(), 
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"), # background=white
        axis.text.x = element_text(angle=45, hjust = 1,vjust=1,size = 9,face = "bold"),
        plot.title = element_text(size=16,face="bold"),
        axis.text.y = element_text(size = 8)) +
  labs(fill="Birdstrikes") 

ggsave("2019-07-23/southwest.png", width = 7, height = 7)

