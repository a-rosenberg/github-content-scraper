library(ggplot2)
library(dplyr)
library(gganimate)
library(tidyverse)
library(lubridate)
library(extrafont)
font_import()
loadfonts()


#Load dataset
r4ds_members <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-16/r4ds_members.csv")
data_plot <- r4ds_members %>% 
  select(date, total_membership,daily_active_members,weekly_active_members) %>% 
  gather(members, count, -date) 



# static plot time-serie
data_plot %>% ggplot(aes(x = date, y = count)) + 
  labs(y="number of members", x="date",
       title="Number of members in R4DS\n(2017 to 2019)")+
  geom_line(aes(color = members), size = 1) +
  scale_color_manual(values = c("brown1","darkolivegreen4","burlywood3"),
                     labels=c("daily active members", "total members",
                              "weekly active members")) +
  geom_point(aes(color = members), size = 3) +
  theme_minimal(base_size = 16)+
  theme(text=element_text(family="Times New Roman"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))

# animation ! exciting times !
my_anim <- data_plot %>% ggplot(aes(x = date, y = count)) + 
  labs(y="number of members", x="date",
       title="Number of members in R4DS\n(2017 to 2019)")+
  geom_line(aes(color = members), size = 1) +
  scale_color_manual(values = c("brown1","darkolivegreen4","burlywood3"),
                     labels=c("daily active members", "total members",
                              "weekly active members")) +
  geom_point(aes(color = members), size = 3) +
  theme_minimal(base_size = 16)+
  theme(text=element_text(family="Times New Roman"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))+
  # Here comes the gganimate stuff
  transition_reveal(date,
                    range = as.Date(c("2017-08-27", "2020-01-01")))

animate(my_anim, height = 350, width =650)

anim_save("time_series1.gif", animation = last_animation(), 
          path = "/Users/aponsero/Documents/Rvisualisations_tidytuesday/week29_19")