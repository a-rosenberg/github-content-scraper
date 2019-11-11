library(ggplot2)
library(dplyr)
library(gganimate)
library(tidyverse)
library(lubridate)
library(extrafont)
font_import()
loadfonts()


#Load dataset
game_data <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-30/video_games.csv")

#remove data with playtime=0
data_plot<-game_data %>% filter(!is.na(metascore) & !is.na(price) 
                                & !is.na(owners) & !is.na(release_date))

#change date
data_plot <- data_plot %>% mutate(year=year(mdy(release_date)))

#modify owners categories
data_plot %>% group_by(owners) %>% tally()

cat1 <- data_plot %>% filter(grepl("20,000$", owners, fixed = FALSE) | 
                       grepl("^20,000", owners, fixed = FALSE) )
cat2 <- data_plot %>% filter(grepl("100,000$", owners, fixed = FALSE))
cat3 <- data_plot %>% filter(grepl("200,000$", owners, fixed = FALSE) | 
                               grepl("^200,000", owners, fixed = FALSE) )
cat4 <- data_plot %>% filter(grepl("1,000,000$", owners, fixed = FALSE))
cat5 <- data_plot %>% filter(grepl("2,000,000$", owners, fixed = FALSE) | 
                               grepl("^2,000,000", owners, fixed = FALSE) )
cat6 <- data_plot %>% filter(grepl("10,000,000$", owners, fixed = FALSE))
cat7 <- data_plot %>% filter(grepl("^10,000,000", owners, fixed = FALSE))
cat1$category <- '1'
cat2$category <- '2'
cat3$category <- '3'
cat4$category <- '4'
cat5$category <- '5'
cat6$category <- '6'
cat7$category <- '7'

data_plot2 <-bind_rows(list(cat1, cat2, cat3, cat4, cat5, cat6, cat7))

# static plot 
owners_labs <- c("0 to 50k","50k to 100k","100k to 500k",
                 "550k to 1,000k","1,000k to 5,000k",'5,000k to 10,000k',
                 "above 10,000k")
p<-data_plot2 %>% ggplot(aes(x = metascore, y = price)) + 
  labs(y="price in USD", x="metascore",
       title="Evolution of video games' prices and metascores")+
  geom_point(aes(color=category,size = category, group = seq_along(category))) +
  scale_color_discrete(name = "number of owners", labels = owners_labs)+
  scale_size_discrete(guide = FALSE) +
  theme_minimal(base_size = 16)+
  theme(text=element_text(family="Times New Roman"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))

p
# animated plot 
my_anim <- p+transition_states(year,
                    transition_length = 1,
                    state_length = 1)+
  enter_fade() + 
  exit_shrink()+
  labs(subtitle= "Year of release : {closest_state}")

#my_anim
animate(my_anim, height = 350, width =650)

anim_save("price_metascore.gif", animation = last_animation(), 
          path = "/Users/aponsero/Documents/other_works/Rvisualisations_tidytuesday/week30_19")
  