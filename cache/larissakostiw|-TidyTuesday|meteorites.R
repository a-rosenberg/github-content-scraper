## ------------------------------------------------------------------------
meteorites <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-11/meteorites.csv")


## ------------------------------------------------------------------------
summary(meteorites)
#summary(meteorites$year)




## ------------------------------------------------------------------------
library(tidyverse)
meteorites$name_type<- as.factor(meteorites$name_type)
#name_type<- distinct(meteorites$name_type)
meteorites$year<- as.numeric(meteorites$year)
#meteorites$year<- as.factor(meteorites$year)


## ------------------------------------------------------------------------
# meteorites$fall<- as.factor(meteorites$fall)
# meteorites$year<- as.factor(meteorites$year)
# 
#Convert NA to 0: Year
meteorites<- meteorites %>%
  filter(!is.na(year))

meteorites<- na.omit(meteorites)

#drop_na in tidyverse


## ------------------------------------------------------------------------
#install.packages("mapdata")
library(mapdata)

#install.packages("ggmap")
library(ggmap)
world<-map_data("world")


## ------------------------------------------------------------------------
meteorites<- meteorites %>%
  filter(year>=1900 & year<2018)


## ------------------------------------------------------------------------
#install.packages("mapproj")
#library(mapproj)
#install.packages("cowplot")
#library(cowplot)
map <- ggplot() +
  geom_polygon(
    data = world,
    aes(x = long, y = lat, group = group),
    fill = "gray50",
    size = 0.1
  ) +
  labs(
    title = "Meteorites on Earth",
    subtitle = "Year: {round(frame_time)}",
    caption = "Source: NASA"
  ) + 
  geom_point(
    data = meteorites,
    aes(
      x = long,
      y = lat,
      size = mass,
      color = mass
    ),     alpha = 0.8
  ) +
  scale_color_distiller(
    palette = "Reds",
    direction = 1
    # labels = c("0", "2.5", "5.0", "7.5", "10.0+"),
    # guide = guide_colorbar(
    #   direction = "horizontal",
    #   barheight = unit(3, units = "mm"),
    #   barwidth = unit(60, units = "mm"),
    #   title.position = "top",
    #   title.hjust = 0.5,
    #   label.hjust = 0.5    
    )   +
    theme_map()  +
    theme(plot.title = element_text(hjust=0.5, face='bold', colour="white"),
        plot.subtitle = element_text(hjust=0.5, face='bold', colour="white"),
        axis.ticks.x=element_blank(),
        panel.background = element_rect(fill="black"),
        plot.background = element_rect(fill="black"),
        legend.position = "none") 
        
  
map



## ------------------------------------------------------------------------
#install.packages("gganimate")
library(gganimate)
#install.packages("gifski")
#library(gifski)
map_gif <- map +
  transition_events(start = year)+
  enter_grow() +
  exit_fade()  transition_time(year)

animate(map_gif, fps=1)


## ------------------------------------------------------------------------
anim_save("Meteorites.gif")

