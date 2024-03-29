## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----Gazinsky ani data---------------------------------------------------
library(ggplot2)
library(dplyr)
library(ggsoccer)
library(ggimage)
library(extrafont)
library(gganimate)
library(tweenr)
# loadfonts()

# data
pass_data <- data.frame(
  x = c(100, 94, 82, 82.5,  84, 76.5, 75.5, 94, 99.2),       # pass balls
  y = c(0,   35, 31, 22,     8, 13, 19, 60, 47.5),
  time = c(1, 2, 3, 4, 5, 6, 7, 8, 9))

golovin_movement <- data.frame(
  x = c(78, 80, 80, 80, 75.5, 74.5, 73.5, 73, 73),   #75, 74, 73
  y = c(30, 30, 27, 25,   10,    9, 15, 15, 15),
  label = "Golovin",
  time = c(1, 2, 3,  4,  5,  6,  7,  8,  9)
)

zhirkov_movement <- data.frame(
  x = c(98, 90, 84, 84, 84, 84, 84, 84, 84),
  y = c( 0,  2,  2,  2,  2,  2,  2,  2,  2),
  label = "Zhirkov",
  time = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
)

gazinsky_movement <- data.frame(
  x = c(90.8),
  y = c(69),
  label = "Gazinsky",
  time = c(6, 7, 8, 9)
)

# segment golovin should only appear 4-5
# segment zhirkov should only appear 1-3
segment_data <- data.frame(
  x = c(77.5, 98),
  y = c(22, 2),
  xend = c(75, 84),
  yend = c(15, 3),
  linetype = c("dashed", "dashed"),
  color = c("darkgreen", "darkgreen"),
  size = c(1.2, 1.25)
)

saudi_data <- data.frame(
  x = c(95),
  y = c(35)
)

data <- tribble(
  ~x,  ~y,  ~label, ~time,
  100,  0,  "ball",     1,
   98,  0, "Golovin",   1,
   98,  0, "Zhirkov",   1,
   94, 35, "ball",      2,
   80, 30, "Golovin",   2,
   90,  2, "Zhirkov",   2,
   82, 31, "ball",      3,
   80, 25, "Golovin",   3,
   82,  3, "Zhirkov",   3, 
   82.5, 25, "ball",    4,
   80, 20, "Golovin",   4, 
   82,  3, "Zhirkov",   4,
   84,  6, "ball",      5, 
   75, 10, "Golovin",   5,
   82,  3, "Zhirkov",   5, 
   77, 13, "ball",      6,
   74,  9, "Golovin",   6,
   82,  3, "Zhirkov",   6,
   76, 19, "ball",      7,
   73, 15, "Golovin",   7,
   82,  3, "Zhirkov",   7,
   94, 60, "ball",      8,
   73, 20, "Golovin",   8,
   82,  3, "Zhirkov",   8,
   99.2, 47.5, "ball",  9,
   73, 20, "Golovin",   9,
   82,  3, "Zhirkov",   9
  
)

# arrange by time!
# instead of string label, give numerical ID value? but the IDs themselves will get "interpolated".... wtf

data2 <- data %>% select(-label)


data3 <- tribble(
  ~x,  ~y,  ~label, ~time,
  100,  0,  "ball",     1,
   94, 35, "ball",      2,
   82, 31, "ball",      3,
   82.5, 25, "ball",    4,
   84,  6, "ball",      5, 
   77, 13, "ball",      6,
   76, 19, "ball",      7,
   94, 60, "ball",      8,
   99.2, 47.5, "ball",  9,

) %>% select(-label)





## ----Gazinsky ani plot---------------------------------------------------
ani <- ggplot(pass_data) +
  annotate_pitch() +
  theme_pitch() +
  coord_flip(xlim = c(49, 101),
             ylim = c(-1, 101)) +
  geom_segment(data = segment_data, 
               aes(x = x, y = y, 
                   xend = xend, yend = yend),
               size = segment_data$size,
               color = segment_data$color,
               linetype = c("dashed", "dashed")) +
  geom_label(
    data = golovin_movement,
    aes(x = x, y = y,
        frame = time,
        label = label)) +
  geom_label(
    data = zhirkov_movement,
    aes(x = x, y = y,
        frame = time,
        label = label)) +
  geom_label(
    data = gazinsky_movement,
    aes(x = x, y = y,
        frame = time,
        label = label)) +
  geom_point(
    data = saudi_data,
    aes(x = x, y = y),
    color = "darkgreen",
    size = 5) +
  ggimage::geom_emoji(
    aes(x = x, y = y, 
        frame = time), 
    image = "26bd", 
    size = 0.035) +
  theme(text = element_text(family = "Dusha V5")) +
  ggtitle(label = "Russia (5) vs. (0) Saudi Arabia", 
          subtitle = "First goal, Yuri Gazinsky (12th Minute)") +
  labs(caption = "By Ryo Nakagawara (@R_by_Ryo)") +
  annotate("text", x = 69, y = 65, family = "Dusha V5",
           label = "After a poor corner kick clearance\n from Saudi Arabia, Golovin picks up the loose ball, \n exchanges a give-and-go pass with Zhirkov\n before finding Gazinsky with a beautiful cross!")

gganimate(ani, "ani.gif")  # use frame to check positionings...



## ----Gazinsky tween data-------------------------------------------------
library(purrr)

# all tween
data_list <- map(seq(nrow(data)),
                      ~ data[c(seq(.x), rep(.x, nrow(data) - .x)), ])
  
data_tween <- data_list %>% 
  tween_states(tweenlength = 2, statelength = 1.25, ease = "linear", nframes = 200)    
# Error in col2rgb(d) : invalid color name 'ball'
# can't recognize non-numeric data >>> SO question + issue opened but unanswered
# probably solved in the new API!


# try withoutlabels col
data_list <- map(seq(nrow(data2)),
                      ~ data2[c(seq(.x), rep(.x, nrow(data2) - .x)), ])
  
data_tween <- data_list %>% 
  tween_states(tweenlength = 2, statelength = 1.25, ease = "linear", nframes = 200)

data_t <- data_tween %>% group_by(.frame) %>% slice(27)

# data3
data_list <- map(seq(nrow(data3)),
                      ~ data3[c(seq(.x), rep(.x, nrow(data3) - .x)), ])
  
data_tween <- data_list %>% 
  tween_states(tweenlength = 0.5, statelength = 0.00000001, ease = "linear", nframes = 75)

data_t <- data_tween %>% group_by(.frame) %>% slice(9)


# plot

gazinsky_goal <- data_t %>% # change depending on whatever df i wantto try...
  ggplot() +
  annotate_pitch() +
  theme_pitch() +
  coord_flip(xlim = c(49, 101),
             ylim = c(-1, 101)) +
  geom_segment(data = segment_data, 
               aes(x = x, y = y, 
                   xend = xend, yend = yend),
               size = segment_data$size,
               color = segment_data$color,
               linetype = c("dashed", "dashed")) +
  geom_point(
    data = saudi_data,
    aes(x = x, y = y),
    color = "darkgreen",
    size = 5) +
  ggimage::geom_emoji(
    aes(x = x, y = y, frame = .frame),    # change frame to .frame or it'll still move by time only -_-
    image = "26bd", size = 0.035) +
  geom_label(data = golo_t,
    aes(x = x, y = y,
        frame = .frame,
        label = label)) +
  theme(text = element_text(family = "Dusha V5")) +
  ggtitle(label = "Russia (5) vs. (0) Saudi Arabia", 
          subtitle = "First goal, Yuri Gazinsky (12th Minute)") +
  labs(caption = "By Ryo Nakagawara (@R_by_Ryo)") +
  annotate("text", x = 69, y = 65, family = "Dusha V5",
           label = "After a poor corner kick clearance\n from Saudi Arabia, Golovin picks up the loose ball, \n exchanges a give-and-go pass with Zhirkov\n before finding Gazinsky with a beautiful cross!")
  
  

# left in the label data so the ball moves to those points as wel... not good.

gganimate(gazinsky_goal, 
          ani.width = 800, ani.height = 500, 
          interval = 0.25, 
          "gazinsky_goal.gif")

# with data3, data_tween as input: leaves a trail of ball emoji behind at every point... (without the slice() part...)

gganimate(gazinsky_goal, 
          ani.width = 800, ani.height = 500, 
          interval = 0.25, 
          "gg_gazinsky.gif")

# with golovin label movement AND ball movement?

gganimate(gazinsky_goal, 
          ani.width = 800, ani.height = 500, 
          interval = 0.25, 
          "gg_ball_label.gif")



## ----Gazinsky movement tweenr--------------------------------------------
golovin_movement_list <- map(seq(nrow(golovin_movement)),
                      ~golovin_movement[c(seq(.x), rep(.x, nrow(golovin_movement) - .x)), ])
  
golovin_tween <- golovin_movement_list %>% 
  tween_states(tweenlength = 0.5, statelength = 0.00000001, ease = "linear", nframes = 75)

golo_t <- golovin_tween %>% group_by(.frame) %>% slice(9)

golovin_move <- ggplot(pass_data) +
  annotate_pitch() +
  theme_pitch() +
  coord_flip(xlim = c(49, 101),
             ylim = c(-1, 101)) +
  geom_segment(data = segment_data, 
               aes(x = x, y = y, 
                   xend = xend, yend = yend),
               size = segment_data$size,
               color = segment_data$color,
               linetype = c("dashed", "dashed")) +
  geom_label(data = golo_t,
    aes(x = x, y = y,
        frame = .frame,
        label = label)) +
  ggimage::geom_emoji(
    aes(x = x, y = y, frame = time), 
    image = "26bd", size = 0.035)

gganimate(golovin_move, 
          ani.width = 800, ani.height = 500, 
          title_frame = FALSE, interval = 0.25, 
          "golovin_move.gif")


## ----zhirkov movement----------------------------------------------------

zhirkov_movement <- data.frame(
  x = c(98, 90, 84, 84, 84, 84, 84, 84, 84),
  y = c( 0,  2,  2,  2,  2,  2,  2,  2,  2),
  label = "Zhirkov",
  time = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
)

zhirkov_movement_list <- map(seq(nrow(zhirkov_movement)),
                      ~zhirkov_movement[c(seq(.x), rep(.x, nrow(zhirkov_movement) - .x)), ])
  
zhirkov_tween <- zhirkov_movement_list %>% 
  tween_states(tweenlength = 0.5, statelength = 0.00000001, ease = "linear", nframes = 75)

zhir_t <- zhirkov_tween %>% group_by(.frame) %>% slice(9)

zhirkov_move <- ggplot(pass_data) +
  annotate_pitch() +
  theme_pitch() +
  coord_flip(xlim = c(49, 101),
             ylim = c(-1, 101)) +
  geom_segment(data = segment_data, 
               aes(x = x, y = y, 
                   xend = xend, yend = yend),
               size = segment_data$size,
               color = segment_data$color,
               linetype = c("dashed", "dashed")) +
  geom_label(data = zhir_t,
    aes(x = x, y = y,
        frame = .frame,
        label = label)) +
  geom_label(data = golo_t,
    aes(x = x, y = y,
        frame = .frame,
        label = label)) +
  geom_label(
    data = gazinsky_movement,
    aes(x = x, y = y,
        label = label)) +
  ggimage::geom_emoji(
    data = data_t,
    aes(x = x, y = y, frame = .frame),    # change frame to .frame or it'll still move by time only -_-
    image = "26bd", size = 0.035)

gganimate(zhirkov_move, 
          ani.width = 800, ani.height = 500, 
          title_frame = FALSE, interval = 0.25, 
          "zhirkov_move.gif")



## ------------------------------------------------------------------------
#<center>
#![](https://upload.wikimedia.org/wikipedia/en/thumb/6/67/2018_FIFA_World_Cup.svg/1200px-2018_FIFA_World_Cup.svg.png)
#</center>


## ----other offside-------------------------------------------------------

ggplot(offside_data) +
  annotate_pitch() +
  theme_pitch() +
  lims(x = c(-1, 101),
       y = c(-1, 101))

ggplot(offside_data) +
  annotate_pitch() +
  theme_pitch() +
  coord_cartesian(xlim = c(60, 101),
                  ylim = c(-1, 101))



create_StatsBomb_ShotMap <- function(grass_colour, line_colour, background_colour, goal_colour){
  
  theme_blankPitch = function(size=12) { 
    theme(
      #axis.line=element_blank(), 
      axis.text.x=element_blank(), 
      axis.text.y=element_blank(), 
      #axis.ticks.y=element_text(size=size),
      #   axis.ticks=element_blank(),
      axis.ticks.length=unit(0, "lines"), 
      #axis.ticks.margin=unit(0, "lines"), 
      axis.title.x=element_blank(), 
      axis.title.y=element_blank(), 
      legend.background=element_rect(fill=background_colour, colour=NA), 
      legend.key=element_rect(colour=background_colour,fill=background_colour), 
      legend.key.size=unit(1.2, "lines"), 
      legend.text=element_text(size=size), 
      legend.title=element_text(size=size, face="bold",hjust=0),
      strip.background = element_rect(colour = background_colour, fill = background_colour, size = .5),
      panel.background=element_rect(fill=background_colour,colour=background_colour), 
      #       panel.border=element_blank(), 
      panel.grid.major=element_blank(), 
      panel.grid.minor=element_blank(), 
      panel.spacing=element_blank(), 
      plot.background=element_blank(), 
      plot.margin=unit(c(0, 0, 0, 0), "lines"), 
      plot.title=element_text(size=size*1.2), 
      strip.text.y=element_text(colour=background_colour,size=size,angle=270),
      strip.text.x=element_text(size=size*1))}
  
    ymin <- 60 # minimum width
    ymax <- 120 # maximum width
    xmin <- 0 # minimum length
    xmax <- 80 # maximum length
    
    # Defining features along the length
    boxEdgeOff <- 102
    sixYardOff <- 114
    penSpotOff <- 108
    halfwayline <- 60
    
    # Defining features along the width
    boxEdgeLeft <- 18
    boxEdgeRight <- 62
    sixYardLeft <- 30 
    sixYardRight <- 50
    goalPostLeft <- 36
    goalPostRight <- 44
    CentreSpot <- 40   
    
    # other dimensions
    centreCirle_d <- 20   
  
  ## define the circle function
  circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }

  #### create leftD arc ####
  dArc <- circleFun(c((40),(penSpotOff)),centreCirle_d,npoints = 1000)
  ## remove part that is in the box
  dArc <- dArc[which(dArc$y <= (boxEdgeOff)),]
    
    ## initiate the plot, set some boundries to the plot
  p <- ggplot() + xlim(c(ymin,ymax)) + ylim(c(xmin,xmax)) +
  # add the theme 
  theme_blankPitch() +
  # add the base rectangle of the pitch 
  geom_rect(aes(xmin=ymin, xmax=ymax, ymin=xmin, ymax=xmax), fill = grass_colour, colour = line_colour) +
  # add the 18 yard box offensive
  geom_rect(aes(xmin=boxEdgeLeft, xmax=boxEdgeRight, ymin=boxEdgeOff, ymax=xmax), fill = grass_colour, colour = line_colour) +
  # add the six yard box offensive
  geom_rect(aes(xmin=sixYardLeft, xmax=sixYardRight, ymin=sixYardOff, ymax=xmax), fill = grass_colour, colour = line_colour) +
  # add the arc circle 
  geom_path(data=dArc, aes(x=x,y=y), colour = line_colour) +
  # add penalty spot 
  geom_point(aes(x = CentreSpot , y = penSpotOff), colour = line_colour) +
     # add the goal offensive
  geom_segment(aes(x = goalPostLeft, y = xmax, xend = goalPostRight, yend = xmax),colour = goal_colour, size = 1)
  
  return(p)

}

p <- create_StatsBomb_ShotMap("#538032", "#ffffff", "#538032", "#000000")

p +
  coord_flip()
  geom_point(aes(x = c(70, 30), 
                 y = c(80, 100)))




