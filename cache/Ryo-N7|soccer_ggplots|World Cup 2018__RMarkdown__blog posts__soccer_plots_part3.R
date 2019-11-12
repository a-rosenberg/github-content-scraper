## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----example field, eval=FALSE, fig.width=9, fig.height=6----------------
## ggplot(point_data) +
##   annotate_pitch() +
##   theme_pitch(aspect_ratio = NULL) +
##   coord_flip() +
##   geom_point(
##     aes(x = x, y = y),
##     size = 1.5) +
##   geom_text(
##     aes(x = x, y = y,
##         label = label),
##     vjust = 1.5, color = "red")


## ----packages, message=FALSE, warning=FALSE------------------------------
library(ggplot2)    # general plotting base
library(dplyr)      # data manipulation/tidying
library(ggsoccer)   # draw soccer field plot
library(ggimage)    # add soccer ball emoji + flags
library(extrafont)  # incorporate Dusha font into plots
library(gganimate)  # animate goal plots
library(tweenr)     # create in-between frames for data
library(purrr)      # for creating a list of dataframes for tweenr
library(countrycode)# for finding ISO codes for geom_flag()
# loadfonts()         run once every new session


## ----gazinsky data-------------------------------------------------------
pass_data <- data.frame(
  x = c(100, 94, 82, 82.5,  84, 76.5, 75.5, 94, 99.2),     
  y = c(0,   35, 31, 22,     8, 13, 19, 60, 47.5),
  time = c(1, 2, 3, 4, 5, 6, 7, 8, 9))

golovin_movement <- data.frame(
  x = c(78, 80, 80, 80, 75.5, 74.5, 73.5, 73, 73),  
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
  x = c(0, 0, 0, 0, NA, 92,   92,   92,   92),
  y = c(0, 0, 0, 0, NA, 66.8, 66.8, 66.8, 66.8),
  label = "Gazinsky",
  time = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
)

# ONLY in static + gganimate versions
segment_data <- data.frame(
  x = c(77.5, 98),
  y = c(22, 2),
  xend = c(75, 84),
  yend = c(15, 3),
  linetype = c("dashed", "dashed"),
  color = c("black", "black"),
  size = c(1.2, 1.25)
)

# saudi defender
saudi_data <- data.frame(
  x = c(95, 95, 90, 87, 84, 80, 79, 79, 79),
  y = c(35, 35, 35, 32, 28, 25, 24, 25, 26),
  label = "M. Al-Breik",
  time = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
)

### soccer ball
ball_data <- tribble(
  ~x,  ~y, ~time,
  100,   0,   1,
  94,   35,   2,
  82,   31,   3,
  82.5, 25,   4,
  84,    6,   5, 
  77,   13,   6,
  76,   19,   7,
  94,   60,   8,
  99.2, 47.5, 9
  
) 



## ----gazinsky old gganimate, eval=FALSE----------------------------------
## ggplot(pass_data) +
##   annotate_pitch() +
##   theme_pitch() +
##   coord_flip(
##     xlim = c(49, 101),
##     ylim = c(-1, 101)) +
##   geom_segment(
##     data = segment_data,
##     aes(x = x, y = y,
##         xend = xend, yend = yend),
##     size = segment_data$size,
##     color = segment_data$color,
##     linetype = c("dashed", "dashed")) +
##   geom_label(
##     data = saudi_data,
##     aes(x = x, y = y,
##         label = label),
##     color = "darkgreen") +
##   geom_label(data = zhirkov_movement,
##     aes(x = x, y = y,
##         frame = time,
##         label = label),
##     color = "red") +
##   geom_label(data = golovin_movement,
##     aes(x = x, y = y,
##         frame = time,
##         label = label),
##     color = "red") +
##   geom_label(
##     data = gazinsky_movement,
##     aes(x = x, y = y,
##         label = label),
##     color = "red") +
##   ggimage::geom_emoji(
##     data = ball_data,
##     aes(x = x, y = y, frame = time),
##     image = "26bd", size = 0.035) +
##   ggtitle(
##     label = "Russia (5) vs. (0) Saudi Arabia",
##     subtitle = "First goal, Yuri Gazinsky (12th Minute)") +
##   labs(caption = "By Ryo Nakagawara (@R_by_Ryo)") +
##   annotate(
##     "text", x = 69, y = 65, family = "Dusha V5",
##     label = "After a poor corner kick clearance\n from Saudi Arabia, Golovin picks up the loose ball, \n exchanges a give-and-go pass with Zhirkov\n before finding Gazinsky with a beautiful cross!") +
##   theme(text = element_text(family = "Dusha V5"))
## 


## ----old tweenr, eval=FALSE----------------------------------------------
## ### soccer ball
## b_list <- ball_data %>% pmap(data.frame)
## 
## ball_tween <- b_list %>%
##   tween_states(tweenlength = 0.5, statelength = 0.00000001, ease = "linear", nframes = 75)
## 
## ### Golovin
## golovin_movement_list <- golovin_movement %>% pmap(data.frame)
## 
## golovin_tween <- golovin_movement_list %>%
##   tween_states(tweenlength = 0.5, statelength = 0.00000001, ease = "linear", nframes = 75)
## 
## golovin_tween <- golovin_tween %>% mutate(label = "Golovin")
## 
## ### Zhirkov
## zhirkov_movement_list <- zhirkov_movement %>% pmap(data.frame)
## 
## zhirkov_tween <- zhirkov_movement_list %>%
##   tween_states(tweenlength = 0.5, statelength = 0.00000001, ease = "linear", nframes = 75)
## 
## zhirkov_tween <- zhirkov_tween %>% mutate(label = "Zhirkov")


## ----gazinsky old tweenr, eval=FALSE-------------------------------------
## ggplot(pass_data) +
##   annotate_pitch() +
##   theme_pitch() +
##   coord_flip(xlim = c(49, 101),
##              ylim = c(-1, 101)) +
##   geom_label(
##     data = saudi_data,
##     aes(x = x, y = y,
##         label = label),
##     color = "darkgreen") +
##   geom_label(data = zhirkov_tween,
##     aes(x = x, y = y,
##         frame = .frame,
##         label = label),
##     color = "red") +
##   geom_label(data = golovin_tween,
##     aes(x = x, y = y,
##         frame = .frame,
##         label = label),
##     color = "red") +
##   geom_label(
##     data = gazinsky_movement,
##     aes(x = x, y = y,
##         label = label),
##     color = "red") +
##   ggimage::geom_emoji(
##     data = ball_tween,
##     aes(x = x, y = y, frame = .frame),
##     image = "26bd", size = 0.035) +
##   ggtitle(label = "Russia (5) vs. (0) Saudi Arabia",
##           subtitle = "First goal, Yuri Gazinsky (12th Minute)") +
##   labs(caption = "By Ryo Nakagawara (@R_by_Ryo)") +
##   annotate("text", x = 69, y = 65, family = "Dusha V5",
##            label = "After a poor corner kick clearance\n from Saudi Arabia, Golovin picks up the loose ball, \n exchanges a give-and-go pass with Zhirkov\n before finding Gazinsky with a beautiful cross!") +
##   theme(text = element_text(family = "Dusha V5"))


## ----gazinsky NEW gganimate, warning=FALSE, fig.width=8, fig.height=6, fig.align='center'----
ggplot(pass_data) +
  annotate_pitch() +
  theme_pitch() +
  theme(text = element_text(family = "Dusha V5")) +
  coord_flip(xlim = c(49, 101),
             ylim = c(-1, 101)) +
  geom_segment(
    data = segment_data, 
    aes(x = x, y = y, 
        xend = xend, yend = yend),
    size = segment_data$size,
    color = segment_data$color,
    linetype = c("dashed", "dashed")) +
  geom_label(
    data = zhirkov_movement,
    aes(x = x, y = y,
        label = label),
    color = "red") +
  geom_label(
    data = golovin_movement,
    aes(x = x, y = y,
        label = label),
    color = "red") + 
  geom_label(
    data = gazinsky_movement,
    aes(x = x, y = y,
        label = label),
    color = "red") +
  geom_label(
    data = saudi_data,
    aes(x = x, y = y,
        label = label),
    color = "darkgreen") +
  ggimage::geom_emoji(
    data = ball_data,
    aes(x = x, y = y),   
    image = "26bd", size = 0.035) +
  ggtitle(label = "Russia (5) vs. (0) Saudi Arabia", 
          subtitle = "First goal, Yuri Gazinsky (12th Minute)") +
  labs(caption = "By Ryo Nakagawara (@R_by_Ryo)") +
  annotate("text", x = 69, y = 65, family = "Dusha V5",
           label = "After a poor corner kick clearance\n from Saudi Arabia, Golovin picks up the loose ball, \n exchanges a give-and-go pass with Zhirkov\n before finding Gazinsky with a beautiful cross!") +
  # new gganimate code:
  transition_manual(time)



## ----gazinsky NEW tweenr, warning=FALSE, fig.width=8, fig.height=6, fig.align='center'----
ggplot(pass_data) +
  annotate_pitch() +
  theme_pitch() +
  coord_flip(xlim = c(49, 101),
             ylim = c(-1, 101)) +
  geom_label(
    data = saudi_data,
    aes(x = x, y = y,
        label = label),
    color = "darkgreen") +
  geom_label(
    data = zhirkov_movement,
    aes(x = x, y = y,
        label = label),
    color = "red") +
  geom_label(
    data = golovin_movement,
    aes(x = x, y = y,
        label = label),
    color = "red") +
  geom_label(
    data = gazinsky_movement,
    aes(x = x, y = y,
        label = label),
    color = "red") +
  enter_grow(fade = TRUE) +
  ggimage::geom_emoji(
    data = ball_data,
    aes(x = x, y = y),   
    image = "26bd", size = 0.035) +
  ggtitle(
    label = "Russia (5) vs. (0) Saudi Arabia", 
    subtitle = "First goal, Yuri Gazinsky (12th Minute)") +
  labs(caption = "By Ryo Nakagawara (@R_by_Ryo)") +
  annotate(
    "text", x = 69, y = 65, family = "Dusha V5",
    label = "After a poor corner kick clearance\n from Saudi Arabia, Golovin picks up the loose ball, \n exchanges a give-and-go pass with Zhirkov\n before finding Gazinsky with a beautiful cross!") +
  theme(text = element_text(family = "Dusha V5")) +
  # new gganimate code:
  transition_states(
    time, 
    transition_length = 0.5, 
    state_length = 0.0001,
    wrap = FALSE) +
  ease_aes("linear")



## ----osako data, warning=FALSE-------------------------------------------
cornerkick_data <- data.frame(
  x = 99, y = 0.3,
  x2 = 94, y2 = 47)

osako_gol <- data.frame(
  x = 94, y = 49,
  x2 = 100, y2 = 55.5)

ball_data <- data.frame(
  x = c(99, 94, 100),
  y = c(0.3, 47, 55.5),
  time = c(1, 2, 3))

player_label <- data.frame(
  x = c(92, 99), 
  y = c(49, 2),
  label = c("Osako", "Honda"))

wc_logo <- data.frame(
  x = 107,
  y = 85) %>% 
  mutate(
    image = "https://upload.wikimedia.org/wikipedia/en/thumb/6/67/2018_FIFA_World_Cup.svg/1200px-2018_FIFA_World_Cup.svg.png")

flag_data <- data.frame(
  x = c(110, 110),
  y = c( 13, 53),
  team = c("japan", "colombia")
  ) %>% 
  mutate(
    image = team %>% 
           countrycode(., origin = "country.name", destination = "iso2c")
  ) %>% 
  select(-team)


## ----osako NEW tweenr, warning=FALSE, fig.height = 5, fig.width = 7, fig.align='center'----
ggplot(ball_data) +
  annotate_pitch() +
  theme_pitch() +
  theme(
    text = element_text(family = "Dusha V5"),
    plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  coord_flip(
    xlim = c(55, 112),
    ylim = c(-1, 101)) +
  geom_label(
    data = player_label, 
    aes(x = x, y = y,
        label = label),
    family = "Dusha V5") +
  geom_point(
    aes(x = 98, y = 50), size = 3, color = "green") +
  annotate(
    geom = "text", family = "Dusha V5", 
    hjust = c(0.5, 0.5, 0.5, 0.5),
    size = c(6.5, 4.5, 5, 3),
    label = c("Japan             (2) vs. Colombia             (1)",
              "Kagawa (PEN 6'), Quintero (39'), Osako (73')",
              "Japan press their man advantage, substitute Honda\ndelivers a delicious corner kick for Osako to (somehow) tower over\nColombia's defense and flick a header into the far corner!",
              "by Ryo Nakagawara (@R_by_Ryo)"),
    x = c(110, 105, 70, 53), 
    y = c(30, 30, 47, 85)) +
  ggimage::geom_emoji(              # soccer ball emoji
    aes(x = x, 
        y = y),
    image = "26bd", size = 0.035) +
  ggimage::geom_flag(               # Japan + Colombia flag
    data = flag_data,
    aes(x = x, y = y,
        image = image),       
    size = c(0.08, 0.08)) +
  geom_image(                       # World Cup logo
    data = wc_logo,     
    aes(x = x, y = y,
        image = image), size = 0.17) +
  # new gganimate code:
  transition_states(
    time, 
    transition_length = 0.5, 
    state_length = 0.0001,
    wrap = FALSE) +
  ease_aes("quadratic-out")


## ----offside data--------------------------------------------------------
# PLAYERS
# JAPAN: x, y (blue)     Senegal: x2, y2  (lightgreen)
trap_data <- data.frame(
  
  time = c(1, 2, 3, 4, 5),
  
  # ball trajectory
  x = c(70, 70, 70, 87, 95),       
  y = c(85, 85, 85, 52, 33),
  
  # offside line bar
  #xo =    c(83, 81.2, 79, 77.5, 70),
  xoend = c(83.8, 81.8, 79, 78.5, 71),
  
  yo =    c( 5,  5,  5,  5, 5),
  yoend = c(95, 95, 95, 95, 95),
  
  # players: japan
  jx  = c(83, 81, 77, 75, 70),
  jy  = c(rep(65, 5)),
  
  jx2 = c(83, 81.8, 78.5, 77, 70),
  jy2 = c(rep(60.5, 5)),
  
  jx3 = c(83, 81, 76.5, 75, 71),
  jy3 = c(rep(55, 5)),
  
  jx4 = c(83, 81.2, 76.3, 75, 70),
  jy4 = c(rep(52, 5)),
  
  jx5 = c(82.8, 81, 77, 74, 70),
  jy5 = c(rep(49, 5)),
  
  jx6 = c(83, 81.8, 77, 74, 70),
  jy6 = c(rep(45, 5)),

  jx7 = c(83.8, 81, 79, 77.5, 70),
  jy7 = c(rep(40, 5)),
  
  # players: senegal
  sx = c(83, 84, 84, 84, 84),
  sy = c(rep(33, 5)),
  
  sx2 = c(83, 85, 87, 92, 95),
  sy2 = c(38, 37, 35, 34, 33),
  
  sx3 = c(83, 84, 84, 83, 83),
  sy3 = c(rep(41, 5)),
  
  sx4 = c(83, 84, 83, 78, 78),
  sy4 = c(rep(45, 5)),
  
  sx5 = c(83, 84, 87, 88, 89),
  sy5 = c(rep(52, 5)),
  
  sx6 = c(83, 85, 84, 84, 83),
  sy6 = c(rep(69, 5))
)

# flags
flag_data <- data.frame(
  x = c( 48, 93),
  y = c(107, 107),
  team = c("japan", "senegal")
  ) %>% 
  mutate(
    image = team %>% 
           countrycode(., origin = "country.name", destination = "iso2c")
  ) %>% 
  select(-team)

# extra players:
goalkeeper_data <- data.frame(
  x = c(98),
  y = c(50)
)

senegal_data <- data.frame(
  x = c(55, 55, 68.5),
  y = c(50, 60, 87)
)


## ----tweenr offside NEW, warning=FALSE, fig.height=10, fig.width=9, fig.align='center'----
ggplot(trap_data) +
  annotate_pitch() +
  theme_pitch(aspect_ratio = NULL) +
  coord_fixed(
    xlim = c(30, 101),
    ylim = c(-5, 131)) +
  # offside line
  geom_segment(aes(x = xoend, y = yo, 
                   xend = xoend, yend = yoend), 
               color = "black", size = 1.3) +
  # japan
  geom_point(aes(x = jx, y = jy), size = 4, color = "blue") +
  geom_point(aes(x = jx2, y = jy2), size = 4, color = "blue") +
  geom_point(aes(x = jx3, y = jy3), size = 4, color = "blue") +
  geom_point(aes(x = jx4, y = jy4), size = 4, color = "blue") +
  geom_point(aes(x = jx5, y = jy5), size = 4, color = "blue") +
  geom_point(aes(x = jx6, y = jy6), size = 4, color = "blue") +
  geom_point(aes(x = jx7, y = jy7), size = 4, color = "blue") +
  # senegal
  geom_point(aes(x = sx, y = sy), size = 4, color = "green") +
  geom_point(aes(x = sx2, y = sy2), size = 4, color = "green") +
  geom_point(aes(x = sx3, y = sy3), size = 4, color = "green") +
  geom_point(aes(x = sx4, y = sy4), size = 4, color = "green") +
  geom_point(aes(x = sx5, y = sy5), size = 4, color = "green") +
  geom_point(aes(x = sx6, y = sy6), size = 4, color = "green") +
  
  # free kick spot (reference)
  geom_point(aes(x = 70, y = 85), color = "blue", size = 1.2) +
  # goalkeeper
  geom_point(
    data = goalkeeper_data,
    aes(x = x, y = y), size = 4, color = "blue") +
  # senegal defenders
  geom_point(
    data = senegal_data,
    aes(x = x, y = y), size = 4, color = "green") +
  annotate(
    geom = "text", family = "Dusha V5", 
    hjust = c(0, 0),
    size = c(6, 6.5),
    label = c("Japan          (2) vs. Senegal         (2)",
              "The Perfect Offside Trap"),
    x = c(30,  30), 
    y = c(107, 115)) +
  ggimage::geom_flag(
    data = flag_data,
    aes(x = x, y = y,
        image = image),       
    size = c(0.07, 0.07)) +
  ggimage::geom_emoji(
    aes(x = x, y = y),
    image = "26bd", size = 0.035) +
  # NEW gganimate code
  transition_states(
    states = time, 
    transition_length = 0.2, 
    state_length = 0.00000001,
    wrap = FALSE) +
  ease_aes("linear")



## ----meme, eval=FALSE----------------------------------------------------
## library(memery)
## img <- ("https://imgflip.com/s/meme/Roll-Safe-Think-About-It.jpg")
## 
## meme_labs <- c("you can't lose the aerial battle", "if you set an offside trap")
## 
## meme(img, meme_labs, "offside_meme.png")
## 


## ----frame_vars example--------------------------------------------------
frames_data <- frame_vars(animation = last_animation())

glimpse(frames_data)


