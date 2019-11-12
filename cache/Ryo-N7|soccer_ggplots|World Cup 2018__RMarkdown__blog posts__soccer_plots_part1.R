## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----packages, eval=FALSE, warning=FALSE, message=FALSE------------------
## library(ggplot2)   # plotting on top of ggsoccer
## library(ggsoccer)  # create soccer pitch overlay
## library(dplyr)     # data manipulation
## library(purrr)     # create multiple dataframes for tweenr
## library(tweenr)    # build frames for animation
## library(gganimate) # animate plots
## library(extrafont) # insert custom fonts into plots
## library(ggimage)   # insert images and emoji into plots


## ----half-pitch template-------------------------------------------------
library(ggplot2)
library(ggsoccer)

data <- data.frame(x = 1, y = 1)

ggplot(data) +
  annotate_pitch() +
  theme_pitch() +
  coord_flip(xlim = c(49, 101),
             ylim = c(-1, 101))


## ----gazinsky data, echo=FALSE, warning=FALSE, message=FALSE-------------
library(ggplot2)
library(dplyr)
library(ggsoccer)
library(ggimage)
library(extrafont)
# loadfonts()

#                              2   1  
pass_data <- data.frame(x = c( 84, 82),
                        y = c(  6, 32),
                        x2 = c(77, 84),
                        y2 = c(13, 8))

#            corner kick + golovin cross
curve_data <- data.frame(x = c(100, 76),
                         y = c(0, 19),
                         x2 = c(94, 94),
                         y2 = c(35, 60))

# Saudi failed clearance, Gazinsky header
ball_data <- data.frame(x = c(94, 94),
                        y = c(35, 60),
                        x2 = c(82, 99.2),
                        y2 = c(33.5, 47.5))

# soccer ball image
goal_img <- data.frame(x = 100,
                       y = 47) %>% 
  mutate(image = "https://d30y9cdsu7xlg0.cloudfront.net/png/43563-200.png")

# golovin + zhirkov movement
movement_data <- data.frame(x = c(83, 98),
                           y = c(24.25, 2),
                           x2 = c(77, 88),
                           y2 = c(21, 6))

label_data <- data.frame(
  x = c(94, 83, 75, 98, 84),
  y = c(60, 23, 11,  0,  6),
  label = c("Gazinsky", "Golovin", "Golovin", "Zhirkov", "Zhirkov"),
  hjust = c(-0.1, -0.05, -0.1,  0.5,  0.5),
  vjust = c( 0.5,  0.5,   0.5, -0.3, -0.3)
)



## ----gazinsky plot-------------------------------------------------------

ggplot(pass_data) +
  annotate_pitch() +
  theme_pitch() + 
  theme(text = element_text(family = "Trebuchet MS")) +
  coord_flip(xlim = c(49, 101),
             ylim = c(-1, 101)) +
  geom_segment(aes(x = x, y = y, xend = x2, yend = y2),
               arrow = arrow(length = unit(0.25, "cm"),
                             type = "closed")) +
  geom_segment(data = ball_data,
               aes(x = x, y = y, xend = x2, yend = y2), 
               linetype = "dashed", size = 0.85,
               color = c("black", "red")) +
  geom_segment(data = movement_data,
               aes(x = x, y = y, xend = x2, yend = y2), 
               linetype = "dashed", size = 1.2,
               color = "darkgreen") +
  geom_curve(data = curve_data, 
             aes(x = x, y = y, xend = x2, yend = y2), 
             curvature = 0.25, 
             arrow = arrow(length = unit(0.25, "cm"),
                           type = "closed")) +
  geom_image(data = goal_img,
             aes(x = x, y = y,
                 image = image), 
             size = 0.035) +
  ggtitle(label = "Russia (5) vs. (0) Saudi Arabia", 
          subtitle = "First goal, Yuri Gazinsky (12th Minute)") +
  labs(caption = "By Ryo Nakagawara (@R_by_Ryo)") +
  geom_label(data = label_data,
    aes(x = x, y = y, 
        label = label,
        hjust = hjust,
        vjust = vjust)) +
  annotate("text", x = 69, y = 65, family = "Trebuchet MS",
           label = "After a poor corner kick clearance\n from Saudi Arabia, Golovin picks up the loose ball, \n exchanges a give-and-go pass with Zhirkov\n before finding Gazinsky with a beautiful cross!")



## ----ball img, eval=FALSE------------------------------------------------
## goal_img <- data.frame(x = 100,
##                        y = 47) %>%
##   mutate(image = "https://d30y9cdsu7xlg0.cloudfront.net/png/43563-200.png")
## 
## ## ggplot2 code ##
## geom_image(data = goal_img,
##              aes(x = x, y = y,
##                  image = image),
##              size = 0.035)
## ## ggplot2 code ##
## 


## ----Cristiano data, echo=FALSE, warning=FALSE, message=FALSE------------
library(ggplot2)
library(ggsoccer)
library(extrafont)
library(emoGG)
library(ggimage)
# loadfonts()
# Official WC 2018 Font: "Dusha"
# http://fifa2018wiki.com/fifa-2018-font-typeface-download-dusha-font-ttf/509/

# look up soccer ball emoji:
# emoji_search("soccer")  # "26bd"

goals_data <- data.frame(x = c(88, 80, 71),
                         y = c(50, 48, 54),
                         label = c(1, 2, 3))

curve_data <- data.frame(x = c(88, 71), y = c(50, 54),
                         xend = c(100, 100), yend = c(54, 54))

annotation_data <- data.frame(
  hjust = c(0.5, 0.5, 0.5, 0, 0, 0),
  label = c("Portugal             (3) vs. Spain            (3)",
            "Cristiano's Hattrick (4', 44', 88')",
            "by Ryo Nakagawara (@R_by_Ryo)",
            "1. Fouled by Nacho in the box,\nCristiano confidently strokes the ball\ninto the right corner from the spot.",
            "2. Guedes lays it off to Cristiano whose\nstrong shot is uncharacteristically\nfumbled by De Gea into the net.",
            "In the final minutes of the game,\nCristiano wins a freekick against Pique\nand curls it beautifully over the wall."),
  x = c(110, 105, 53, 76, 66, 66), 
  y = c(30, 20, 85, 5, 5, 55)
)

flag_data <- data.frame(
  image = c("PT", "ES"),
  x = c(110, 110),
  y = c(19.1, 50.3)
)



## ----Cristiano plot, fig.height=5, fig.width=8---------------------------

ggplot(goals_data) +
  annotate_pitch() +
  theme_pitch() +
  theme(text = element_text(family = "Dusha V5"),
        legend.position = "none") +
  coord_flip(xlim = c(55, 112),
             ylim = c(-1, 101)) +
  geom_segment(x = 80, y = 48, 
               xend = 97, yend = 48) +  # 2nd 
  geom_segment(x = 97, y = 48, 
               xend = 100, yend = 45.5,
               arrow = arrow(length = unit(0.25, "cm"),
                             type = "closed")) +        # degea fumble
  geom_curve(data = curve_data,
             aes(x = x, y = y, 
                 xend = xend, yend = yend),     # FREEKICK
             curvature = 0.3, 
             arrow = arrow(length = unit(0.25, "cm"), type = "closed")) +
  geom_text(data = annotation_data,
            family = "Dusha V5", 
            aes(x = x, y = y,
                hjust = hjust, label = label), 
            size = c(6.5, 4.5, 3, 3.5, 3.5, 3.5)) +
  geom_flag(data = flag_data,
            aes(x = x, y = y,
                image = image), size = c(0.08, 0.08)) +       # Portugal + Spain Flag
  ggimage::geom_emoji(aes(x = 105, 
                 y = c(45, 50, 55)),
             image = "26bd", size = 0.035) +
  geom_point(aes(x = x, y = y), 
             shape = 21, size = 7, color = "black", fill = "white") +
  geom_text(aes(x = x, y = y, label = label, family = "Dusha V5"))



## ---- eval=FALSE---------------------------------------------------------
## annotation_data <- data.frame(
##   hjust = c(0.5, 0.5, 0.5, 0, 0, 0),
##   label = c("Portugal             (3) vs. Spain            (3)",
##             "Cristiano's Hattrick (4', 44', 88')",
##             "by Ryo Nakagawara (@R_by_Ryo)",
##             "1. Fouled by Nacho in the box,\nCristiano confidently strokes the ball\ninto the right corner from the spot.",
##             "2. Guedes lays it off to Cristiano whose\nstrong shot is uncharacteristically\nfumbled by De Gea into the net.",
##             "In the final minutes of the game,\nCristiano wins a freekick against Pique\nand curls it beautifully over the wall."),
##   x = c(110, 105, 53, 76, 66, 66),
##   y = c(30, 20, 85, 5, 5, 55)
## )


## ----geom_flag, eval=FALSE-----------------------------------------------
## 
## flag_data <- data.frame(
##   image = c("PT", "ES"),
##   x = c(110, 110),
##   y = c(19.1, 50.1)
## )
## 
## ## ggplot2 code ##
## geom_flag(data = flag_data,
##           aes(x = x, y = y,
##               image = image, size = size))
## ## ggplot2 code ##
## 


## ----emoji, warning=FALSE, message=FALSE---------------------------------
library(emoGG)
library(ggimage)

emoji_search("soccer") # "26bd"



## ----geom_emoji, eval=FALSE----------------------------------------------
## ## ggplot2 code ##
## ggimage::geom_emoji(aes(x = 105,
##                         y = c(45, 50, 55)),
##                     image = "26bd", size = 0.035)
## ## ggplot2 code ##


## ----extrafont, eval=FALSE-----------------------------------------------
## font_import()  # import font files in your computer
## 
## font_install() # install any new font files added to your computer
## 
## loadfonts()    # run every new session once!
## 
## fonts()        # to check out what fonts are ready for use in R!


## ----Osako winner data, warning=FALSE, message=FALSE---------------------
library(ggplot2)
library(dplyr)
library(ggsoccer)
library(extrafont)
library(ggimage)
library(countrycode)

cornerkick_data <- data.frame(x = 99, y = 0.3,
                              x2 = 94, y2 = 47)

osako_gol <- data.frame(x = 94, y = 49,
                        x2 = 100, y2 = 55.5)

player_label <- data.frame(x = c(92, 99), 
                           y = c(49, 2))

annotation_data <- data.frame(
  x = c(110, 105, 70, 92, 53), 
  y = c(30, 30, 45, 81, 85),
  hjust = c(0.5, 0.5, 0.5, 0.5, 0.5),
  label = c("Japan             (2) vs. Colombia             (1)",
            "Kagawa (PEN 6'), Quintero (39'), Osako (73')",
            "Japan press their man advantage, substitute Honda\ndelivers a delicious corner kick for Osako to (somehow) tower over\nColombia's defense and flick a header into the far corner!",
            "Bonus: Ospina looking confused and\ndoing a lil' two-step-or-god-knows-what.",
            "by Ryo Nakagawara (@R_by_Ryo)")
)

flag_data <- data.frame(
  x = c(110, 110),
  y = c(13, 53),
  team = c("japan", "colombia")
  ) %>% 
  mutate(
    image = team %>% 
           countrycode(., origin = "country.name", destination = "iso2c")
  ) %>% 
  select(-team)

wc_logo <- data.frame(x = 107,
                       y = 85) %>% 
  mutate(image = "https://upload.wikimedia.org/wikipedia/en/thumb/6/67/2018_FIFA_World_Cup.svg/1200px-2018_FIFA_World_Cup.svg.png")



## ----Osako winner plot, fig.width=7, fig.height=5, warning=FALSE, message=FALSE----

ggplot(osako_gol) +
  annotate_pitch() +
  theme_pitch() +
  theme(text = element_text(family = "Dusha V5"),
        plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  coord_flip(xlim = c(55, 112),
             ylim = c(-1, 101)) +
  geom_curve(data = cornerkick_data,
             aes(x = x, y = y, xend = x2, yend = y2),
             curvature = -0.15, 
             arrow = arrow(length = unit(0.25, "cm"),
                             type = "closed")) +
  geom_segment(aes(x = x, y = y, xend = x2, yend = y2),
               arrow = arrow(length = unit(0.25, "cm"),
                             type = "closed")) +
  geom_label(data = player_label, 
             aes(x = x, y = y),
             label = c("Osako", "Honda"), family = "Dusha V5") +
  geom_point(aes(x = 98, y = 50), size = 3, color = "green") +
  geom_text(aes(x = 99.7, y = 50), size = 5, label = "???", family = "Dusha V5") +
  geom_text(data = annotation_data,
            family = "Dusha V5", 
            aes(x = x, y = y,
                hjust = hjust, label = label), 
            size = c(6.5, 4.5, 4, 3.5, 3)) +
  ggimage::geom_flag(data = flag_data,
                     aes(x = x, y = y,
                         image = image),       
                     size = c(0.08, 0.08)) +
  ggimage::geom_emoji(aes(x = 95, 
                          y = 50),
             image = "26bd", size = 0.035) +
  geom_image(data = wc_logo,
             aes(x = x, y = y,
                 image = image), size = 0.17)



## ----ISO flag, warning=FALSE, message=FALSE------------------------------
library(countrycode)

flag_data <- data.frame(
  x = c(110, 110),
  y = c(13, 53),
  team = c("japan", "colombia")
  ) %>% 
  mutate(
    image = team %>% 
           countrycode(., origin = "country.name", destination = "iso2c")
  ) %>% 
  select(-team)

glimpse(flag_data)



## ----gazinsky animate, echo=FALSE, warning=FALSE, message=FALSE----------
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
  label = "ball movement",
  time = c(1, 2, 3, 4, 5, 6, 7, 8, 9))

golovin_movement <- data.frame(
  x = c(78, 80, 80, 80, 75, 74, 73, 73, 73),
  y = c(30, 30, 27, 25,  10, 9, 15, 15, 15),
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
  x = c(91.5),
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

# animate

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

gganimate(ani, "gazi_goal.gif", title_frame = FALSE)



## ----osako tween, echo=FALSE, warning=FALSE, message=FALSE---------------
library(purrr)
library(tweenr)


ball_data <- data.frame(x = c(99, 94, 100),
                        y = c(0.3, 47, 55.5))

ball_list <- map(seq(nrow(ball_data)),
                      ~ ball_data[c(seq(.x), rep(.x, nrow(ball_data) - .x)), ])
  
osako_tween <- ball_list %>% 
  tween_states(tweenlength = 1.5, statelength = 0.01, ease = "quadratic-out", nframes = 50)

os_t <- osako_tween %>% group_by(.frame) %>% slice(3)
# take only changed 3rd value (1 and 2 of each frame are just copies of ball point origin coords)

g2 <- ggplot(os_t) +
  annotate_pitch() +
  theme_pitch() +
  theme(text = element_text(family = "Dusha V5")) +
  coord_flip(xlim = c(55, 112),
             ylim = c(-1, 101)) +
  geom_label(data = player_label, 
             aes(x = x, y = y),
             label = c("Osako", "Honda"), family = "Dusha V5") +
  geom_point(aes(x = 98, y = 50), size = 3, color = "green") +
  annotate(geom = "text", family = "Dusha V5", 
           hjust = c(0.5, 0.5, 0.5, 0.5),
           size = c(6.5, 4.5, 4, 3),
           label = c("Japan             (2) vs. Colombia             (1)",
                     "Kagawa (PEN 6'), Quintero (39'), Osako (73')",
                     "Japan press their man advantage, substitute Honda\ndelivers a delicious corner kick for Osako to (somehow) tower over\nColombia's defense and flick a header into the far corner!",
                     "by Ryo Nakagawara (@R_by_Ryo)"),
           x = c(110, 105, 70, 53), 
           y = c(30, 30, 45, 85)) +
  ggimage::geom_emoji(aes(x = x, 
                          y = y,
                          frame = .frame),
             image = "26bd", size = 0.035) +
  ggimage::geom_flag(aes(image = "JP"),       # Japan Flag
            x = 110, y = 13, size = 0.08) +
  ggimage::geom_flag(aes(image = "CO"),       # Colombia Flag
            x = 110, y = 53, size = 0.08) +
  geom_image(data = wc_logo,
             aes(x = x, y = y,
                 image = image), size = 0.17) +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

gganimate(g2, 
          ani.width = 800, ani.height = 500, 
          interval = 0.01,
          "osako_goal.gif") 


