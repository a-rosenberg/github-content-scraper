## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----echo = FALSE, warning=FALSE, message=FALSE, fig.width=8, fig.height=5----
library(dplyr)
library(ggplot2)
library(ggimage)
library(countrycode)
library(tidyr)
library(forcats)
library(extrafont)
#loadfonts()  run once per new session!

group_h <- data.frame(

  time =      c( 1, 2, 3, 4),
  japan =   c(   1, 3, 2, 2),
  senegal =   c( 2, 1, 3, 3),
  colombia =   c(3, 2, 1, 1),
  poland = c(    4, 4, 4, 4)
  
)

group_h <- group_h %>% 
  gather(team, position, -time) %>% 
  mutate(team = as.factor(team)) %>% 
  mutate(team = as.factor(team),
         team = fct_relevel(team, 
                            "japan", "senegal", "colombia", "poland"),
         flag = team %>% 
           countrycode(., origin = "country.name", destination = "iso2c"))

x_labs <- c("0'", "59'", "74'", "Full Time")
y_labs <- c("1st", "2nd", "3rd", "4th")

score_labs <- data.frame(
  x = c(2, 3, 4, 4, 4, 4),
  y = c(3, 1, 1, 2, 3, 4),
  score = c("0-1", "1-0", 
            "1-0", "0-1", "0-1", "1-0")
)

country_labs <- data.frame(
  x = c(1, 1, 1, 1),
  y = c(1, 2, 3, 4),
  country = c("Japan", "Senegal", "Colombia", "Poland")
)

goals_labs <- data.frame(
  x = c(2, 3),
  y = c(3, 1),
  scorers = c("Bednarek (Poland)", "Mina")
)

points_labs <- data.frame(
  x = c(4.8, 4.8, 4.8, 4.8),
  y = c(1, 2, 3, 4),
  points = c("6 pts.", "4 pts.", "4 pts.", "3 pts.")
)

## PLOT

ggplot(
  group_h,
  aes(time, position)) +
  geom_line(
    aes(group = team), linetype = "dotted") +
  geom_flag(
    aes(image = flag), 
    size = 0.11) +
  geom_text(
    data = country_labs,
    aes(x = x, y = y, 
        label = country,
        family = "Dusha V5"),
    nudge_y = 0.3, size = 6) +
  geom_text(
    data = score_labs,
    aes(x = x, y = y, 
        label = score,
        family = "Dusha V5"),
    nudge_y = 0.3, size = 6) +
  geom_text(
    data = goals_labs,
    aes(x = x, y = y, 
        label = scorers,
        family = "Dusha V5"),
    nudge_y = -0.25, size = 4) +
  geom_text(
    data = points_labs,
    aes(x = x, y = y, 
        label = points,
        family = "Dusha V5"),
    size = 5) +
  scale_y_reverse(
    expand = c(0, 0), 
    limits = c(4.8, 0.6),
    breaks = 1:4,
    labels = y_labs) +
  scale_x_continuous(
    position = "top", 
    breaks = 1:4,
    labels = x_labs,
    expand = c(0, 0),
    limits = c(0.6, 5)) +
  labs(
    title = "Group H Table Throughout The Last Matchday",
    subtitle = "Japan vs. Poland & Senegal vs. Colombia",
    caption = "by @R_By_Ryo") +
  theme_minimal() +
  theme(
    text = element_text(family = "Dusha V5", size = 18),
    axis.title = element_blank(),
    legend.position = "none",
    panel.grid = element_blank())



## ----warning=FALSE, message=FALSE----------------------------------------
library(dplyr)        # the usual data cleaning
library(tidyr)        # the usual data tidying
library(forcats)      # dealing with factor data
library(ggplot2)      # plotting
library(ggimage)      # adding images and flags into the plots
library(countrycode)  # easy way to access ISO codes
library(extrafont)    # inserting custom fonts into the plots
# loadfonts()  run once per new session!



## ----initial df----------------------------------------------------------
group_d <- data.frame(

  time =      c(1, 2, 3, 4, 5, 6, 7),
  croatia =   c(1, 1, 1, 1, 1, 1, 1),
  nigeria =   c(2, 3, 2, 2, 2, 3, 3),
  iceland =   c(3, 4, 3, 4, 3, 4, 4),
  argentina = c(4, 2, 4, 3, 4, 2, 2)
  
)



## ----spread() + ISO codes------------------------------------------------

group_d <- group_d %>% 
  gather(team, position, -time) %>% 
  mutate(team = as.factor(team),
         team = fct_relevel(team, 
                            "croatia", "nigeria", "argentina", "iceland"),
         flag = team %>% 
           countrycode(., origin = "country.name", destination = "iso2c"))

glimpse(group_d)



## ----country labs--------------------------------------------------------
country_labs <- data.frame(
  x = c(rep(1, 4)),
  y = c(rep(1:4, 1)),
  country = c("Croatia", "Nigeria", "Iceland", "Argentina")
)



## ----axis labels---------------------------------------------------------

x_labs <- c("0'", "14'", "51'", "53'", "76'", "86'", "Full Time")
y_labs <- c("1st", "2nd", "3rd", "4th")



## ----score + goal labs---------------------------------------------------
score_labs <- data.frame(
  x = c(2, 3, 4, 5, 6, 
        7, 7, 7, 7),    # always have score labels for every team at FULL TIME
  y = c(2, 2, 4, 3, 2, 
        1, 2, 3, 4),
  score = c("1-0", "1-1", "0-1", "1-1", "2-1", 
            "2-1", "2-1", "1-2", "1-2")          # Full Time scores
)

goals_labs <- data.frame(
  x = c(2, 3, 4, 5, 6, 7),
  y = c(2, 2, 4, 3, 2, 1),
  scorers = c(
    "Messi", "Moses (pen.)", "(Croatia)\nBadelj", 
    "G. Sigurdsson (pen.)", "Rojo", "Perisic (90')")
)



## ----points labs---------------------------------------------------------
points_labs <- data.frame(
  x = c(rep(max(group_d$time), 4)),
  y = c(rep(1:4, 1)),
  points = c("9 pts.", "4 pts.", "3 pts.", "1 pts.")
)



## ----theme()-------------------------------------------------------------

theme_matchday <- theme_minimal() +
  theme(
    text = element_text(family = "Dusha V5", size = 18),
    axis.title = element_blank(),
    axis.text = element_text(color = "grey30"),
    legend.position = "none",
    panel.grid = element_blank())



## ----fig.width=8, fig.height=5-------------------------------------------

# NOTE: Argentina in 4th at start due to more yellow cards in tie-breaker vs. Iceland.

ggplot(group_d, aes(time, position)) +
  geom_line(
    aes(group = team), 
    linetype = "dotted") +
  geom_flag(
    aes(image = flag), 
    size = 0.11) +
  geom_text(
    data = country_labs,
    aes(x = x, y = y, 
        label = country,
        family = "Dusha V5"),
    nudge_y = 0.3, size = 5.5) +
  geom_text(
    data = score_labs,
    aes(x = x, y = y, 
        label = score,
        family = "Dusha V5"),
    nudge_y = 0.3, size = 5.5) +
  geom_text(
    data = goals_labs,
    aes(x = x, y = y, 
        label = scorers,
        family = "Dusha V5"),
    nudge_y = -0.38, size = 3.5) +
  geom_text(
    data = points_labs,
    aes(x = x, y = y, 
        label = points,
        family = "Dusha V5"),
    nudge_x = 0.8,
    size = 5,
    color = "grey30") + # match color with the other axes labels!
  scale_y_reverse(
    expand = c(0, 0), 
    limits = c(4.8, 0.6),
    breaks = 1:4,
    labels = y_labs) +
  scale_x_continuous(
    position = "top", 
    breaks = 1:7,
    labels = x_labs,
    expand = c(0, 0),
    limits = c(0.5, 8.1)) +
  labs(
    title = "Group D Table Throughout The Last Matchday",
    subtitle = "Nigeria vs. Argentina & Iceland vs. Croatia",
    caption = "by @R_By_Ryo") +
  theme_matchday


