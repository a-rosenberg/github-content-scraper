## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "README-"
)


## ---- include=FALSE------------------------------------------------------
library(grid)
library(tidyverse)
library(gumballthemes)


## ----example, eval=F-----------------------------------------------------
## ## basic example code
## p <- mtcars %>%
##   as_tibble() %>%
##   ggplot(aes(y = wt, x = mpg)) +
##   geom_point() +
##   geom_quantileframe() +
##   scale_y_quantile(mtcars$wt) +
##   scale_x_quantile(mtcars$mpg, digits = 1) +
##   ylab("Weight (tons)") +
##   xlab("Fuel efficiency (mpg)") +
##   scale_color_gumball() +
##   theme_gumball() +
##   guides(col = guide_legend(ncol= 6))
## 
## p %>% ggsave(filename = "basic_img.JPG", height = 3.5, width = 5.95)


## ------------------------------------------------------------------------
library(extrafont)
p <- mtcars %>%
  as_tibble() %>%
  ggplot(aes(y = wt, x = mpg)) +
  geom_point() +
  geom_quantileframe() +
  scale_y_quantile(mtcars$wt) +
  scale_x_quantile(mtcars$mpg, digits = 1) +
  ylab("Weight (tons)") +
  xlab("Fuel efficiency (mpg)") +
  scale_color_gumball() +
  hrbrthemes::theme_ipsum(grid = F) +
  guides(col = guide_legend(ncol= 6))

p %>% ggsave(filename = "basic_hrbrh_img.JPG", height = 3.5, width = 5.95)


## ------------------------------------------------------------------------
p <- mtcars %>%
  as_tibble() %>%
  ggplot(aes(y = wt, x = mpg, color = factor(carb))) +
  geom_point() +
  geom_quantileframe() +
  scale_y_quantile(mtcars$wt) +
  scale_x_quantile(mtcars$mpg, digits = 1) +
  ylab("Weight (tons)") +
  xlab("Fuel efficiency (mpg)") +
  scale_color_gumball() +
  theme_gumball()+
  guides(col = guide_legend(ncol= 6)) 

p %>% ggsave(filename = "basic_color_img.JPG", height = 3.5, width = 5.95)


## ---- echo=F, include=FALSE----------------------------------------------
library(fredr)
fredr_set_key("c1856e7bf91eaca807aa6584e2614397")


## ------------------------------------------------------------------------
#library(fredr)
#fredr_set_key("abcdefghijklmnopqrstuvwxyz123456")
unrate <- fredr_series_observations(
  series_id = "UNRATE",
  observation_start = as.Date("1990-01-01"),
  observation_end = as.Date("2018-04-01"),
  frequency = "q",
  units = "chg"
)

unrate %>% glimpse()


## ------------------------------------------------------------------------
p <- unrate %>% 
  ggplot(aes(y = value, x = date)) +
  geom_line() +
  geom_quantileframe(sides = "l") +
  scale_y_quantile(unrate$value) + 
  scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
  ylab("Change In UNRATE") +
  xlab("") +
  annotate("text", y = 1.0, x = as.Date("1997-01-01"),
           label = "Change in UNRATE\nis normally between\n0.07% and -.20% with a\nmedian of -0.07") +
  guides(col = guide_legend(ncol= 6)) +
  scale_color_gumball() +
  theme_gumball()

p %>% ggsave(filename = "time_series.JPG", height = 3.5, width = 5.95)

