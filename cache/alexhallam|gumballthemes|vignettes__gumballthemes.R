## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ----include=F-----------------------------------------------------------
library(grid) # still working on putting this in NAMESPACE
library(gumballthemes)
library(tidyverse)


## ---- fig.align="center"-------------------------------------------------
#library(grid) # still working on putting this in NAMESPACE
#library(gumballthemes)
#library(tidyverse)
mtcars %>%
  as_tibble() %>%
  ggplot(aes(y = mpg, x = hp, color = factor(carb))) +
  geom_point() +
  geom_quantileframe() +
  scale_y_quantile(mtcars$mpg) +
  scale_x_quantile(mtcars$hp, digits = 0) +
  ylab("Miles Per Gallon") +
  xlab("Horse Power") +
  guides(col = guide_legend(ncol= 6)) +
  scale_color_gumball() +
  theme_gumball()


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


## ---- fig.align="center", fig.width=5.4, fig.height=3--------------------
unrate %>% 
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


## ---- fig.align="center", fig.width=5.4, fig.height=3--------------------
unrate_monthly <- fredr_series_observations(
  series_id = "UNRATE",
  observation_start = as.Date("1990-01-01"),
  observation_end = as.Date("2018-04-01"),
  frequency = "m"
)

unrate_monthly %>% 
  mutate(year = factor(lubridate::year(date))) %>% 
  ggplot(aes(y = value, x = year)) +
  ggthemes::geom_tufteboxplot() +
  geom_quantileframe(sides = "l") + 
  scale_y_quantile(unrate_monthly$value, digits = 2) + 
  theme_gumball() +
  theme(axis.text.x = element_text(hjust = 1,angle = 45))


