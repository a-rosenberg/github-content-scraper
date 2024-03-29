## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, 
                      message = FALSE, 
                      warning = FALSE, 
                      dpi = 144,
                      fig.align = "center")
remove(list = ls(all.names = TRUE))
detachAllPackages <- function() {
  basic.packages.blank <-  c("stats","graphics","grDevices","utils","datasets","methods","base")
  basic.packages <- paste("package:", basic.packages.blank, sep = "")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search())) == 1,TRUE,FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list) > 0)  for (package in package.list) {
    detach(package, character.only = TRUE)}}
detachAllPackages()
if (!require(pacman)) {
  install.packages("pacman")
  require(pacman)
}
p_load(tidyverse, knitr, data.table, lubridate, zoo, hrbrthemes, tidytuesdayR, prophet, forecast, gridExtra)

`%g%` <- function(x,y) {
  z <- paste0(y, collapse = "|")
  grepl(z, x, ignore.case = T)
}

nowt <- function(x = NULL) x


## ------------------------------------------------------------------------
tt_load(2019, week = 29) %>% 
  map(~list2env(.x[1], envir = .GlobalEnv))


## ------------------------------------------------------------------------
fit <- r4ds_members %>% 
  select(ds = date, 
         y  = weekly_active_members) %>% 
  prophet(yearly.seasonality = T, 
          weekly.seasonality = T)

future <- make_future_dataframe(fit, periods = 365.25/2, freq = 'day')

m <- predict(fit, future)

plot(fit, m) +
  theme_ipsum_rc() +
  labs(
    title = "R for Data Science Online Learning Community",
    subtitle = "Modeling and Forecasting Weekly Active Slack Members",
    caption = "Source: https://github.com/rfordatascience/tidytuesday",
    y = "Active Members",
    x = ""
  ) -> p

ggsave(p, filename = "tt_2019_29.png",device = "png", dpi = 144, width = 8, height = 6)

