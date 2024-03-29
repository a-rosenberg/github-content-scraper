## ----message = F, warning = F--------------------------------------------
library(tidyverse)
library(magrittr)
library(ggthemes)


## ----message = F, warning = F--------------------------------------------
raw_df <- read_csv("../data/2018-08-28/nfl_2010-2017.csv") %>% select(-1)


## ------------------------------------------------------------------------
ari_df <- raw_df %>% 
  filter(team == "ARI")

avg_df <- raw_df %>% 
  group_by(team, game_year, game_week) %>% 
  summarise(rush_total = sum(rush_yds, na.rm = T),
            pass_total = sum(pass_yds, na.rm = T)) %>% 
  group_by(game_year, game_week) %>% 
  summarise(rush_mean = mean(rush_total, na.rm = T),
            pass_mean = mean(pass_total, na.rm = T)) 


## ------------------------------------------------------------------------
library(scico)

plot_tile <- function(df, var, ttext) {
  quo_var = enquo(var)
  ggplot(df, aes(game_year, game_week)) +
  geom_tile(aes(fill = !!quo_var), colour = "grey50") +
  scale_fill_scico() +
  scale_x_continuous(limits = c(1999.5, 2017.5), expand = c(0, 0),
                     breaks = seq(2000, 2016, by = 2)) +
  scale_y_continuous(limits = c(0.5, 16.5), expand = c(0, 0),
                     breaks = 1:16) + 
  labs(title = ttext, x = "Year", y = "Week") +
  guides(fill = guide_legend(title = "Rush Yards")) +
  theme_bw() + 
  theme(panel.grid = element_blank(), panel.border = element_blank()) +
  NULL
}

p1 <- avg_df %>% 
  plot_tile(rush_mean, "Mean Rushing Yards Per Week")
p2 <- avg_df %>% 
  plot_tile(pass_mean, "Mean Passing Yards Per Week")


## ----fig.width=10, fig.height=12-----------------------------------------
library(patchwork)
p1 + p2 + plot_layout(ncol = 1)


## ------------------------------------------------------------------------
avg_year_df <- raw_df %>% 
  group_by(team, game_year, game_week) %>% 
  summarise(rush_total = sum(rush_yds, na.rm = T),
            pass_total = sum(pass_yds, na.rm = T)) %>% 
  group_by(game_year) %>% 
  summarise(rush_mean = mean(rush_total, na.rm = T),
            pass_mean = mean(pass_total, na.rm = T)) 


## ----fig.width=10--------------------------------------------------------
avg_year_df %>% 
  ggplot() +
  geom_line(aes(x = game_year, y = rush_mean, color = "blue"), size = 2) +
  geom_line(aes(x = game_year, y = pass_mean, color = "red"), size = 2) +
  scale_x_continuous(limits = c(2000, 2017), expand = c(0, 0),
                     breaks = seq(2000, 2016, by = 2)) +
  scale_y_continuous(limits = c(25, 275), expand = c(0, 0),
                     breaks = seq(25, 275, by = 25)) +
#  theme(panel.grid = element_blank(), panel.border = element_blank()) +
  labs(title = "Trends in NFL Rushing and Passing Yards per Team Game", 
       x = "Year", y = "Team Yards per Game",
       caption = "Source: Pro-Football-Reference.com") +
  NULL

