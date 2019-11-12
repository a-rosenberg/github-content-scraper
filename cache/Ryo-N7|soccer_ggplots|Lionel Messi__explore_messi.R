## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---- message=FALSE, warning=FALSE---------------------------------------
pacman::p_load(tidyverse, StatsBombR, 
               SBpitch, soccermatics, rlang, 
               extrafont, ggupset, glue, tibbletime,
               patchwork, cowplot)

loadfonts(device = "win", quiet = TRUE)


## ------------------------------------------------------------------------
comps <- FreeCompetitions()

glimpse(comps)


## ------------------------------------------------------------------------
messi_matches_raw <- comps %>% 
  filter(competition_id ==11) %>% 
  FreeMatches()


messi_data_raw <- StatsBombFreeEvents(MatchesDF = messi_matches_raw)

glimpse(messi_data_raw)


## ------------------------------------------------------------------------
saveRDS(messi_data_raw, file = here::here("data/messi_data_raw.RDS"))
messi_data_raw <- readRDS(file = here::here("data/messi_data_raw.RDS"))


## ------------------------------------------------------------------------
messi_data_clean <- messi_data_raw %>% 
  allclean() %>%  ## cleanlocations, goalkeeper, shot, freezeframe, defensive
  left_join(comps %>% select(season_id, season_name), by = "season_id")


## ------------------------------------------------------------------------
saveRDS(messi_data_clean, file = here::here("data/messi_data_clean.RDS"))
messi_data_clean <- readRDS(file = here::here("data/messi_data_clean.RDS"))


## ------------------------------------------------------------------------
messi_split <- messi_data_clean %>% 
  group_split(season_id)

map(.x = messi_split, ~select(.x, season_id) %>% unique()) %>% bind_rows()


messi_2009_2010_df <- messi_split[[1]]
messi_2009_2010_df$season_id %>% unique()

messi_2010_2011_df <- messi_split[[2]]
messi_2010_2011_df$season_id %>% unique()

messi_2011_2012_df <- messi_split[[3]]
messi_2011_2012_df$season_id %>% unique()

messi_2004_2005_df <- messi_split[[4]]
messi_2004_2005_df$season_id %>% unique()

messi_2005_2006_df <- messi_split[[5]]
messi_2005_2006_df$season_id %>% unique()

messi_2006_2007_df <- messi_split[[6]]
messi_2006_2007_df$season_id %>% unique()

messi_2007_2008_df <- messi_split[[7]]
messi_2007_2008_df$season_id %>% unique()

messi_2008_2009_df <- messi_split[[8]]
messi_2008_2009_df$season_id %>% unique()


## ---- eval=FALSE---------------------------------------------------------
## saveRDS(messi_2004_2005_df, file = here::here("data/messi_2004_2005_df.RDS"))
## messi_2004_2005_df <- readRDS(file = here::here("data/messi_2004_2005_df.RDS"))
## 
## saveRDS(messi_2005_2006_df, file = here::here("data/messi_2005_2006_df.RDS"))
## messi_2005_2006_df <- readRDS(file = here::here("data/messi_2005_2006_df.RDS"))
## 
## saveRDS(messi_2006_2007_df, file = here::here("data/messi_2006_2007_df.RDS"))
## messi_2006_2007_df <- readRDS(file = here::here("data/messi_2006_2007_df.RDS"))
## 
## saveRDS(messi_2007_2008_df, file = here::here("data/messi_2007_2008_df.RDS"))
## messi_2007_2008_df <- readRDS(file = here::here("data/messi_2007_2008_df.RDS"))


## ------------------------------------------------------------------------
glimpse(messi_data_clean)


## ------------------------------------------------------------------------
messi_data_clean %>% 
  select(type.name) %>% 
  unique()


## ------------------------------------------------------------------------
messi_data_clean %>% 
  filter(team.name == "Barcelona") %>% 
  select(player.name, player.id) %>% unique() %>% View()


## ------------------------------------------------------------------------
JoinPlayerNickName(messi_2007_2008_df)


## ------------------------------------------------------------------------
messi_2007_2008_df %>% 
  filter(team.name == "Barcelona") %>% 
  select(player.name) %>% 
  unique()


## ------------------------------------------------------------------------
messi_data_clean %>% 
  filter(team.name == "Barcelona", 
         type.name == "Pass",
         player.id == 5503) -> df


## ------------------------------------------------------------------------
pass_mapper_plot <- function(df, lengthPitch = 105, widthPitch = 68, 
                         xBins = 10, yBins = NULL, 
                         arrow = c("none", "r", "l"), 
                         colLow = "white", colHigh = "red", 
                         title = NULL, subtitle = NULL, 
                         x = "location.x", y = "location.y") {
  #browser()
  df <- df %>% 
    mutate(x = location.x, 
           y = location.y)
  
  if (is.null(yBins)) { 
    yBins <- xBins
  }
  
  df <- df[df$x > 0 & df$x < lengthPitch & df$y > 0 & df$y < widthPitch, ]
  
  x.range <- seq(0, lengthPitch, length.out = xBins + 1) 
  y.range <- seq(0, widthPitch, length.out = yBins + 1)
  
  p <- soccerPitch(lengthPitch, widthPitch, arrow = arrow, 
                   title = title, subtitle = subtitle, theme = "blank") + 
    geom_bin2d(data = df, aes(x, y), 
               binwidth = c(diff(x.range)[1], diff(y.range)[1])) +
    scale_fill_gradient(low = colLow, high = colHigh) + 
    guides(fill = FALSE)
  
  p <- soccerPitchFG(p)
  return(p)
}



soccer_pitch(lengthPitch, widthPitch, arrow = arrow, 
                   title = title, subtitle = subtitle, theme = "blank") + 
    geom_bin2d(data = df, aes(x, y))



## ------------------------------------------------------------------------
messi_data_clean %>% 
  filter(team.name == "Barcelona", 
         type.name == "Pass",
         player.id == 5503) %>% 
  soccerHeatmap(x = "location.x", y = "location.y")


messi_data_clean %>% 
  filter(team.name == "Barcelona", 
         type.name == "Pass",
         player.id == 5503) %>% 
  pass_mapper_plot()


## ------------------------------------------------------------------------
messi_2004_2005_df %>% 
  filter(team.name == "Barcelona", 
         type.name == "Pass",
         player.id == 5503) %>% 
  pass_mapper_plot()

messi_2005_2006_df %>% 
  filter(team.name == "Barcelona", 
         type.name == "Pass",
         player.id == 5503) %>% 
  pass_mapper_plot()

messi_2006_2007_df %>% 
  filter(team.name == "Barcelona", 
         type.name == "Pass",
         player.id == 5503) %>% 
  pass_mapper_plot()

messi_2007_2008_df %>% 
  filter(team.name == "Barcelona", 
         type.name == "Pass",
         player.id == 5503) %>% 
  pass_mapper_plot()



## ------------------------------------------------------------------------
messi_2007_2008_df$type.name %>% unique()


## ------------------------------------------------------------------------
lengthPitch = 105 
widthPitch = 68 
                         
xBins = 10 
yBins = NULL 
                         
arrow <- c("none", "r", "l") 
                         
colLow = "white" 
colHigh = "red" 
                         
title = NULL 
subtitle = NULL


## ------------------------------------------------------------------------
pass_receipt_plot <- function(df, lengthPitch = 105, widthPitch = 68, 
                         xBins = 10, yBins = NULL, 
                         arrow = c("none", "r", "l"), 
                         colLow = "white", colHigh = "red", 
                         title = NULL, subtitle = NULL) {
  #browser()
  df <- df %>% 
    mutate(x = location.x, 
           y = location.y)
  
  if (is.null(yBins)) { 
    yBins <- xBins
  }
  
  df <- df[df$x > 0 & df$x < lengthPitch & df$y > 0 & df$y < widthPitch, ]
  
  x.range <- seq(0, lengthPitch, length.out = xBins + 1) 
  y.range <- seq(0, widthPitch, length.out = yBins + 1)
  
  p <- soccerPitch(lengthPitch, widthPitch, arrow = arrow, 
                   title = title, subtitle = subtitle, theme = "blank") + 
    geom_point(data = df, aes(x = x, y = y))
  
  p <- soccerPitchFG(p)
  return(p)
}


## ------------------------------------------------------------------------
messi_2007_2008_df %>% 
  filter(player.id == 5503, 
         type.name == 'Ball Receipt*') %>% 
  pass_receipt_plot()


## ------------------------------------------------------------------------
df_receipt <- messi_2007_2008_df %>% 
  filter( (player.id == 5503 & type.name == 'Ball Receipt*') |
            lead(player.id == 5503 & type.name == 'Ball Receipt*') & 
            type.name == "Pass") %>% View()


## ------------------------------------------------------------------------
df_receipt %>% 
  filter(play_pattern.id == 1) %>% 
  View()


## ------------------------------------------------------------------------
messi_pass_received <- messi_data_clean %>% 
  mutate(pass.outcome.name = fct_explicit_na(pass.outcome.name, "Complete")) %>%
  filter(type.name == "Pass",
         pass.outcome.name == "Complete",
         pass.recipient.id == 5503,
         !play_pattern.name %in% c("From Corner", "From Free Kick",
                                   "From Throw In")) %>% 
  select(player.name, pass.recipient.name, 
         season_id, season_name,
         position.name, position.id,
         location.x, location.y,
         pass.end_location.x, pass.end_location.y,
         contains("pass")) %>% 
  mutate(pass_duo = map2(player.name, pass.recipient.name, ~c(.x, .y))) %>% 
  add_count(player.name, pass.recipient.name)
  


## ------------------------------------------------------------------------
messi_2007_2008_df %>% 
  mutate(pass.outcome.name = fct_explicit_na(pass.outcome.name, "Complete")) %>%
  filter(type.name == "Pass",
         pass.outcome.name == "Complete",
         pass.recipient.id == 5503,
         !play_pattern.name %in% c("From Corner", "From Free Kick",
                                   "From Throw In")) %>% 
  select(player.name, pass.recipient.name, 
         position.name, position.id,
         location.x, location.y,
         pass.end_location.x, pass.end_location.y,
         contains("pass")) -> messi_0708_pass_received


## ------------------------------------------------------------------------
create_Pitch() +
  geom_segment(data = clasico_1112 %>% filter(team.name == "Real Madrid", period == 1), 
               aes(x = location.x, y = location.y,                               
                   xend = pass.end_location.x, yend = pass.end_location.y),
               lineend = "round", size = 0.6, color = "red",
               arrow = arrow(length = unit(0.08, "inches"))) + 
  labs(title = "Lionel Messi, Passes Received", 
       subtitle = "La Liga, 2007/2008") + 
  coord_fixed(ratio = 105/100)

create_Pitch() +
  geom_segment(data = clasico_1112 %>% filter(team.name == "Real Madrid", period == 2), 
               aes(x = location.x, y = location.y,                               
                   xend = pass.end_location.x, yend = pass.end_location.y),
               lineend = "round", size = 0.6, 
               arrow = arrow(length = unit(0.08, "inches"))) + 
  labs(title = "Lionel Messi, Passes Received", 
       subtitle = "La Liga, 2007/2008") + 
  coord_fixed(ratio = 105/100)

create_Pitch() +
  geom_segment(data = clasico_1112 %>% filter(team.name == "Barcelona", period == 1), 
               aes(x = location.x, y = location.y,                               
                   xend = pass.end_location.x, yend = pass.end_location.y),
               lineend = "round", size = 0.6, color = "blue",
               arrow = arrow(length = unit(0.08, "inches"))) + 
  labs(title = "Lionel Messi, Passes Received", 
       subtitle = "La Liga, 2007/2008") + 
  coord_fixed(ratio = 105/100)


create_Pitch() +
  geom_segment(data = clasico_1112 %>% filter(team.name == "Barcelona", period == 2), 
               aes(x = location.x, y = location.y,                               
                   xend = pass.end_location.x, yend = pass.end_location.y),
               lineend = "round", size = 0.6, 
               arrow = arrow(length = unit(0.08, "inches"))) + 
  labs(title = "Lionel Messi, Passes Received", 
       subtitle = "La Liga, 2007/2008") + 
  coord_fixed(ratio = 105/100)


## ------------------------------------------------------------------------
create_Pitch() +
  geom_segment(data = messi_0708_pass_received, 
               aes(x = location.x, y = location.y,                               
                   xend = pass.end_location.x, yend = pass.end_location.y),
               lineend = "round", size = 0.6, 
               arrow = arrow(length = unit(0.08, "inches"))) + 
  labs(title = "Lionel Messi, Passes Received", 
       subtitle = "La Liga, 2007/2008") + 
  coord_fixed(ratio = 105/100)


## ------------------------------------------------------------------------
messi_0708_pass_received_clean <- messi_0708_pass_received %>% 
  ## Posiciones
  mutate(position_zone = case_when(
    position.id %in% c(1) ~ "Goalkeeper",
    position.id %in% c(2:8) ~ "Defender",
    position.id %in% c(9:16, 18:20) ~ "Midfielder",
    position.id %in% c(17, 21:25) ~ "Striker/Winger",
    TRUE ~ "NA"
  )) %>% 
  mutate(angle_round = round(pass.angle * 180 / pi / round.angle) *
           round.angle) %>% 
  add_count(player.name, name = "pass_n") %>% 
  add_count(player.name, angle_round, name = "angle_n") %>% 
  group_by(player.name) %>% 
  mutate(max_n = max(angle_n),
         angle_norm = angle_n / max_n) %>% 
  ungroup() %>% 
  group_by(angle_round, player.name, position_zone, pass_n) %>% 
  summarize(angle_norm = mean(angle_norm),
            distance = mean(pass.length),
            distance = if_else(distance > 30, 30, distance))

glimpse(messi_0708_pass_received_clean)


## ---- fig.height = 8, fig.width = 15-------------------------------------
round.angle <- 15

messi_0708_pass_received_clean %>% 
  filter(player.name == "Andrés Iniesta Luján") %>% 
  ggplot() +
  geom_bar(aes(x = angle_round, y = angle_norm, fill = distance),
           stat = "identity") +
  scale_y_continuous(limits = c(0, 1))+
  scale_x_continuous(breaks = seq(-180, 180, by = 90), 
                     limits = c(-180, 180)) +
  coord_polar(start = pi, direction = 1) +
  #RColorBrewer::brewer.pal.info
  colorspace::scale_fill_continuous_sequential(palette = "Blues", rev = TRUE) +
  # viridis::scale_fill_viridis("Distance (yards)", limits = c(0, 30),
  #                    na.value = "#FDE725FF") +
  labs(x = '', y = '', title = "Iniesta to Messi") +
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5),
        #legend.position = "none", #uncomment to remove colorbar
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA)) +
  facet_grid(position_zone ~ player.name)
  #
  facet_wrap(~player.name, nrow = 5)


## ------------------------------------------------------------------------
  #select(player.name, pass.recipient.name) %>% 
  # mutate(pass_duo = data.table::transpose(as.list(player.name,
  #                                                 pass.recipient.name)))
  # mutate(pass_duo = transpose(list(player.name, pass.recipient.name))) %>% glimpse()
  # mutate(pass_duo = pmap(player.name, pass.recipient.name, ~ as.list))
  # as_tibble() %>% 
  # mutate(pass_duo = list(player.name, pass.recipient.name))
  # unite(col = pass_duo, player.name, pass.recipient.name, sep = " - ") %>% 
  #distinct(player.name, pass.recipient.name, .keep_all = TRUE) %>% 


## ------------------------------------------------------------------------
messi_passduo <- messi_0708_pass_received %>% 
  mutate(pass_duo = map2(player.name, pass.recipient.name, ~c(.x, .y))) %>% 
  add_count(player.name, pass.recipient.name) %>% 
  ggplot(aes(x = pass_duo)) +
  #geom_point(fill = "#a70042") +
  # geom_segment(aes(x = pass_duo, y = 0, 
  #                  xend = pass_duo, yend = n)) +
  #ggalt::geom_lollipop(fill = "#a70042") +
  geom_bar(fill = "#a70042") + 
  scale_x_upset(n_intersections = 10,
                expand = c(0.01, 0.01)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Pass Duos with Messi (2007-2008 Season)",
       subtitle = str_wrap("Xavi passed to Messi more than double the amount of times of the 2nd highest passer, Zambrotta", width = 70),
       caption = "Source: StatsBomb",
       x = NULL, y = "Number of Passes") +
  theme_combmatrix(
    text = element_text(family = "Roboto Condensed", color = "#a70042"),
    axis.text.y = element_text(family = "Roboto Condensed", color = "#a70042"),
    #plot.background = element_rect(fill = "#004c99"),
    panel.background = element_rect(fill = "#004c99"),
    combmatrix.panel.point.color.fill = "#a70042",
    combmatrix.panel.line.color.fill = "#a70042",
    panel.grid.major.x = element_blank(),
    axis.ticks = element_blank())
  

messi_passduo


## ------------------------------------------------------------------------
messi_passduo <- messi_pass_received %>% 
  filter(season_name == "2011/2012") %>% 
  mutate(pass_duo = map2(player.name, pass.recipient.name, ~c(.x, .y))) %>% 
  add_count(player.name, pass.recipient.name) %>% 
  ggplot(aes(x = pass_duo)) +
  #geom_point(fill = "#a70042") +
  # geom_segment(aes(x = pass_duo, y = 0, 
  #                  xend = pass_duo, yend = n)) +
  #ggalt::geom_lollipop(fill = "#a70042") +
  geom_bar(fill = "#a70042") + 
  scale_x_upset(n_intersections = 10,
                expand = c(0.01, 0.01)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Passes to Messi (2011-2012 Season)",
       subtitle = str_wrap("Xavi passed to Messi more than double the amount of times of the 2nd highest passer, Zambrotta", width = 70),
       caption = "Source: StatsBomb",
       x = NULL, y = "Number of Passes") +
  theme_combmatrix(
    text = element_text(family = "Roboto Condensed", color = "#a70042"),
    axis.text.y = element_text(family = "Roboto Condensed", color = "#a70042"),
    panel.background = element_rect(fill = "#004c99"),
    combmatrix.panel.point.color.fill = "#a70042",
    combmatrix.panel.line.color.fill = "#a70042",
    combmatrix.panel.line.size = 0,
    panel.grid.major.x = element_blank(),
    axis.ticks = element_blank())
  
messi_passduo


## ------------------------------------------------------------------------
messi_pass_received$n %>% unique()


## ------------------------------------------------------------------------
messi_pass_nested <- messi_pass_received %>% 
  group_by(season_id, season_name) %>% 
  nest() %>%
  mutate(plot = map2(
    data, season_name, 
    ~ ggplot(data = .x, aes(x = pass_duo)) +
      geom_bar(fill = "#a70042") + 
      scale_x_upset(n_intersections = 10,
                    expand = c(0.01, 0.01)) +
      scale_y_continuous(expand = c(0, 0)) +
      labs(title = glue::glue("Passes to Messi ({.y})"),
           caption = "Source: StatsBomb",
           x = NULL, y = "Number of Passes") +
      theme_combmatrix(
        text = element_text(family = "Roboto Condensed", 
                            color = "#a70042"),
        axis.text.y = element_text(family = "Roboto Condensed", 
                                   color = "#a70042"),
        panel.background = element_rect(fill = "#004c99"),
        combmatrix.panel.point.color.fill = "#a70042",
        combmatrix.panel.line.color.fill = "#a70042",
        panel.grid.major.x = element_blank(),
        axis.ticks = element_blank())))

messi_pass_nested$plot[[8]]


## ------------------------------------------------------------------------
glimpse(tidy_movies)
tidy_movies


## ------------------------------------------------------------------------
messi_pass_received %>% 
  filter(season_name == "2011/2012") %>% 
  ggplot(aes(x = pass_duo, y = pass_num)) +
  geom_point() 


## ------------------------------------------------------------------------
tidy_movies %>%
  distinct(title, year, length, .keep_all=TRUE) %>%
  ggplot(aes(x=Genres)) +
    geom_bar() +
    scale_x_upset(order_by = "degree", n_sets = 5) +
  theme_combmatrix()


## ------------------------------------------------------------------------
messi_data_clean %>% 
  filter(team.name == "Barcelona",
         season_name == "2011/2012",
         type.name == "Shot",
         shot.type.name != "Penalty") %>% 
  #select(contains("shot")) %>% 
  top_n(n = 5, wt = shot.statsbomb_xg) %>% 
  select(player.name, 
         location.x, location.y, 
         contains("shot"), contains("goalkeeper")) %>% 
  arrange(desc(shot.statsbomb_xg))


messi_data_clean %>% 
  filter(team.name == "Barcelona",
         season_name == "2011/2012",
         type.name == "Shot",
         shot.type.name != "Penalty") %>% 
  #select(contains("shot")) %>% 
  top_n(n = 5, wt = shot.statsbomb_xg) %>% 
  filter(index == 1552) -> shotframeone


## ---- fig.height=6, fig.width=10-----------------------------------------
shotframeone$shot.freeze_frame -> freeze_df

freeze_df_loc <- freeze_df %>% 
  reduce(bind_rows) %>% 
  mutate(location.x = (map(location, 1) %>% unlist()), 
         location.y = (map(location, 2) %>% unlist()))


## ------------------------------------------------------------------------
shotframeone %>% 
  mutate(villa.x = location.x,
         villa.y = location.y) %>% 
  select(-location) %>% 
  full_join(freeze_df_loc %>% select(-location)) %>% 
  select(player.name, 
         location.x, location.y, villa.x, villa.y,
         contains("shot"), contains("goalkeeper"),
         teammate) %>% 
  mutate(rlocation.y = (location.y - 80) * -1)


## ------------------------------------------------------------------------
shotframeone %>% 
  mutate(villa.x = location.x,
         villa.y = location.y) %>% 
  select(-location) %>% 
  full_join(freeze_df_loc %>% select(-location)) %>% 
  select(player.name, 
         location.x, location.y, villa.x, villa.y,
         contains("shot"), contains("goalkeeper"),
         teammate, minute, second,
         shot.body_part.name, shot.type.name,
         shot.statsbomb_xg) %>% 
  mutate(rlocation.y = (location.y - 80) * -1) %>% 
  fill(minute, second,
       shot.body_part.name, shot.type.name,
       shot.statsbomb_xg) -> villagoal_df

ggplot(villagoal_df) +
  annotate_pitch(dimensions = pitch_statsbomb,
                 limits = FALSE) +
  theme_pitch() +
  theme(text = element_text(family = "Roboto Condensed")) +
  coord_flip(xlim = c(55, 122),
             ylim = c(-1, 80)) +
  geom_point(aes(x = location.x, y = location.y, 
                 color = player.name)) +
  geom_point(aes(x = villa.x, y = villa.y, 
                 color = "green")) +
  geom_segment(aes(x = villa.x, y = villa.y,
                   xend = shot.end_location.x,
                   yend = shot.end_location.y)) +
  labs(title = glue::glue("Goal for David Villa ({villagoal_df$minute}:{villagoal_df$second})"),
       subtitle = glue::glue("
                             {villagoal_df$shot.body_part.name} From {villagoal_df$shot.type.name}
                             xG: {villagoal_df$shot.statsbomb_xg}")) +
  guides(color = FALSE)


## ------------------------------------------------------------------------
shotframeone %>% 
  mutate(villa.x = location.x,
         villa.y = location.y) %>% 
  select(-location) %>% 
  full_join(freeze_df_loc %>% select(-location)) %>% 
  select(player.name, 
         location.x, location.y, villa.x, villa.y,
         contains("shot"), contains("goalkeeper"),
         teammate) %>% 
  mutate(rlocation.y = (location.y - 80) * -1) %>% 
  ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb,
                 limits = FALSE) +
  theme_pitch() +
  theme(text = element_text(family = "Roboto Condensed")) +
  geom_point(aes(x = location.x, y = location.y, 
                 color = player.name)) +
  geom_point(aes(x = villa.x, y = villa.y, 
                 color = "green")) +
  geom_segment(aes(x = villa.x, y = villa.y,
                   xend = shot.end_location.x,
                   yend = shot.end_location.y)) +
  labs(title = "Goal for David Villa")


## ------------------------------------------------------------------------
messi_data_clean %>% 
  filter(team.name == "Real Madrid") %>% 
  select(team.name, match_id) %>% 
  unique()


## ------------------------------------------------------------------------
clasico_1112 <- messi_data_clean %>% 
  filter(match_id == 69334) %>% 
  mutate(shot.statsbomb_xg = if_else(is.na(shot.statsbomb_xg), 
                                     0, shot.statsbomb_xg))

clasico_1112_xg <- clasico_1112 %>% 
  group_by(team.name) %>% 
  summarize(tot_xg = sum(shot.statsbomb_xg) %>% signif(digits = 2)) %>% 
  mutate(team_label = glue::glue("{team.name}: {tot_xg} xG"))

clasico_1112 <- clasico_1112 %>% 
  left_join(clasico_1112_xg, by = "team.name")


## ------------------------------------------------------------------------
clasico_1112$shot.statsbomb_xg

clasico_1112 %>% 
  group_by(team.name) %>% 
  summarize(tot_xg = sum(shot.statsbomb_xg))


## ------------------------------------------------------------------------
clasico_1112$shot.outcome.name %>% unique()


## ---- fig.height=6, fig.width=10-----------------------------------------
windowsFonts(robotoc = windowsFont("Roboto Condensed"))

clasico_xg_timelineplot <- clasico_1112 %>% 
  ggplot() +
  geom_segment(x = 0, xend = 94,
               y = 0, yend = 0) +
  geom_rect(data = clasico_1112 %>% filter(shot.outcome.name == "Goal"),
            aes(xmin = minute - 2, xmax = minute + 2,
                ymin = -0.005, ymax = 0.005), 
            alpha = 0.3, fill = "green") +
  geom_point(data = clasico_1112 %>% filter(shot.statsbomb_xg != 0),
             shape = 21, stroke = 1.5,
             aes(x = minute, y = 0, 
                 size = shot.statsbomb_xg, fill = team.name)) +
  scale_fill_manual(values = c("Barcelona" = "#a50044",
                                "Real Madrid" = "white")) +
  facet_wrap(vars(team_label), ncol = 1) +
  scale_x_continuous(breaks = seq(0, 90, by = 5),
                     labels = c(seq(0, 40, by = 5), "HT", 
                                seq(50, 85, by = 5), "FT"),
                     limits = c(-3, 95),
                     expand = c(0.01, 0)) +
  scale_y_continuous(limits = c(-0.005, 0.005)) +
  scale_size(range = c(2, 6)) +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(size = 16, family = "robotoc"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())
  
clasico_xg_timelineplot


## ------------------------------------------------------------------------
barc_sevil <- messi_data_clean %>% 
  filter(match_id == 69289) %>% 
  mutate(shot.statsbomb_xg = if_else(is.na(shot.statsbomb_xg), 
                                     0, shot.statsbomb_xg))

barc_sevil_xg <- barc_sevil %>% 
  group_by(team.name) %>% 
  summarize(tot_xg = sum(shot.statsbomb_xg) %>% round(digits = 2)) %>% 
  mutate(team_label = glue::glue("{team.name}: {tot_xg} xG"))

barc_sevil <- barc_sevil %>% 
  left_join(barc_sevil_xg, by = "team.name")


## ---- fig.height=6, fig.width=10-----------------------------------------
windowsFonts(robotoc = windowsFont("Roboto Condensed"))

barc_sevil_xg_timelineplot <- barc_sevil %>% 
  ggplot() +
  geom_segment(x = 0, xend = 94,
               y = 0, yend = 0) +
  geom_rect(data = barc_sevil %>% filter(shot.outcome.name == "Goal"),
            aes(xmin = minute - 2, xmax = minute + 2,
                ymin = -0.005, ymax = 0.005), 
            alpha = 0.3, fill = "green") +
  geom_point(data = barc_sevil %>% filter(shot.statsbomb_xg != 0),
             shape = 21, stroke = 1.5,
             aes(x = minute, y = 0, 
                 size = shot.statsbomb_xg, fill = team.name)) +
  scale_fill_manual(values = c("Barcelona" = "#a50044",
                                "Sevilla" = "#CD853F")) +
  facet_wrap(vars(team_label), ncol = 1) +
  scale_x_continuous(breaks = seq(0, 90, by = 5),
                     labels = c(seq(0, 40, by = 5), "HT", 
                                seq(50, 85, by = 5), "FT"),
                     limits = c(-3, 95),
                     expand = c(0.01, 0)) +
  scale_y_continuous(limits = c(-0.005, 0.005)) +
  scale_size(range = c(2, 6)) +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(size = 16, family = "robotoc"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())
  
barc_sevil_xg_timelineplot


## ------------------------------------------------------------------------
# clasico_1112_xg <- clasico_1112 %>% 
#   group_by(team.name) %>% 
#   summarize(tot_xg = sum(shot.statsbomb_xg)) %>% 
#   mutate(team_label = glue::glue("{team.name}: {tot_xg} xG"))
# 
# clasico_1112 <- clasico_1112 %>% 
#   left_join(clasico_1112_xg, by = "team.name")

clasico_rollsum <- clasico_1112 %>% 
  group_by(minute, team.name, period) %>% 
  summarize(sumxg = sum(shot.statsbomb_xg)) %>% 
  ungroup() %>% 
  group_by(team.name) %>% 
  mutate(rollsum = lag(cumsum(sumxg)),
         rollsum = if_else(is.na(rollsum), 0, rollsum)) %>% 
  select(team.name, minute, rollsum)

# 
# first_min <- clasico_1112$minute %>% unique() %>% first()
# last_min <- clasico_1112$minute %>% unique() %>% last()
# minute <- c(first_min:last_min)
# team.name <- c("Real Madrid", "Barcelona")
# 
# crossing(minute, team.name)
# 
# crossing(minute, team.name) %>% 
#   left_join(clasico_rollsum, by = c("minute", "team.name")) %>% 
#   filter(team.name == "Real Madrid") %>% 
#   tail(50)
# 
# 
# clasico_rollsum %>% 
#   left_join(crossing(minute, team.name), by = c("minute", "team.name")) %>% 
#   distinct()
# 
# clasico_rollsum %>% 
#   distinct()


## ---- fig.height=6, fig.width=8------------------------------------------
tot_clasico_df <- clasico_1112_xg %>% 
  pull(tot_xg)



windowsFonts(robotoc = windowsFont("Roboto Condensed"))

clasico_rollsum %>% 
  ggplot(aes(x = minute, y = rollsum, 
             group = team.name, color = team.name)) +
  geom_line(size = 2.5) +
  scale_color_manual(values = c("Barcelona" = "#a50044",
                                 "Real Madrid" = "#000000")) +
  scale_x_continuous(breaks = c(seq(0, 90, by = 5), 94),
                     labels = c(seq(0, 40, by = 5), "HT", 
                                seq(50, 90, by = 5), "FT"),
                     expand = c(0.01, 0),
                     limits = c(0, 94)) +
  scale_y_continuous(sec.axis = sec_axis(~ ., breaks = tot_clasico_df)) +
  labs(title = glue("
                    Real Madrid: 1
                    Barcelona: 3"),
       subtitle = "December 10, 2011",
       x = NULL,
       y = "Expected Goals") +
  theme_minimal() +
  theme(text = element_text(family = "robotoc"),
        plot.title = element_text(size = 40, family = "robotoc",
                                  color = "red"),
        plot.subtitle = element_text(size = 18, family = "robotoc",
                                     color = "grey20"),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16, face = "bold"),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 16),
        legend.position = c(0.25, 0.95),
        legend.direction = "horizontal",
        legend.title = element_blank())


## ---- fig.height=6, fig.width=8------------------------------------------
tot_clasico_df <- clasico_1112_xg %>% 
  pull(tot_xg)

windowsFonts(robotoc = windowsFont("Roboto Condensed"))

clasico_rollsum %>% 
  ggplot(aes(x = minute, y = rollsum, 
             group = team.name, color = team.name)) +
  geom_line(size = 2.5) +
  scale_color_manual(values = c("Barcelona" = "#a50044",
                                 "Real Madrid" = "#000000")) +
  scale_x_continuous(breaks = seq(0, 95, by = 5),
                     labels = c(seq(0, 40, by = 5), "HT", 
                                seq(50, 90, by = 5), "FT"),
                     expand = c(0.01, 0)) +
  scale_y_continuous(sec.axis = sec_axis(~ ., breaks = tot_clasico_df)) +
  labs(title = glue("
                    Real Madrid: 1
                    Barcelona: 3"),
       subtitle = "December 10, 2011",
       x = NULL,
       y = "Expected Goals") +
  theme_minimal() +
  theme(text = element_text(family = "robotoc"),
        plot.title = element_text(size = 40, family = "robotoc",
                                  color = "red"),
        plot.subtitle = element_text(size = 18, family = "robotoc",
                                     color = "grey20"),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16, face = "bold"),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 16),
        legend.position = c(0.25, 0.95),
        legend.direction = "horizontal",
        legend.title = element_blank())


## ------------------------------------------------------------------------
ggsave(filename = here::here("Lionel Messi/output/clasico_accumulated_xGplot.png"),
       height = 6, width = 8)


## ------------------------------------------------------------------------
# barc_sevil_xg <- barc_sevil %>% 
#   group_by(team.name) %>% 
#   summarize(tot_xg = sum(shot.statsbomb_xg) %>% round(digits = 2)) %>% 
#   mutate(team_label = glue::glue("{team.name}: {tot_xg} xG"))
# 
# barc_sevil <- barc_sevil %>% 
#   left_join(barc_sevil_xg, by = "team.name")

#rolling_sum <- rollify(.f = sum, window = 5)

barc_sevil_rollsum <- barc_sevil %>% 
  group_by(minute, team.name, team_label) %>% 
  summarize(sumxg = sum(shot.statsbomb_xg)) %>% 
  ungroup() %>% 
  group_by(team.name) %>% 
  mutate(rollsum = lag(cumsum(sumxg)),
         rollsum = if_else(is.na(rollsum), 0, rollsum)) %>% 
  select(team.name, minute, rollsum)


## ---- fig.height=6, fig.width=8------------------------------------------
tot_df <- barc_sevil_xg %>% 
  pull(tot_xg)

windowsFonts(robotoc = windowsFont("Roboto Condensed"))

barc_sevil_rollsum %>% 
  ggplot(aes(x = minute, y = rollsum, 
             group = team.name, color = team.name)) +
  geom_line(size = 2.5) +
  scale_color_manual(values = c("Barcelona" = "#a50044",
                                 "Sevilla" = "#CD853F")) +
  scale_x_continuous(breaks = seq(0, 90, by = 5),
                     labels = c(seq(0, 40, by = 5), "HT", 
                                seq(50, 85, by = 5), "FT")) +
  scale_y_continuous(sec.axis = sec_axis(~ ., breaks = tot_df)) +
  labs(title = glue("
                    Barcelona: 5
                    Sevilla: 1"),
       subtitle = "------, 2011",
       x = NULL,
       y = "Expected Goals") +
  theme_minimal() +
  theme(text = element_text(family = "robotoc"),
        plot.title = element_text(size = 40, family = "robotoc",
                                  color = "red"),
        plot.subtitle = element_text(size = 18, family = "robotoc",
                                     color = "grey20"),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16, face = "bold"),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 16),
        legend.position = c(0.25, 0.95),
        legend.direction = "horizontal",
        legend.title = element_blank())


## ------------------------------------------------------------------------
clasico_1112$location.x

clasico_1112$type.name %>% unique()


## ------------------------------------------------------------------------
rolling_sum <- rollify(.f = sum, window = 5)

# clasico_1112 %>% 
#   filter(type.name == "Pass", 
#          location.x >= 80) %>% 
#   select(team.name, minute) %>% 
#   group_by(team.name, minute) %>% 
#   count() %>% 
#   ungroup() %>% 
#   group_by(team.name) %>% 
#   mutate(rollsum = rolling_sum(n),
#          rollsum = if_else(is.na(rollsum), 0L, rollsum))

roll_final_pass <- clasico_1112 %>% 
  group_by(team.name, minute) %>% 
  mutate(count = case_when(
    type.name == "Pass" & location.x >= 80 ~ 1L,
    TRUE ~ 0L
  )) %>% 
  select(team.name, minute, count) %>% 
  ungroup() %>% 
  group_by(team.name, minute) %>% 
  summarize_all(sum) %>% 
  ungroup() %>% 
  group_by(team.name) %>% 
  mutate(rollsum = rolling_sum(count),
         rollsum = if_else(is.na(rollsum), 0L, rollsum))

## crossing ALL mins
first_min <- clasico_1112$minute %>% unique() %>% first()
last_min <- clasico_1112$minute %>% unique() %>% last()
minute <- c(first_min:last_min)
team.name <- c("Real Madrid", "Barcelona")

crossing(minute, team.name)

roll_clasico_pass <- crossing(minute, team.name) %>%
  left_join(roll_final_pass, by = c("minute", "team.name")) %>% 
  group_by(team.name) %>% 
  slice(which(row_number() %% 5 == 1) )



## ---- fig.height=6, fig.width=10-----------------------------------------
windowsFonts(robotoc = windowsFont("Roboto Condensed"))

finalthird_rollingplot <- roll_clasico_pass %>% 
  ggplot(aes(x = minute, y = rollsum, 
             group = team.name)) +
  geom_line(data = roll_clasico_pass,
            size = 1.25) +
  geom_point(data = roll_clasico_pass,
             aes(fill = team.name),
             size = 5, shape = 21, stroke = 2.5) +
  scale_x_continuous(breaks = seq(0, 90, by = 5),
                     labels = c(seq(0, 40, by = 5), "HT", 
                                seq(50, 85, by = 5), "FT"),
                     limits = c(-3, 95),
                     expand = c(0.01, 0)) +
  scale_y_continuous(breaks = seq(0, 30, by = 5),
                     labels = seq(0, 30, by = 5)) +
  scale_fill_manual(values = c("Barcelona" = "#a50044",
                                 "Real Madrid" = "#FFFFFF")) +
  labs(title = glue("
                    Real Madrid: 1
                    Barcelona: 3"),
       subtitle = "December 10, 2011",
       x = NULL,
       y = "Final Third Passes") +
  theme_minimal() +
  theme(text = element_text(family = "robotoc"),
        plot.title = element_text(size = 40, family = "robotoc",
                                  color = "red"),
        plot.subtitle = element_text(size = 18, family = "robotoc",
                                     color = "grey20"),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16, face = "bold"),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 16),
        legend.position = c(0.25, 0.95),
        legend.direction = "horizontal",
        legend.title = element_blank())

finalthird_rollingplot


## ---- fig.height=6, fig.width=10-----------------------------------------
windowsFonts(robotoc = windowsFont("Roboto Condensed"))

finalthird_rollingplot <- roll_final_pass %>% 
  ggplot(aes(x = minute, y = rollsum, 
             group = team.name)) +
  geom_line(data = roll_final_pass %>% 
              slice(which(row_number() %% 5 == 1) ),
            size = 1.25) +
  geom_point(data = roll_final_pass %>% 
               slice(which(row_number() %% 5 == 1) ),
             aes(fill = team.name),
             size = 5, shape = 21, stroke = 2.5) +
  scale_x_continuous(breaks = seq(0, 90, by = 5),
                     labels = c(seq(0, 40, by = 5), "HT", 
                                seq(50, 85, by = 5), "FT"),
                     limits = c(-3, 95),
                     expand = c(0.01, 0)) +
  scale_y_continuous(breaks = seq(0, 30, by = 5),
                     labels = seq(0, 30, by = 5)) +
  scale_fill_manual(values = c("Barcelona" = "#a50044",
                                 "Real Madrid" = "#FFFFFF")) +
  labs(title = glue("
                    Real Madrid: 1
                    Barcelona: 3"),
       subtitle = "December 10, 2011",
       x = NULL,
       y = "Final Third Passes") +
  theme_minimal() +
  theme(text = element_text(family = "robotoc"),
        plot.title = element_text(size = 40, family = "robotoc",
                                  color = "red"),
        plot.subtitle = element_text(size = 18, family = "robotoc",
                                     color = "grey20"),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16, face = "bold"),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 16),
        legend.position = c(0.25, 0.95),
        legend.direction = "horizontal",
        legend.title = element_blank())

finalthird_rollingplot


## ------------------------------------------------------------------------
roll_fin_sev <- barc_sevil %>% 
  group_by(team.name, minute) %>% 
  mutate(count = case_when(
    type.name == "Pass" & location.x >= 80 ~ 1L,
    TRUE ~ 0L
  )) %>% 
  select(team.name, minute, count) %>% 
  ungroup() %>% 
  group_by(team.name, minute) %>% 
  summarize_all(sum) %>% 
  ungroup() %>% 
  group_by(team.name) %>% 
  mutate(rollsum = rolling_sum(count),
         rollsum = if_else(is.na(rollsum), 0L, rollsum))

## crossing ALL mins
first_min <- barc_sevil$minute %>% unique() %>% first()
last_min <- barc_sevil$minute %>% unique() %>% last()
minute <- c(first_min:last_min)
team.name <- c("Sevilla", "Barcelona")

crossing(minute, team.name)

crossing(minute, team.name) %>%
  left_join(roll_fin_sev, by = c("minute", "team.name")) %>% 
  group_by(team.name) %>% 
  slice(which(row_number() %% 5 == 1) )


## ---- fig.height=6, fig.width=10-----------------------------------------
windowsFonts(robotoc = windowsFont("Roboto Condensed"))

finalthird_barsev_rollingplot <- roll_fin_sev %>% 
  ggplot(aes(x = minute, y = rollsum, 
             group = team.name)) +
  geom_line(data = roll_fin_sev %>% 
              slice(which(row_number() %% 5 == 1) ),
            size = 1.25) +
  geom_point(data = roll_fin_sev %>% 
               slice(which(row_number() %% 5 == 1) ),
             aes(fill = team.name),
             size = 5, shape = 21, stroke = 2.5) +
  scale_x_continuous(breaks = seq(0, 90, by = 5),
                     labels = c(seq(0, 40, by = 5), "HT", 
                                seq(50, 85, by = 5), "FT"),
                     limits = c(-3, 95),
                     expand = c(0.01, 0)) +
  scale_y_continuous(breaks = seq(0, 30, by = 5),
                     labels = seq(0, 30, by = 5)) +
  scale_fill_manual(values = c("Barcelona" = "#a50044",
                                 "Sevilla" = "#CD853F")) +
  labs(title = glue("
                    Barcelona: 5
                    Sevilla: 0"),
       subtitle = "October 30, 2010",
       x = NULL,
       y = "Final Third Passes") +
  theme_minimal() +
  theme(text = element_text(family = "robotoc"),
        plot.title = element_text(size = 40, family = "robotoc",
                                  color = "red"),
        plot.subtitle = element_text(size = 18, family = "robotoc",
                                     color = "grey20"),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16, face = "bold"),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 16),
        legend.position = c(0.25, 0.95),
        legend.direction = "horizontal",
        legend.title = element_blank())

finalthird_barsev_rollingplot


## ------------------------------------------------------------------------
clasico_xg_timelineplot

finalthird_rollingplot


## ---- fig.height=14, fig.width=10, message=FALSE-------------------------
plot_grid(finalthird_rollingplot,
          clasico_xg_timelineplot, ncol = 1,
          align = "v", axis = "l")


## ------------------------------------------------------------------------
ggsave(filename = here::here("Lionel Messi/clasico_match_plot.png"),
       height = 14, width = 10)


## ---- fig.height=14, fig.width=10, message=FALSE-------------------------
plot_grid(finalthird_barsev_rollingplot,
          barc_sevil_xg_timelineplot, ncol = 1,
          align = "hv", axis = "l")


## ------------------------------------------------------------------------
ggsave(filename = here::here("Lionel Messi/barcasev_match_plot.png"),
       height = 14, width = 10)


## ------------------------------------------------------------------------
clasico_1112 %>% 
  group_by(team.name) %>% 
  summarize(pos_time = sum(TimeInPoss)) %>% 
  mutate(total = sum(pos_time),
         pos_perc = pos_time / total)


## ------------------------------------------------------------------------
clasico_1112$possession %>% unique()

clasico_1112$StartOfPossession

clasico_1112$StartOfPossession %>% unique()

clasico_1112$TimeToPossEnd %>% unique()

clasico_1112$TimeInPoss %>% unique() %>% head(30)


## ------------------------------------------------------------------------
clasico_1112 %>% 
  mutate(field_zone = case_when(
    location.x >= 102 ~ "Danger Zone",
    location.x < 102 & location.x > 80 ~ "Attacking Third",
    location.x <= 80 ~ "Safe Zone"
  )) %>% 
  select(#team.name, 
         possession_team.name, 
         field_zone, TimeInPoss, minute,
         TimeToPossEnd, StartOfPossession,
         timestamp, type.name, play_pattern.name) %>% 
  filter(!is.na(field_zone)) %>% 
  distinct(timestamp, .keep_all = TRUE) %>% View()



  group_by(team.name, field_zone, timestamp, TimeInPoss) %>% 
  mutate(sum_time = TimeInPoss) %>% 
  ungroup() %>% 
  group_by(field_zone) %>% 
  mutate(tot_time = sum(sum_time),
         perc_time = (sum_time / tot_time) %>% signif(digits = 2)) %>% 
  arrange(field_zone)


## ------------------------------------------------------------------------
clasico_1112 %>% 
  group_by(team.name) %>% 
  filter(type.name %in% c("Half Start", "Half End"))


## ------------------------------------------------------------------------
pass_comp_df <- messi_data_clean %>% 
  filter(#season_name == "2011/2012",
         team.name == "Barcelona") %>% 
  mutate(pass.outcome.name = fct_explicit_na(pass.outcome.name, "Complete"),
         pass.outcome.name = ifelse(pass.outcome.name != "Complete",
                                    "Incomplete", "Complete")) %>%
  filter(type.name == "Pass",
         !play_pattern.name %in% c("From Corner", "From Free Kick",
                                   "From Throw In")) %>% 
  select(player.name, pass.recipient.name, 
         season_name, position.name, 
         pass.outcome.name,
         location.x, location.y,
         pass.end_location.x, pass.end_location.y,
         contains("pass")) #%>% View()

glimpse(pass_comp_df)


## ------------------------------------------------------------------------
pass_comp_df %>% 
  select(player.name, season_name, pass.outcome.name) %>% 
  group_by(player.name, pass.outcome.name, season_name) %>% 
  add_tally() %>% 
  ungroup() %>% 
  distinct() %>% #arrange(player.name)
  group_by(player.name, season_name) %>% 
  mutate(sum_pass = sum(n),
         pass_perc = (n / sum_pass) %>% round(digits = 2)) %>% 
  ungroup() %>% 
  # group_by(season_name) %>% 
  # mutate(first_quantile = quantile(sum_pass)[2]) %>% 
  # ungroup() %>% 
  filter(pass.outcome.name == "Complete") %>% 
  # filter(sum_pass > first_quantile + 1.58 * (IQR(sum_pass))) %>% 
  select(player.name, season_name, #pass.outcome.name, 
         n, sum_pass, pass_perc) %>% #arrange(player.name)
  ggplot(aes(x = pass_perc, y = season_name)) +
  ggbeeswarm::geom_beeswarm(groupOnX = FALSE, size = 3, cex = 2)


## ------------------------------------------------------------------------


