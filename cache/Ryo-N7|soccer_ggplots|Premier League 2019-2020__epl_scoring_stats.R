## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---- message=FALSE, warning=FALSE---------------------------------------
pacman::p_load(tidyverse, polite, scales, ggimage, ggforce,
               understatr,
               jsonlite, xml2, qdapRegex, stringi, stringr,
               rvest, glue, extrafont, ggrepel, magick, ggtext)
loadfonts(quiet = TRUE)


## ------------------------------------------------------------------------
get_script <- function(x) {
  as.character(html_nodes(x, "script"))
}

# subset data element of html page
get_data_element <- function(x, element_name) {
  stri_unescape_unicode(str_subset(x, element_name))
}

# fix json element for parsing
fix_json <- function(x) {
  str_subset(
    unlist(
      rm_square(
        x, extract = TRUE, include.markers = TRUE
      )
    ),
    "\\[\\]", negate = TRUE
  )
}

# get player name part of html page
get_player_name <- function(x) {

  player_name <- html_nodes(x, ".header-wrapper:first-child")
  trimws(html_text(player_name))
}


## ------------------------------------------------------------------------
EPL2019 <- get_league_teams_stats("EPL", "2019")

glimpse(EPL2019)

EPL2019_teams <- EPL2019 %>% 
  select(team_name) %>% unique() %>% pull() %>% 
  str_replace_all(" ", "_")

glimpse(EPL2019_teams)


## ------------------------------------------------------------------------
get_team_shots_data <- function(team_name) {
  
  url <- glue::glue("https://understat.com/team/{team_name}/2019")
  
  team_page <- polite::bow(url)
  
  team_data <- polite::scrape(team_page) %>% 
    get_script()
  
  team_situation_data <- get_data_element(team_data, "statisticsData") %>% 
    str_replace(., "\\{\"situation\"",  "[\\{\"situation\"") %>% 
    str_replace(., "\\}'\\)", "\\}]'\\)") %>% 
    fix_json() %>% 
    fromJSON()
  
  ## stats by theme
  
  situation_df <- team_situation_data %>% 
    .[[1]] %>% 
    unlist() %>% 
    enframe()
  
  formation_df <- team_situation_data %>% 
    .[[2]] %>% 
    unlist() %>% 
    enframe()
  
  gamestate_df <- team_situation_data %>% 
    .[[3]] %>% 
    unlist() %>% 
    enframe()
  
  minutes_df <- team_situation_data %>% 
    .[[4]] %>% 
    unlist() %>% 
    enframe()
  
  zone_df <- team_situation_data %>% 
    .[[5]] %>% 
    unlist() %>% 
    enframe()
  
  speed_df <- team_situation_data %>% 
    .[[6]] %>% 
    unlist() %>% 
    enframe()
  
  shot_df <- team_situation_data %>% 
    .[[7]] %>% 
    unlist() %>% 
    enframe() 
  
  data_df <- list(situation_df, formation_df, gamestate_df, 
                  minutes_df, zone_df, speed_df, shot_df)
  
  print(paste(team_name, " done!"))
  
  return(data_df)
}


## ------------------------------------------------------------------------
EPL_shots_data_df_raw <- map(EPL2019_teams, 
                             ~get_team_shots_data(team_name = .x)) %>% 
  set_names(EPL2019_teams)


## ------------------------------------------------------------------------
saveRDS(EPL_shots_data_df_raw, 
        file = here::here("data/EPL_shots_data_df_raw.RDS"))


## ------------------------------------------------------------------------
EPL_shots_data_df_raw <- readRDS(
  file = here::here("data/EPL_shots_data_df_raw.RDS"))


## ------------------------------------------------------------------------
EPL_gamestate_data_df <- EPL_shots_data_df_raw %>% 
  ## pick out 3rd data frame which is game states
  map(3) %>% 
  bind_rows(.id = "team_name") %>% 
  mutate(name = str_replace(name, "Goal diff ", ""),
         name = str_replace(name, "against.", "against_")) %>% 
  separate(name, c("game_state", "metric"), 
           sep = "\\.", extra = "merge") %>% 
  filter(metric != "stat") %>% 
  mutate(state = case_when(
    game_state == "0" ~ "Drawing",
    game_state %in% c("> +1", "+1") ~ "Winning",
    game_state %in% c("-1", "< -1") ~ "Losing",
    TRUE ~ NA_character_
  )) %>% 
  filter(metric == "time") %>% 
  select(-game_state, -metric) %>% 
  mutate(value = as.numeric(value)) %>% 
  group_by(state, team_name) %>% 
  summarize(value = sum(value)) %>% 
  ungroup() %>% 
  group_by(team_name) %>% 
  mutate(total = sum(value)) %>% 
  ungroup() %>% 
  group_by(state, team_name) %>% 
  mutate(percentage = value / total) %>% 
  ungroup() %>% 
  arrange(team_name) %>% 
  mutate(team_name = str_replace(team_name, "_", " "),
         team_name = as_factor(team_name),
         state = as_factor(state),
         state = fct_relevel(state, "Losing", "Drawing", "Winning"),
         percentage = percentage * 100)

glimpse(EPL_gamestate_data_df)


## ------------------------------------------------------------------------
result_vals <- c("Winning" = "darkgreen", "Drawing" = "grey", "Losing" = "red")

EPL_gamestate_df_clean <- EPL_gamestate_data_df %>% 
  pivot_wider(names_from = "state", values_from = "percentage") %>% 
  mutate(Winning = if_else(is.na(Winning), 0, Winning),
         Drawing = if_else(is.na(Drawing), 0, Drawing),
         Losing = if_else(is.na(Losing), 0, Losing)) %>% 
  group_by(team_name) %>% 
  summarize_all(sum) %>% 
  ungroup() %>% 
  mutate(equals = Winning + Drawing + Losing) %>% 
  select(-equals, -value, -total) %>% 
  mutate(win_half = Winning / 2,
         draw_half =(  (Winning + Drawing) - Winning  )  / 2 + (Winning),
         lose_half = ( (Winning + Drawing + Losing) - (Winning + Drawing) )  / 2 + (Winning + Drawing)) %>% 
  mutate(team_name = fct_relevel(team_name, team_winning_order),
         team_name = fct_rev(team_name)) %>% 
  # mutate(Winning = round(Winning, digits = 1),
  #        Drawing = round(Drawing, digits = 1),
  #        Losing = round(Losing, digits = 1)) %>% 
  mutate(equals = Winning + Drawing + Losing) 

ars <- c(43.95393, 22.45681, 	33.58925)
sum(ars)

largeRem2(ars)
pcVec <- ars


## ------------------------------------------------------------------------
ggplot(EPL_gamestate_df_clean,
       aes(x = team_name, xend = team_name)) + 
  geom_segment(aes(y = 0, yend = Winning, 
                   color = "Winning"), 
               size = 4) +
  geom_segment(aes(y = Winning, yend = Winning + Drawing, 
                   color = "Drawing"), 
               size = 4) +
  geom_segment(aes(y = Winning + Drawing, yend = Winning + Drawing + Losing, 
                   color = "Losing"), 
               size = 4) +
  geom_text(aes(y = win_half, label = Winning)) +
  scale_color_manual(values = result_vals, name = "Game State",
                    breaks = c("Winning", "Drawing", "Losing"),
                    labels = c("Winning", "Drawing", "Losing")) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Percentage of Time Spent:") +
  coord_flip() +
  theme_minimal() +
  theme(text = element_text(family = "Titillium Web"),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.y = element_line(color = "black"),
        axis.ticks.length.y = unit(0.25, "cm"),
        legend.position = "top")


## ------------------------------------------------------------------------
EPL_gamestate_data_df %>% 
  pivot_wider(names_from = "state", values_from = "percentage") %>% 
  mutate(team_name = fct_reorder(team_name, Winning)) %>% 
  filter(!is.na(Winning)) %>% 
  select(-Drawing, -Losing) %>% 
  arrange(desc(Winning)) %>% 
  select(team_name) %>% pull() %>% as.character() -> team_winning_order



## ------------------------------------------------------------------------
result_vals <- c("Winning" = "darkgreen", "Drawing" = "grey", "Losing" = "red")


EPL_gamestate_data_df %>% 
  mutate(team_name = fct_relevel(team_name, team_winning_order),
         team_name = fct_rev(team_name)) %>% 
  ggplot(aes(x = team_name, y = percentage, 
             fill = state)) + 
  geom_col() +
  scale_fill_manual(values = result_vals, 
                    breaks = c("Winning", "Drawing", "Losing"),
                    labels = c("Winning", "Drawing", "Losing")) +
  coord_flip() +
  theme_minimal()


## ------------------------------------------------------------------------
EPL_minutes_df <- EPL_shots_data_df_raw %>% 
  ## pick out 4th data frame which is minutes
  map(4) %>% 
  bind_rows(.id = "team_name")

EPL_minutes_df_clean <- EPL_minutes_df %>% 
  mutate(name = str_replace(name, "against.", "against_"),
         team_name = str_replace(team_name, "_", " "),
         team_name = as_factor(team_name)) %>% 
  separate(name, c("timing", "metric"), 
           sep = "\\.", extra = "merge") %>% 
  #select(-timing) %>% 
  filter(metric != "stat") %>% 
  pivot_wider(names_from = "metric", values_from = "value") %>% 
  mutate_at(vars(-team_name, -timing), as.numeric) %>% 
  mutate(xGperShot = xG / shots,
         xGAperShot = against_xG / against_shots)

glimpse(EPL_minutes_df_clean)


## ------------------------------------------------------------------------
EPL_minutes_df_clean %>% 
  mutate(shots = shots / 11,
         against_shots = against_shots / 11) %>% 
  ggplot(aes(x = timing)) +
  geom_point(aes(y = shots), color = "darkgreen") +
  geom_line(aes(y = shots, group = 1), color = "darkgreen") +
  geom_point(aes(y = against_shots), color = "red") +
  geom_line(aes(y = against_shots, group = 1), color = "red") +
  labs(x = "Minutes", y = "Shots per Game") +
  theme_minimal() +
  facet_wrap(~ team_name)


## ------------------------------------------------------------------------
EPL_minutes_df_clean %>% 
  ggplot(aes(x = timing)) +
  geom_point(aes(y = xGperShot), color = "darkgreen") +
  geom_line(aes(y = xGperShot, group = 1), color = "darkgreen") +
  geom_point(aes(y = xGAperShot), color = "red") +
  geom_line(aes(y = xGAperShot, group = 1), color = "red") +
  labs(x = "Minutes", y = "Expected Goals (per shot)") +
  theme_minimal() +
  facet_wrap(~team_name)


## ------------------------------------------------------------------------
EPL_minutes_df_clean %>% 
  mutate(goalsperGame = goals / 11,
         against_goalsperGame = against_goals / 11) %>% 
  ggplot(aes(x = timing)) +
  # actual
  geom_point(aes(y = goalsperGame), color = "darkgreen") +
  geom_line(aes(y = goalsperGame, group = 1), color = "darkgreen") +
  geom_point(aes(y = against_goalsperGame), color = "darkred") +
  geom_line(aes(y = against_goalsperGame, group = 1), color = "darkred") +
  ## xG
  geom_point(aes(y = xGperShot), color = "green") +
  geom_line(aes(y = xGperShot, group = 1), color = "green") +
  geom_point(aes(y = xGAperShot), color = "red") +
  geom_line(aes(y = xGAperShot, group = 1), color = "red") +
  labs(x = "Minutes", y = "Goals per Game | xG per Shot") +
  theme_minimal() +
  facet_wrap(~team_name)


## ------------------------------------------------------------------------
EPL_minutes_df_clean %>% 
  mutate(goalsperGame = goals / 11,
         against_goalsperGame = against_goals / 11) %>% 
  ggplot(aes(x = timing)) +
  # actual
  geom_point(aes(y = goalsperGame), color = "darkgreen") +
  geom_line(aes(y = goalsperGame, group = 1), color = "darkgreen") +
  ## xG
  geom_point(aes(y = xGperShot), color = "green") +
  geom_line(aes(y = xGperShot, group = 1), color = "green") +
  labs(x = "Minutes", y = "Goals per Game | xG per Shot") +
  theme_minimal() +
  facet_wrap(~team_name)


## ------------------------------------------------------------------------
EPL_minutes_df_clean %>% 
    mutate(goalsperGame = goals / 11,
         against_goalsperGame = against_goals / 11) %>% 
  ggplot(aes(x = timing)) +
  # actual
  geom_point(aes(y = against_goalsperGame), color = "darkred") +
  geom_line(aes(y = against_goalsperGame, group = 1), color = "darkred") +
  ## xG
  geom_point(aes(y = xGAperShot), color = "red") +
  geom_line(aes(y = xGAperShot, group = 1), color = "red") +
  labs(x = "Minutes", y = "Goals per Game | xG per Shot") +
  theme_minimal() +
  facet_wrap(~team_name)


## ------------------------------------------------------------------------
EPL_minutes_df_clean %>% 
  mutate(xGD = xG - against_xG,
         GD = goals - against_goals) %>% 
  ggplot(aes(x = timing)) +
  # actual
  geom_point(aes(y = GD), color = "black") +
  geom_line(aes(y = GD, group = 1), color = "black") +
  ## xG
  geom_point(aes(y = xGD), color = "gray") +
  geom_line(aes(y = xGD, group = 1), color = "gray") +
  geom_hline(yintercept = 0, color = "red") +
  labs(x = "Minutes", y = "Goal Difference | xGD") +
  theme_minimal() +
  facet_wrap(~team_name)


## ------------------------------------------------------------------------
url <- "https://understat.com/team/Liverpool/2019"

team_page <- polite::bow(url)

team_data <- polite::scrape(team_page) %>% 
  get_script()

team_situation_data <- get_data_element(team_data, "statisticsData") %>% 
  str_replace(., "\\{\"situation\"",  "[\\{\"situation\"") %>% 
  str_replace(., "\\}'\\)", "\\}]'\\)") %>% 
  fix_json() %>% 
  fromJSON()


## ------------------------------------------------------------------------
situation_df <- team_situation_data %>% 
  .[[1]] %>% 
  unlist() %>% 
  enframe()

situation_df_clean <- situation_df %>% 
  mutate(name = str_replace(name, "against.", "against_")) %>% 
  separate(name, c("situation", "metric"), 
           sep = "\\.", extra = "merge")


## ------------------------------------------------------------------------
situation_df_clean %>% 
  ggplot(aes(x = situation, y = value)) +
  geom_segment(aes(xend = situation,
                   y = 0, yend = value)) +
  geom_point(color = "red", size = 2.5) + 
  theme_minimal() +
  facet_wrap("metric", scales = "free_y")


## ------------------------------------------------------------------------
formation_df <- team_situation_data %>% 
  .[[2]] %>% 
  unlist() %>% 
  enframe()


## ------------------------------------------------------------------------
gamestate_df <- team_situation_data %>% 
  .[3] %>% 
  unlist() %>% 
  enframe()

gamestate_df_clean <- gamestate_df %>% 
  mutate(name = str_replace(name, "gameState.Goal diff ", ""),
         name = str_replace(name, "against.", "against_")) %>% 
  separate(name, c("game_state", "metric"), 
           sep = "\\.", extra = "merge") %>% 
  filter(metric != "stat")

glimpse(gamestate_df_clean)


## ------------------------------------------------------------------------
game_state_clean <- gamestate_df_clean %>% 
  filter(metric == "time") %>% 
  mutate(state = case_when(
    game_state == "0" ~ "Drawing",
    game_state %in% c("> +1", "+1") ~ "Winning",
    game_state %in% c("-1", ">-1") ~ "Losing",
    TRUE ~ NA_character_
  )) %>% 
  select(-game_state, -metric) %>% 
  mutate(value = as.numeric(value)) %>% 
  group_by(state) %>% 
  summarize(value = sum(value)) %>% 
  ungroup() %>% 
  mutate(team_name = "Liverpool",
         total = sum(value),
         percentage = value / total)

glimpse(game_state_clean)


## ------------------------------------------------------------------------
ggplot(game_state_clean,
       aes(x = team_name, y = percentage, fill = as.factor(state))) + 
  geom_col()


## ------------------------------------------------------------------------
minutes_df <- team_situation_data %>% 
  .[4] %>% 
  unlist() %>% 
  enframe()

minutes_df_clean <- minutes_df %>% 
  mutate(name = str_replace(name, "against.", "against-")) %>% 
  separate(name, c("timing", "minutes", "metric"), 
           sep = "\\.", extra = "merge") %>% 
  select(-timing) %>% 
  filter(metric != "stat") %>% 
  pivot_wider(names_from = "metric", values_from = "value") %>% 
  mutate_at(vars(-minutes), as.numeric) %>% 
  mutate(xGperShot = xG / shots,
         xGAperShot = `against-xG` / `against-shots`)

glimpse(minutes_df_clean)


## ------------------------------------------------------------------------
zone_df <- team_situation_data %>% 
  .[[5]] %>% 
  unlist() %>% 
  enframe()

zone_df_clean <- zone_df %>% 
  mutate(name = str_replace(name, "against.", "against-")) %>% 
  separate(name, c("zone", "metric"), 
           sep = "\\.", extra = "merge") %>% 
  filter(metric != "stat") %>% 
  mutate(value = as.numeric(value))


## ------------------------------------------------------------------------
zone_df_clean %>% 
  ggplot(aes(x = zone, y = value)) +
  geom_segment(aes(xend = zone,
                   y = 0, yend = value)) +
  geom_point(color = "red", size = 2.5) + 
  theme_minimal() +
  facet_wrap("metric", scales = "free_y")


## ------------------------------------------------------------------------
shot_df <- team_situation_data %>% 
  .[[7]] %>% 
  unlist() %>% 
  enframe() 

shot_df_clean <- shot_df %>% 
  mutate(name = str_replace(name, "against.", "against-")) %>% 
  separate(name, c("result", "metric"), 
           sep = "\\.", extra = "merge") %>% 
  filter(metric != "stat") %>% 
  mutate(value = as.numeric(value))


## ------------------------------------------------------------------------
shot_df_clean %>% 
  ggplot(aes(x = result, y = value)) +
  geom_segment(aes(xend = result,
                   y = 0, yend = value)) +
  geom_point(color = "red", size = 2.5) + 
  theme_minimal() +
  facet_wrap("metric", scales = "free_y")


## ------------------------------------------------------------------------
minutes_df_clean %>% 
  pivot_longer(-minutes, names_to = "metric", values_to = "value") %>% 
  ggplot(aes(x = minutes, y = value)) + 
  geom_point(color = "red", size = 2.5) + 
  geom_line(aes(group = metric)) +
  theme_minimal() +
  facet_wrap("metric", scales = "free_y")


## ------------------------------------------------------------------------
minutes_df_clean %>% 
  filter(metric == "shots" |
         metric == "against-shots") %>% 
  ggplot() + 
  geom_point(data = minutes_df_clean %>% 
               filter(metric == "shots"),
             aes(x = minutes, y = value),
             color = "blue", size = 2.5) + 
  geom_point(data = minutes_df_clean %>% 
               filter(metric == "against-shots"),
             aes(x = minutes, y = value),
             color = "red", size = 2.5) + 
  geom_line(aes(x = minutes, y = value, group = metric)) +
  labs(title = "Shots/Shots Against by Time") +
  theme_minimal()


## ------------------------------------------------------------------------
minutes_df_clean %>% 
  filter(metric == "xG" |
         metric == "against-xG") %>% 
  ggplot() + 
  geom_point(data = minutes_df_clean %>% 
               filter(metric == "xG"),
             aes(x = minutes, y = value),
             color = "blue", size = 2.5) + 
  geom_point(data = minutes_df_clean %>% 
               filter(metric == "against-xG"),
             aes(x = minutes, y = value),
             color = "red", size = 2.5) + 
  geom_line(aes(x = minutes, y = value, group = metric)) +
  labs(title = "xG/xGA by Time") +
  theme_minimal()


## ------------------------------------------------------------------------
minutes_df_clean %>% 
  mutate(shots = shots / 11,
         `against-shots` = `against-shots` / 11) %>% 
  ggplot(aes(x = minutes)) +
  geom_point(aes(y = shots), color = "darkgreen") +
  geom_line(aes(y = shots, group = 1), color = "darkgreen") +
  geom_point(aes(y = `against-shots`), color = "red") +
  geom_line(aes(y = `against-shots`, group = 1), color = "red") +
  labs(x = "Minutes", y = "Shots per Game") +
  theme_minimal()


## ------------------------------------------------------------------------
minutes_df_clean %>% 
  ggplot(aes(x = minutes)) +
  geom_point(aes(y = xGperShot), color = "darkgreen") +
  geom_line(aes(y = xGperShot, group = 1), color = "darkgreen") +
  geom_point(aes(y = xGAperShot), color = "red") +
  geom_line(aes(y = xGAperShot, group = 1), color = "red") +
  labs(x = "Minutes", y = "Expected Goals (per shot)") +
  theme_minimal()


## ------------------------------------------------------------------------
minutes_df_clean %>% 
  mutate(goals = goals / 11,
         `against-goals` = `against-goals` / 11) %>% 
  ggplot(aes(x = minutes)) +
  # actual
  geom_point(aes(y = goals), color = "darkgreen") +
  geom_line(aes(y = goals, group = 1), color = "darkgreen") +
  geom_point(aes(y = `against-goals`), color = "darkred") +
  geom_line(aes(y = `against-goals`, group = 1), color = "darkred") +
  ## xG
  geom_point(aes(y = xGperShot), color = "green") +
  geom_line(aes(y = xGperShot, group = 1), color = "green") +
  geom_point(aes(y = xGAperShot), color = "red") +
  geom_line(aes(y = xGAperShot, group = 1), color = "red") +
  labs(x = "Minutes", y = "Goals per Game | xG per Game") +
  theme_minimal()

