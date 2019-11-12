## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)


## ------------------------------------------------------------------------
pacman::p_load(dplyr, tidyr, stringr, stringi, purrr,
               tibble, rvest, polite, lubridate,
               ggplot2, jsonlite, xml2, qdapRegex,
               ggtext, extrafont, ggrepel, ggforce,
               understatr, ggsoccer,
               grid, gridExtra)
loadfonts(quiet = TRUE)


## ------------------------------------------------------------------------
bobby_shots <- get_player_shots(482)


## ---- fig.height=8, fig.width=10-----------------------------------------
pitch_custom <- list(
  length = 1084,
  width = 878,
  penalty_box_length = 224,
  penalty_box_width = 493,
  six_yard_box_length = 73,
  six_yard_box_width = 247,
  penalty_spot_distance = 150,
  goal_width = 99,
  origin_x = 0,
  origin_y = 0
)

df <- data.frame(x = 1042.604, y = 534.604)

ggplot(df) +
  annotate_pitch(dimensions = pitch_custom) +
  theme_pitch(aspect_ratio = 878/1084) +
  coord_flip(xlim = c(550, 1085),
              ylim = c(-1, 880)) +
  geom_point(aes(x = x, y = y), shape = 21,
             fill = "red",
             colour = "red", size = 5) +
  geom_point(x = 1080, y = 850)


ggplot(df) +
  annotate_pitch(dimensions = pitch_custom) +
  theme_pitch(aspect_ratio = 878/1284) +
  geom_point(x = 0, y = 0) +
  geom_point(x = 600, y = 878)


## ------------------------------------------------------------------------
shots <- data.frame(x = c(90, 85, 82, 78, 83, 74, 94, 91),
                    y = c(43, 40, 52, 56, 44, 71, 60, 54))

ggplot(shots) +
  annotate_pitch(colour = "white",
                 fill   = "chartreuse4",
                 limits = FALSE) +
  geom_point(aes(x = x, y = 100 - y),
             colour = "yellow", 
             size = 4) +
  theme_pitch() +
  theme(plot.background = element_rect(fill = "chartreuse4"),
        title = element_text(colour = "white")) +
  coord_flip(xlim = c(49, 101),
             ylim = c(-1, 101)) +
  ggtitle("Simple shotmap",
          "ggsoccer example")


## ------------------------------------------------------------------------
player_url <- str_glue("{home_url}/player/{player_id}")

# read player page
player_page <- read_html(player_url)

# locate script tags
player_data <- get_script(player_page)

# isolate player data
player_data <- get_data_element(player_data, "shotsData")

# pick out JSON string
player_data <- fix_json(player_data)

# parse JSON
player_data <- fromJSON(player_data)


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
home_url <- "https://understat.com"
player_id <- 482


## ------------------------------------------------------------------------
match_url <- stringr::str_glue("{home_url}/match/{match_id}")

match_page <- polite::bow(match_url)

team_stats <- polite::scrape(match_page) %>% 
  html_nodes("div.scheme-block:nth-child(4)") %>% 
  html_text() %>% 
  str_remove_all(., "CHANCES") %>% 
  str_remove_all(., "([0-9]{2,}%)") %>% 
  str_replace_all(., "SHOTS ON TARGET", "ON-TARGET") %>% #trimws() %>% 
  str_squish() %>% 
  read.table(text = ., header = FALSE, sep = " ",
             col.names = c("var_name", "home", "away")) %>% 
  t() %>% 
  tibble::as_tibble() %>% 
  janitor::row_to_names(row_number = 1) %>% 
  mutate_at(vars(-TEAMS), ~ as.numeric(.))

home_stats <- team_stats[1,]

away_stats <- team_stats[2,]

team_stats


## ------------------------------------------------------------------------
home_url <- "https://understat.com"
match_id <- 11643


## ------------------------------------------------------------------------
match_url <- stringr::str_glue("{home_url}/match/{match_id}")

match_page <- polite::bow(match_url)

match_data <- polite::scrape(match_page) %>% 
  get_script()

match_shots_data <- get_data_element(match_data, "shotsData")

#msd <- jsonlite::stream_in(match_shots_data)

match_shots_data <- fix_json(match_shots_data)

# Home: Liverpool
liv_shots_data <- fromJSON(match_shots_data[1])

## add 'team_name' with home team name from 'h_team' var
liv_shots_data$team_name <- liv_shots_data$h_team


# Away: Arsenal
nor_shots_data <- fromJSON(match_shots_data[2])

## add 'team_name' with away team name from 'a_team' var
nor_shots_data$team_name <- nor_shots_data$a_team


## ------------------------------------------------------------------------
match_shots_data_clean <- liv_shots_data %>% 
  full_join(nor_shots_data) %>% 
  select(-id, -h_team, -a_team,
         -h_goals, -a_goals) %>% 
  mutate_at(vars(minute, xG, X, Y, 
                 player_id, match_id, season), ~ as.numeric(.)) %>% 
  mutate(team_name = forcats::as_factor(team_name),
         xG = if_else(is.na(xG), 0, xG) %>% round(digits = 2),
         result = case_when(
           result == "SavedShot" ~ "Saved Shot",
           result == "BlockedShot" ~ "Blocked Shot",
           result == "MissedShots" ~ "Missed Shot",
           result == "ShotOnPost" ~ "On Post",
           result == "OwnGoal" ~ "Own Goal",
           TRUE ~ result),
         situation = case_when(
           situation == "OpenPlay" ~ "Open Play", 
           situation == "FromCorner" ~ "From Corner",
           situation == "DirectFreekick" ~ "From Free Kick",
           TRUE ~ situation),
         lastAction = case_when(
           lastAction == "BallRecovery" ~ "Ball Recovery",
           lastAction == "BallTouch" ~ "Ball Touch",
           lastAction == "LayOff" ~ "Lay Off",
           lastAction == "TakeOn" ~ "Take On",
           TRUE ~ lastAction),
         shotType = case_when(
           shotType == "LeftFoot" ~ "Left Foot",
           shotType == "RightFoot" ~ "Right Foot",
           TRUE ~ shotType)) %>% 
  separate(player, into = c("firstname", "player"), 
           sep = "\\s", extra = "merge") %>% 
  ## players like Fabinho listed without Tavares last name
  mutate(player = if_else(is.na(player), firstname, player))

last_min <- match_shots_data_clean$minute %>% unique() %>% last()
minute <- c(0:last_min)
team_name <- c("Liverpool", "Norwich")

livnor_rollsumxG <- match_shots_data_clean %>% 
  full_join(crossing(minute, team_name)) %>% 
  arrange(minute) %>% 
  group_by(team_name) %>% 
  mutate(xG = if_else(is.na(xG), 0, xG) %>% round(digits = 2),
         rollsum = lag(cumsum(xG))) %>% 
  ungroup() %>% 
  mutate(player_label = case_when(
    result == "Goal" ~ glue::glue("{player}: {xG %>% round(digits = 2)} xG"),
    result == "Own Goal" ~ glue::glue("{player} (Own Goal): {xG %>% round(digits = 2)} xG"),
    TRUE ~ ""),
    ## 
    rollsum_goal = rollsum + xG,
    minute_goal = minute + 1)


## ---- fig.height=6, fig.width=10-----------------------------------------
cumsum_xG_plot <- livnor_rollsumxG %>% 
  ggplot(aes(x = minute_goal, y = rollsum_goal, 
             color = team_name, group = team_name)) +
  geom_line(size = 2.5) +
  geom_label_repel(data = livnor_rollsumxG %>% 
                     filter(result %in% c("Goal", "Own Goal")),
                   aes(x = minute_goal, y = rollsum_goal, 
                       color = team_name, label = player_label),
                   nudge_x = -10, nudge_y = 0.35,
                   show.legend = FALSE) +
  geom_point(data = livnor_rollsumxG %>% 
               filter(result %in% c("Goal", "Own Goal")),
             aes(x = minute_goal, y = rollsum_goal, 
                       color = team_name),
             size = 5, shape = 21, fill = "white", stroke = 1.25,
             show.legend = FALSE) +
  scale_x_continuous(breaks = c(seq(0, 90, by = 5), 91),
                     labels = c(seq(0, 40, by = 5), "HT", 
                                seq(50, 85, by = 5),"", "FT"),
                     expand = c(0.01, 0),
                     limits = c(0, 91)) +
  scale_y_continuous(limits = c(0, 3),
                     sec.axis = sec_axis(~ ., breaks = team_stats$xG)) +
  scale_color_manual(
    values = c("Liverpool" = "#d00027",
               "Norwich" = "#00a650"),
    breaks = c("Liverpool", "Norwich"),
    labels = c("<b style ='color:#d00027'>Liverpool</b>",
               "<b style='color: #00a650'>Norwich</b>")) +
  labs(title = glue::glue("<b style ='color:#d00027'>{home_stats$TEAMS}: {home_stats$GOALS} </b><b style ='color:#d00027; font-size: 20'>({home_stats$xPTS} xPTs)</b><br> <b style='color: #00a650'>{away_stats$TEAMS}: {away_stats$GOALS} </b><b style='color: #00a650; font-size: 20'>({away_stats$xPTS} xPTs)</b>"),
       subtitle = "August 9, 2019 (Matchday 1)",
       x = NULL,
       y = "Expected Goals") +
  theme_minimal() +
  theme(text = element_text(family = "Roboto Condensed"),
        plot.title = element_markdown(size = 40, 
                                      family = "Roboto Condensed"),
        plot.subtitle = element_text(size = 18, 
                                     family = "Roboto Condensed",
                                     color = "grey20"),
        axis.title = element_text(size = 18, color = "grey20"),
        axis.text = element_text(size = 16, face = "bold"),
        panel.grid.minor.x = element_blank(),
        legend.text = element_markdown(size = 14),
        legend.position = c(0.2, 0.95),
        legend.direction = "horizontal",
        legend.title = element_blank())

cumsum_xG_plot


## ------------------------------------------------------------------------
home_url <- "https://understat.com"
match_id <- 11658


## ------------------------------------------------------------------------
match_url <- stringr::str_glue("{home_url}/match/{match_id}")

match_page <- polite::bow(match_url)

match_data <- polite::scrape(match_page) %>% 
  get_script()

match_shots_data <- get_data_element(match_data, "shotsData")

#msd <- jsonlite::stream_in(match_shots_data)

match_shots_data <- fix_json(match_shots_data)

# Home: Southampton
sot_shots_data <- fromJSON(match_shots_data[1])

## add 'team_name' with home team name from 'h_team' var
sot_shots_data$team_name <- sot_shots_data$h_team

# Away: Liverpool
liv_shots_data <- fromJSON(match_shots_data[2])

## add 'team_name' with away team name from 'a_team' var
liv_shots_data$team_name <- liv_shots_data$a_team


## ------------------------------------------------------------------------
match_shots_data_clean <- liv_shots_data %>% 
  full_join(sot_shots_data) %>% 
  select(-id, -h_team, -a_team,
         -h_goals, -a_goals) %>% 
  mutate_at(vars(minute, xG, X, Y, 
                 player_id, match_id, season), ~ as.numeric(.)) %>% 
  mutate(team_name = forcats::as_factor(team_name),
         xG = if_else(is.na(xG), 0, xG) %>% round(digits = 2),
         result = case_when(
           result == "SavedShot" ~ "Saved Shot",
           result == "BlockedShot" ~ "Blocked Shot",
           result == "MissedShots" ~ "Missed Shot",
           result == "ShotOnPost" ~ "On Post",
           result == "OwnGoal" ~ "Own Goal",
           TRUE ~ result),
         situation = case_when(
           situation == "OpenPlay" ~ "Open Play", 
           situation == "FromCorner" ~ "From Corner",
           situation == "DirectFreekick" ~ "From Free Kick",
           TRUE ~ situation),
         lastAction = case_when(
           lastAction == "BallRecovery" ~ "Ball Recovery",
           lastAction == "BallTouch" ~ "Ball Touch",
           lastAction == "LayOff" ~ "Lay Off",
           lastAction == "TakeOn" ~ "Take On",
           TRUE ~ lastAction),
         shotType = case_when(
           shotType == "LeftFoot" ~ "Left Foot",
           shotType == "RightFoot" ~ "Right Foot",
           TRUE ~ shotType)) %>% 
  separate(player, into = c("firstname", "player"), 
           sep = "\\s", extra = "merge") %>% 
  ## players like Fabinho listed without Tavares last name
  mutate(player = if_else(is.na(player), firstname, player))

last_min <- match_shots_data_clean$minute %>% unique() %>% last()
minute <- c(0:last_min)
team_name <- c("Southampton", "Liverpool")

livsot_rollsumxG <- match_shots_data_clean %>% 
  full_join(crossing(minute, team_name)) %>% 
  arrange(minute) %>% 
  group_by(team_name) %>% 
  mutate(xG = if_else(is.na(xG), 0, xG) %>% round(digits = 2),
         rollsum = lag(cumsum(xG))) %>% 
  ungroup() %>% 
  mutate(player_label = case_when(
    result == "Goal" ~ glue::glue("{player}: {xG %>% round(digits = 2)} xG"),
    TRUE ~ ""),
    ## 
    rollsum_goal = rollsum + xG,
    minute_goal = minute + 1)


## ---- fig.height=6, fig.width=10-----------------------------------------
cumsum_xG_plot <- livsot_rollsumxG %>% 
  ggplot(aes(x = minute_goal, y = rollsum_goal, 
             color = team_name, group = team_name)) +
  geom_line(size = 2.5) +
  geom_label_repel(data = livsot_rollsumxG %>% filter(result == "Goal"),
                   aes(x = minute_goal, y = rollsum_goal, 
                       color = team_name, label = player_label),
                   nudge_x = -10, nudge_y = 0.35,
                   show.legend = FALSE) +
  geom_point(data = livsot_rollsumxG %>% filter(result == "Goal"),
             aes(x = minute_goal, y = rollsum_goal, 
                       color = team_name),
             size = 5, shape = 21, fill = "white", stroke = 1.25,
             show.legend = FALSE) +
  scale_x_continuous(breaks = c(seq(0, 90, by = 5), 91),
                     labels = c(seq(0, 40, by = 5), "HT", 
                                seq(50, 85, by = 5),"", "FT"),
                     expand = c(0.01, 0),
                     limits = c(0, 91)) +
  scale_y_continuous(limits = c(0, 3),
                     sec.axis = sec_axis(~ ., breaks = team_stats$xG)) +
  scale_color_manual(
    values = c("Liverpool" = "#000000",
               "Southampton" = "#ed1a3b"),
    breaks = c("Liverpool", "Southampton"),
    labels = c("<b style ='color:#000000'>Liverpool</b>",
               "<b style='color: #ed1a3b'>Southampton</b>")) +
  labs(title = glue::glue("<b style ='color:#ed1a3b'>{home_stats$TEAMS}: {home_stats$GOALS} </b><b style ='color:#ed1a3b; font-size: 20'>({home_stats$xPTS} xPTs)</b><br> <b style='color: #000000'>{away_stats$TEAMS}: {away_stats$GOALS} </b><b style='color: #000000; font-size: 20'>({away_stats$xPTS} xPTs)</b>"),
       subtitle = "August 17, 2019 (Matchday 2)",
       x = NULL,
       y = "Expected Goals") +
  theme_minimal() +
  theme(text = element_text(family = "Roboto Condensed"),
        plot.title = element_markdown(size = 40, 
                                      family = "Roboto Condensed"),
        plot.subtitle = element_text(size = 18, 
                                     family = "Roboto Condensed",
                                     color = "grey20"),
        axis.title = element_text(size = 18, color = "grey20"),
        axis.text = element_text(size = 16, face = "bold"),
        panel.grid.minor.x = element_blank(),
        legend.text = element_markdown(size = 14),
        legend.position = c(0.2, 0.95),
        legend.direction = "horizontal",
        legend.title = element_blank())

cumsum_xG_plot


## ------------------------------------------------------------------------
match_id <- 11670
match_url <- stringr::str_glue("{home_url}/match/{match_id}")

match_page <- polite::bow(match_url)

match_data <- polite::scrape(match_page) %>% 
  get_script()

match_shots_data <- get_data_element(match_data, "shotsData")

#msd <- jsonlite::stream_in(match_shots_data)

match_shots_data <- fix_json(match_shots_data)

# Home: Liverpool
liv_shots_data <- fromJSON(match_shots_data[1])

## add 'team_name' with home team name from 'h_team' var
liv_shots_data$team_name <- liv_shots_data$h_team


# Away: Arsenal
ars_shots_data <- fromJSON(match_shots_data[2])

## add 'team_name' with away team name from 'a_team' var
ars_shots_data$team_name <- ars_shots_data$a_team


## ------------------------------------------------------------------------
match_shots_data_clean <- liv_shots_data %>% 
  full_join(ars_shots_data) %>% 
  select(-id, -h_team, -a_team,
         -h_goals, -a_goals) %>% 
  mutate_at(vars(minute, xG, X, Y, 
                 player_id, match_id, season), ~ as.numeric(.)) %>% 
  mutate(team_name = forcats::as_factor(team_name),
         xG = if_else(is.na(xG), 0, xG) %>% round(digits = 2),
         result = case_when(
           result == "SavedShot" ~ "Saved Shot",
           result == "BlockedShot" ~ "Blocked Shot",
           result == "MissedShots" ~ "Missed Shot",
           result == "ShotOnPost" ~ "On Post",
           result == "OwnGoal" ~ "Own Goal",
           TRUE ~ result),
         situation = case_when(
           situation == "OpenPlay" ~ "Open Play", 
           situation == "FromCorner" ~ "From Corner",
           situation == "DirectFreekick" ~ "From Free Kick",
           TRUE ~ situation),
         lastAction = case_when(
           lastAction == "BallRecovery" ~ "Ball Recovery",
           lastAction == "BallTouch" ~ "Ball Touch",
           lastAction == "LayOff" ~ "Lay Off",
           lastAction == "TakeOn" ~ "Take On",
           TRUE ~ lastAction),
         shotType = case_when(
           shotType == "LeftFoot" ~ "Left Foot",
           shotType == "RightFoot" ~ "Right Foot",
           TRUE ~ shotType)) %>% 
  separate(player, into = c("firstname", "player"), 
           sep = "\\s", extra = "merge") %>% 
  ## players like Fabinho listed without Tavares last name
  mutate(player = if_else(is.na(player), firstname, player))

last_min <- match_shots_data_clean$minute %>% unique() %>% last()
minute <- c(0:last_min)
team_name <- c("Liverpool", "Arsenal")
# crossing(minute, team_name)

livars_rollsumxG <- match_shots_data_clean %>% 
  full_join(crossing(minute, team_name)) %>% 
  arrange(minute) %>% 
  group_by(team_name) %>% 
  mutate(xG = if_else(is.na(xG), 0, xG) %>% round(digits = 2),
         rollsum = lag(cumsum(xG))) %>% 
  ungroup() %>% 
  mutate(player_label = case_when(
    result == "Goal" ~ glue::glue("{player}: {xG %>% round(digits = 2)} xG"),
    TRUE ~ ""),
    ## 
    rollsum_goal = rollsum + xG,
    minute_goal = minute + 1)


## ---- fig.height=6, fig.width=10-----------------------------------------
cumsum_xG_plot <- livars_rollsumxG %>% 
  ggplot(aes(x = minute_goal, y = rollsum_goal, 
             color = team_name, group = team_name)) +
  geom_line(size = 2.5) +
  geom_label_repel(data = livars_rollsumxG %>% filter(result == "Goal"),
                   aes(x = minute_goal, y = rollsum_goal, 
                       color = team_name, label = player_label),
                   nudge_x = -10, nudge_y = 0.35,
                   show.legend = FALSE) +
  geom_point(data = livars_rollsumxG %>% filter(result == "Goal"),
             aes(x = minute_goal, y = rollsum_goal, 
                       color = team_name),
             size = 5, shape = 21, fill = "white", stroke = 1.25,
             show.legend = FALSE) +
  scale_x_continuous(breaks = c(seq(0, 90, by = 5), 94),
                     labels = c(seq(0, 40, by = 5), "HT", 
                                seq(50, 90, by = 5), "FT"),
                     expand = c(0.01, 0),
                     limits = c(0, 94)) +
  scale_y_continuous(limits = c(0, 3),
                     sec.axis = sec_axis(~ ., breaks = team_stats$xG)) +
  scale_color_manual(
    values = c("Liverpool" = "#d00027",
               "Arsenal" = "#063672"),
    breaks = c("Liverpool", "Arsenal"),
    labels = c("<b style ='color:#d00027'>Liverpool</b>",
               "<b style='color: #063672'>Arsenal</b>")) +
  labs(title = glue::glue(
    "<b style ='color:{home_stats$home_team_color}'>{home_stats$TEAMS}: {home_stats$GOALS} </b><b style ='color:{home_stats$home_team_color}; font-size: 20'>({home_stats$xPTS} xPTs)</b><br> <b style='color:{away_stats$away_team_color}'>{away_stats$TEAMS}: {away_stats$GOALS} </b><b style='color:{away_stats$away_team_color}; font-size: 20'>({away_stats$xPTS} xPTs)</b>"),
       subtitle = glue::glue("{match_date} (Matchday {matchday_num})"),
       x = NULL,
       y = "Expected Goals") +
  theme_minimal() +
  theme(text = element_text(family = "Roboto Condensed"),
        plot.title = element_markdown(size = 40, 
                                      family = "Roboto Condensed"),
        plot.subtitle = element_text(size = 18, 
                                     family = "Roboto Condensed",
                                     color = "grey20"),
        axis.title = element_text(size = 18, color = "grey20"),
        axis.text = element_text(size = 16, face = "bold"),
        panel.grid.minor.x = element_blank(),
        legend.text = element_markdown(size = 14),
        legend.position = c(0.2, 0.95),
        legend.direction = "horizontal",
        legend.title = element_blank())

cumsum_xG_plot


## ------------------------------------------------------------------------
match_id <- 11680

match_url <- stringr::str_glue("{home_url}/match/{match_id}")

match_page <- polite::bow(match_url)

match_data <- polite::scrape(match_page) %>% 
  get_script()

match_shots_data <- get_data_element(match_data, "shotsData")

#msd <- jsonlite::stream_in(match_shots_data)

match_shots_data <- fix_json(match_shots_data)

# Home: Burnley
burn_shots_data <- fromJSON(match_shots_data[1])

## add 'team_name' with home team name from 'h_team' var
burn_shots_data$team_name <- burn_shots_data$h_team


# Away: LFC
liv_shots_data <- fromJSON(match_shots_data[2])

## add 'team_name' with away team name from 'a_team' var
liv_shots_data$team_name <- liv_shots_data$a_team


## ------------------------------------------------------------------------
match_shots_data_clean <- liv_shots_data %>% 
  full_join(burn_shots_data) %>% 
  select(-id, -h_team, -a_team,
         -h_goals, -a_goals) %>% 
  mutate_at(vars(minute, xG, X, Y, 
                 player_id, match_id, season), ~ as.numeric(.)) %>% 
  mutate(team_name = forcats::as_factor(team_name),
         xG = if_else(is.na(xG), 0, xG) %>% round(digits = 2),
         result = case_when(
           result == "SavedShot" ~ "Saved Shot",
           result == "BlockedShot" ~ "Blocked Shot",
           result == "MissedShots" ~ "Missed Shot",
           result == "ShotOnPost" ~ "On Post",
           result == "OwnGoal" ~ "Own Goal",
           TRUE ~ result),
         situation = case_when(
           situation == "OpenPlay" ~ "Open Play", 
           situation == "FromCorner" ~ "From Corner",
           situation == "DirectFreekick" ~ "From Free Kick",
           TRUE ~ situation),
         lastAction = case_when(
           lastAction == "BallRecovery" ~ "Ball Recovery",
           lastAction == "BallTouch" ~ "Ball Touch",
           lastAction == "LayOff" ~ "Lay Off",
           lastAction == "TakeOn" ~ "Take On",
           TRUE ~ lastAction),
         shotType = case_when(
           shotType == "LeftFoot" ~ "Left Foot",
           shotType == "RightFoot" ~ "Right Foot",
           TRUE ~ shotType)) %>% 
  arrange(minute) %>% 
  separate(player, into = c("firstname", "player"), 
           sep = "\\s", extra = "merge") %>% 
  ## players like Fabinho listed without Tavares last name
  mutate(player = if_else(is.na(player), firstname, player))

last_min <- match_shots_data_clean$minute %>% unique() %>% last()
if (last_min < 90) {
  last_min <- 90
}

minute <- c(0:last_min)
team_name <- c("Liverpool", "Burnley")
# crossing(minute, team_name)

livars_rollsumxG <- match_shots_data_clean %>% 
  full_join(crossing(minute, team_name)) %>% 
  arrange(minute) %>% 
  group_by(team_name) %>% 
  mutate(xG = if_else(is.na(xG), 0, xG) %>% round(digits = 2),
         rollsum = lag(cumsum(xG))) %>% 
  ungroup() %>% 
  mutate(player_label = case_when(
    result == "Goal" ~ glue::glue("{player}: {xG %>% round(digits = 2)} xG"),
    result == "Own Goal" ~ glue::glue("{player} (Own Goal): {xG %>% round(digits = 2)} xG"),
    TRUE ~ ""),
    # xG = case_when(
    #        result == "OwnGoal" ~ rollsum,
    #        TRUE ~ xG
    #      ),
    ## 
    rollsum_goal = rollsum + xG,
    minute_goal = minute + 1)


## ---- fig.height=6, fig.width=10-----------------------------------------
cumsum_xG_plot <- livars_rollsumxG %>% 
  ggplot(aes(x = minute_goal, y = rollsum_goal, 
             color = team_name, group = team_name)) +
  geom_line(size = 2.5) +
  geom_label_repel(data = livars_rollsumxG %>% filter(result == "Goal"),
                   aes(x = minute_goal, y = rollsum_goal, 
                       color = team_name, label = player_label),
                   nudge_x = -10, nudge_y = 0.35,
                   size = 5, show.legend = FALSE) +
  geom_label_repel(data = livars_rollsumxG %>% filter(result == "Own Goal"),
                   aes(x = minute_goal, y = rollsum_goal, 
                       color = team_name, label = player_label),
                   nudge_x = -10, nudge_y = 0.35,
                   size = 5, show.legend = FALSE) +
  geom_point(data = livars_rollsumxG %>% 
               filter(result %in% c("Goal", "Own Goal")),
             aes(x = minute_goal, y = rollsum_goal, 
                       color = team_name),
             size = 5, shape = 21, fill = "white", stroke = 1.25,
             show.legend = FALSE) +
  ## if extratime or last min is < 93/94 then just skip 90 min label
  scale_x_continuous(breaks = c(seq(0, 90, by = 5), 92),
                     labels = c(seq(0, 40, by = 5), "HT", 
                                seq(50, 85, by = 5), "", "FT"),
                     expand = c(0.01, 0),
                     limits = c(0, 92)) +
  scale_y_continuous(limits = c(0, 1.75),
                     sec.axis = sec_axis(~ ., breaks = team_stats$xG)) +
  scale_color_manual(
    values = c("Burnley" = "#6C1D45",
               "Liverpool" = "#d00027"),
    breaks = c("Burnley", "Liverpool"),
    labels = c("<b style='color: #6C1D45'>Burnley</b>",
               "<b style ='color:#d00027'>Liverpool</b>")) +
  labs(title = glue::glue("<b style ='color:#6C1D45'>{home_stats$TEAMS}: {home_stats$GOALS} </b><b style ='color:#6C1D45; font-size: 20'>({home_stats$xPTS} xPTs)</b><br> <b style='color: #d00027'>{away_stats$TEAMS}: {away_stats$GOALS} </b><b style='color: #d00027; font-size: 20'>({away_stats$xPTS} xPTs)</b>"),
       subtitle = "August 31, 2019 (Matchday 4)",
       x = NULL,
       y = "Expected Goals") +
  theme_minimal() +
  theme(text = element_text(family = "Roboto Condensed"),
        plot.title = element_markdown(size = 40, 
                                      family = "Roboto Condensed"),
        plot.subtitle = element_text(size = 18, 
                                     family = "Roboto Condensed",
                                     color = "grey20"),
        axis.title = element_text(size = 18, color = "grey20"),
        axis.text = element_text(size = 16, face = "bold"),
        panel.grid.minor.x = element_blank(),
        legend.text = element_markdown(size = 18),
        legend.position = c(0.2, 0.95),
        legend.direction = "horizontal",
        legend.title = element_blank())

cumsum_xG_plot


## ------------------------------------------------------------------------
match_url <- stringr::str_glue("{home_url}/match/{match_id}")

match_page <- polite::bow(match_url)

team_stats <- polite::scrape(match_page) %>% 
  html_nodes("div.scheme-block:nth-child(4)") %>% 
  html_text() %>% 
  str_remove_all(., "CHANCES") %>% 
  str_remove_all(., "([0-9]{2,}%)") %>% 
  str_replace_all(., "SHOTS ON TARGET", "ON-TARGET") %>% 
  str_replace_all(., "Newcastle United", "Newcastle-United") %>% 
  str_squish() %>% 
  read.table(text = ., header = FALSE, sep = " ",
             col.names = c("var_name", "home", "away")) %>% 
  t() %>% 
  tibble::as_tibble() %>% 
  janitor::row_to_names(row_number = 1) %>% 
  mutate_at(vars(-TEAMS), ~ as.numeric(.))

home_stats <- team_stats[1,]

away_stats <- team_stats[2,]
away_stats$TEAMS <- "Newcastle United"

team_stats

## set to red vs. blue as DEFAULT
home_stats$home_team_color <- "#d00027"
away_stats$away_team_color <- "#000000"


## ------------------------------------------------------------------------
match_id <- 11683

match_url <- stringr::str_glue("{home_url}/match/{match_id}")

match_page <- polite::bow(match_url)

match_data <- polite::scrape(match_page) %>% 
  get_script()

match_shots_data <- get_data_element(match_data, "shotsData")

#msd <- jsonlite::stream_in(match_shots_data)

match_shots_data <- fix_json(match_shots_data)

# Home: Burnley
liv_shots_data <- fromJSON(match_shots_data[1])

## add 'team_name' with home team name from 'h_team' var
liv_shots_data$team_name <- liv_shots_data$h_team


# Away: LFC
new_shots_data <- fromJSON(match_shots_data[2])

## add 'team_name' with away team name from 'a_team' var
new_shots_data$team_name <- new_shots_data$a_team

## Match date
match_date <- scrape(match_page) %>% 
  html_nodes(".breadcrumb > li:nth-child(3)") %>% 
  html_text()


## ------------------------------------------------------------------------
match_shots_data_clean <- liv_shots_data %>% 
  full_join(new_shots_data) %>% 
  select(-id, -h_team, -a_team,
         -h_goals, -a_goals) %>% 
  mutate_at(vars(minute, xG, X, Y, 
                 player_id, match_id, season), ~ as.numeric(.)) %>% 
  mutate(team_name = forcats::as_factor(team_name),
         xG = if_else(is.na(xG), 0, xG) %>% round(digits = 2),
         result = case_when(
           result == "SavedShot" ~ "Saved Shot",
           result == "BlockedShot" ~ "Blocked Shot",
           result == "MissedShots" ~ "Missed Shot",
           result == "ShotOnPost" ~ "On Post",
           result == "OwnGoal" ~ "Own Goal",
           TRUE ~ result),
         situation = case_when(
           situation == "OpenPlay" ~ "Open Play", 
           situation == "FromCorner" ~ "From Corner",
           situation == "DirectFreekick" ~ "From Free Kick",
           TRUE ~ situation),
         lastAction = case_when(
           lastAction == "BallRecovery" ~ "Ball Recovery",
           lastAction == "BallTouch" ~ "Ball Touch",
           lastAction == "LayOff" ~ "Lay Off",
           lastAction == "TakeOn" ~ "Take On",
           TRUE ~ lastAction),
         shotType = case_when(
           shotType == "LeftFoot" ~ "Left Foot",
           shotType == "RightFoot" ~ "Right Foot",
           TRUE ~ shotType)) %>% 
  arrange(minute) %>% 
  separate(player, into = c("firstname", "player"), 
           sep = "\\s", extra = "merge") %>% 
  ## players like Fabinho listed without Tavares last name
  mutate(player = if_else(is.na(player), firstname, player))

last_min <- match_shots_data_clean$minute %>% unique() %>% last()
if (last_min < 90) {
  last_min <- 90
}

minute <- c(0:last_min)
team_name <- c("Liverpool", "Newcastle United")
# crossing(minute, team_name)

livnew_rollsumxG <- match_shots_data_clean %>% 
  full_join(crossing(minute, team_name)) %>% 
  arrange(minute) %>% 
  group_by(team_name) %>% 
  mutate(xG = if_else(is.na(xG), 0, xG) %>% round(digits = 2),
         rollsum = lag(cumsum(xG))) %>% 
  ungroup() %>% 
  mutate(player_label = case_when(
    result == "Goal" ~ glue::glue("{player}: {xG %>% round(digits = 2)} xG"),
    result == "Own Goal" ~ glue::glue("{player} (Own Goal): {xG %>% round(digits = 2)} xG"),
    TRUE ~ ""),
    # xG = case_when(
    #        result == "OwnGoal" ~ rollsum,
    #        TRUE ~ xG
    #      ),
    ## 
    rollsum_goal = rollsum + xG,
    minute_goal = minute + 1)


## ---- fig.height=6, fig.width=10-----------------------------------------
cumsum_xG_plot <- livnew_rollsumxG %>% 
  ggplot(aes(x = minute_goal, y = rollsum_goal, 
             color = team_name, group = team_name)) +
  geom_line(size = 2.5) +
  geom_label_repel(data = livnew_rollsumxG %>% filter(result == "Goal"),
                   aes(x = minute_goal, y = rollsum_goal, 
                       color = team_name, label = player_label),
                   nudge_x = -10, nudge_y = 0.35,
                   size = 5, show.legend = FALSE) +
  geom_label_repel(data = livnew_rollsumxG %>% filter(result == "Own Goal"),
                   aes(x = minute_goal, y = rollsum_goal, 
                       color = team_name, label = player_label),
                   nudge_x = -10, nudge_y = 0.35,
                   size = 5, show.legend = FALSE) +
  geom_point(data = livnew_rollsumxG %>% 
               filter(result %in% c("Goal", "Own Goal")),
             aes(x = minute_goal, y = rollsum_goal, 
                       color = team_name),
             size = 5, shape = 21, fill = "white", stroke = 1.25,
             show.legend = FALSE) +
  ## if extratime or last min is < 93/94 then just skip 90 min label
  scale_x_continuous(breaks = c(seq(0, 90, by = 5), 90),
                     labels = c(seq(0, 40, by = 5), "HT", 
                                seq(50, 85, by = 5), "", "FT"),
                     expand = c(0.01, 0),
                     limits = c(0, 90)) +
  scale_y_continuous(limits = c(0, 4),
                     sec.axis = sec_axis(~ ., breaks = team_stats$xG)) +
  scale_color_manual(
    values = c("Liverpool" = "#d00027",
               "Newcastle United" = "#000000"),
    breaks = c("Liverpool", "Newcastle United"),
    labels = c("<b style ='color:#d00027'>Liverpool</b>",
               "<b style='color: #000000'>Newcastle United</b>")) +
  labs(title = glue::glue(
    "<b style ='color:{home_stats$home_team_color}'>{home_stats$TEAMS}: {home_stats$GOALS} </b><b style ='color:{home_stats$home_team_color}; font-size: 20'>({home_stats$xPTS} xPTs)</b><br> <b style='color:{away_stats$away_team_color}'>{away_stats$TEAMS}: {away_stats$GOALS} </b><b style='color:{away_stats$away_team_color}; font-size: 20'>({away_stats$xPTS} xPTs)</b>"),
       subtitle = glue::glue("{match_date} (Matchday {matchday_num})"),
       x = NULL,
       y = "Expected Goals") +
  theme_minimal() +
  theme(text = element_text(family = "Roboto Condensed"),
        plot.title = element_markdown(size = 40,
                                      family = "Roboto Condensed"),
        plot.subtitle = element_markdown(size = 18, 
                                     family = "Roboto Condensed",
                                     color = "grey20"),
        axis.title = element_text(size = 18, color = "grey20"),
        axis.text = element_text(size = 16, face = "bold"),
        panel.grid.minor.x = element_blank(),
        legend.text = element_markdown(size = 18),
        legend.position = c(0.2, 0.95),
        legend.direction = "horizontal",
        legend.title = element_blank()
        )

cumsum_xG_plot


## ---- fig.height=8, fig.width=10-----------------------------------------
pitch_custom <- list(
  length = 587,
  width = 373,
  penalty_box_length = 101,
  penalty_box_width = 211,
  six_yard_box_length = 31,
  six_yard_box_width = 111,
  penalty_spot_distance = 66,
  goal_width = 45,
  origin_x = 0,
  origin_y = 0
)


## ---- fig.height=8, fig.width=10-----------------------------------------
df <- data.frame(x = 570, y = 190)

ggplot(df) +
  annotate_pitch(dimensions = pitch_custom) +
  theme_pitch(aspect_ratio = 373/587) +
  geom_point(aes(x = x, y = y), 
             colour = "red", size = 2) +
  geom_point(x = 0, y = 0, 
             colour = "red", size = 2) +
  geom_point(x = 0, y = 219, 
             colour = "red", size = 2) +
  geom_point(x = 524, y = 186.5, 
             colour = "blue", size = 2) 



## ------------------------------------------------------------------------
pitch_custom2 <- list(
  length = 100,
  width = 100,
  penalty_box_length = 17,
  penalty_box_width = 58,
  six_yard_box_length = 6,
  six_yard_box_width = 26.4,
  penalty_spot_distance = 11,
  goal_width = 7.3,
  origin_x = 0,
  origin_y = 0
)


## ---- fig.height=8, fig.width=10-----------------------------------------
liv_shots_data %>% 
  mutate_at(vars(X, Y, xG), ~ as.numeric(.)) %>% 
  mutate(x = X * 100,
         y = Y * 100) %>% 
  select(minute, player, result, X, x, Y, y, xG) %>% 
  ggplot() +
  annotate_pitch(dimensions = pitch_custom2) +
  theme_pitch(aspect_ratio = 800/1000) + 
  geom_point(aes(x = x, y = y, size = xG), 
             colour = "red", show.legend = FALSE)


## ------------------------------------------------------------------------
liv_shots_df <- liv_shots_data %>% 
  mutate_at(vars(X, Y, xG), ~ as.numeric(.)) %>% 
  mutate(x = X * 587,
         y = Y * 373) %>% 
  select(minute, player, result, X, x, Y, y, xG)


ggplot(liv_shots_df) +
  annotate_pitch(dimensions = pitch_custom) +
  theme_pitch(aspect_ratio = 373/587) +
  geom_point(aes(x = x, y = y, size = xG,
                 color = result), 
             show.legend = FALSE)


## ------------------------------------------------------------------------
ars_shots_df <- ars_shots_data %>% 
  mutate_at(vars(X, Y, xG), ~ as.numeric(.)) %>% 
  mutate(x = 587 - (X * 587),
         y = 373 - (Y * 373)) %>% 
  select(minute, player, result, X, x, Y, y, xG)


ggplot(ars_shots_df) +
  annotate_pitch(dimensions = pitch_custom) +
  theme_pitch(aspect_ratio = 373/587) +
  geom_point(aes(x = x, y = y, size = xG), 
             colour = "blue", show.legend = FALSE)


## ------------------------------------------------------------------------
match_shots_data_clean <- match_shots_data_clean %>% 
  mutate(x = case_when(
    h_a == "a" ~ 587 - (X * 587),
    h_a == "h" ~ X * 587,
    TRUE ~ 0),
    y = case_when(
      h_a == "a" ~ 373 - (Y * 373),
      h_a == "h" ~ Y * 373,
      TRUE ~ 0)) %>% 
  select(minute, player, team_name, 
         result, X, x, Y, y, xG) %>% 
  mutate(shot_label = glue::glue("({minute}') {player} - {result}: {xG}"),
         result = forcats::as_factor(result)) %>% 
  mutate(result = forcats::fct_relevel(result, "Goal", "On Post",
                                       "Saved Shot", "Blocked Shot", 
                                       "Missed Shots", "Own Goal")) 


## ------------------------------------------------------------------------
match_shots_data_clean <- match_shots_data_clean %>% 
  mutate(x = case_when(
    h_a == "a" ~ 100 - (X * 100),
    h_a == "h" ~ X * 100,
    TRUE ~ 0),
    y = case_when(
      h_a == "a" ~ 100 - (Y * 100),
      h_a == "h" ~ Y * 100,
      TRUE ~ 0)) %>% 
  select(minute, player, team_name, 
         result, X, x, Y, y, xG) %>% 
  mutate(shot_label = glue::glue("({minute}') {player} - {result}: {xG}"),
         result = forcats::as_factor(result)) %>% 
  mutate(result = forcats::fct_relevel(result, "Goal", "On Post",
                                       "Saved Shot", "Blocked Shot", 
                                       "Missed Shots", "Own Goal")) 


## ---- fig.height=8, fig.width=10-----------------------------------------
shotxG_map <- match_shots_data_clean %>% 
  ggplot(aes(x = x, y = y, 
             size = xG, group = team_name)) +
  annotate_pitch(dimensions = pitch_custom) +
  theme_pitch(aspect_ratio = 373/587) +
  theme(plot.title = element_markdown(family = "Roboto Condensed", 
                                      size = 20, hjust = 0.5),
        plot.caption = element_text(family = "Roboto Condensed",
                                    size = 14, hjust = 0),
        text = element_markdown(family = "Roboto Condensed")) +
  labs(title = glue::glue("
                          <b style ='color:#d00027'>{home_stats$TEAMS}</b> | Shots: {home_stats$SHOTS} | On Target: {home_stats$`ON-TARGET`} | xG per Shot: {round(home_stats$xG/home_stats$SHOTS, 2)}<br>
                          <b style='color:#063672'>{away_stats$TEAMS}</b> | Shots: {away_stats$SHOTS} | On Target: {away_stats$`ON-TARGET`} | xG per Shot: {round(away_stats$xG/away_stats$SHOTS, 2)}"),
       caption = " @R_by_Ryo                                                                                                                                                             Data: understat.com") +
  geom_point(aes(color = team_name), show.legend = FALSE) +
  geom_point(data = match_shots_data_clean %>% 
      filter(result == "Goal"),
      aes(color = team_name), 
      color = "black", stroke = 2.5,
      show.legend = FALSE, shape = 21) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_color_manual(
    values = c("Liverpool" = "#d00027",
               "Arsenal" = "#063672"),
    breaks = c("Liverpool", "Arsenal"),
    labels = c("<b style ='color:#d00027'>Liverpool</b>",
               "<b style='color: #063672'>Arsenal</b>"),
    guide = FALSE) +
  ## Home
  geom_rich_text(x = 440.25, y = 50, 
            color = "grey20", size = 10,
            fill = NA, label.color = NA,
            label = glue::glue("<b style ='color:#d00027'>{home_stats$TEAMS}</b>: {home_stats$xG} xG")) +
  ## Away
  geom_rich_text(x = 146.75, y = 50, 
            color = "grey20", size = 10,
            fill = NA, label.color = NA,
            label = glue::glue("<b style ='color:#063672'>{away_stats$TEAMS}</b>: {away_stats$xG} xG"))
  # geom_text_repel(data = match_shots_data_clean %>% 
  #     filter(result == "Goal"),
  #     aes(label = shot_label), size = 4) +
  # geom_mark_circle(
  #   data = match_shots_data_clean %>% 
  #     filter(result == "Goal", player == "Mohamed Salah", minute == 48),
  #   aes(x = x, y = y, label = shot_label), 
  #   color = "black", size = 1) +
  # geom_mark_circle(
  #   data = match_shots_data_clean %>% 
  #     filter(result == "Goal", player == "Joel Matip"),
  #   aes(x = x, y = y, label = shot_label), 
  #   color = "black", size = 1) +
  # geom_mark_circle(
  #   data = match_shots_data_clean %>% 
  #     filter(result == "Goal", player == "Mohamed Salah", minute == 58),
  #   aes(x = x, y = y, label = shot_label), 
  #   color = "black", size = 1) +
  # geom_mark_circle(
  #   data = match_shots_data_clean %>% 
  #     filter(result == "Goal", player == "Lucas Torreira"),
  #   aes(x = x, y = y, label = shot_label), 
  #   color = "black", size = 1)

shotxG_map


## ------------------------------------------------------------------------
# cowplot::plot_grid(cumsum_xG_plot, shotxG_map,
#                    ncol = 1, align = "hv", axis = "l")
library(grid)
library(gtable)

png(filename = here::here("Premier League 2019-2020/output/livars_plot.png"), 
    width = 1200, height = 1600, res = 144, bg = "white")

one <- ggplotGrob(cumsum_xG_plot)
two <- ggplotGrob(shotxG_map)

gg <- rbind(one, two, size = "last")
gg$widths <- unit.pmax(one$widths, two$widths)

grid.newpage()
grid.draw(gg)
dev.off()


## ---- fig.width=8, fig.height = 10---------------------------------------
png(filename = here::here("Premier League 2019-2020/output/livars-circlePlotONLY.png"), 
    width = 1600, height = 2000, res = 144, bg = "white")

grid.newpage()
grid.arrange(shotxG_map)
dev.off()


## ---- fig.height=8, fig.width=10-----------------------------------------
shape_vals <- c("Goal" = "circle filled",
                "Saved Shot" = "square filled",
                "Blocked Shot" = "diamond filled",
                "Missed Shot" = "triangle filled")

shotxGShape_map <- match_shots_data_clean %>% 
  ggplot(aes(x = x, y = y, 
             size = xG, group = team_name)) +
  annotate_pitch(dimensions = pitch_custom) +
  theme_pitch(aspect_ratio = 373/587) +
  theme(plot.title = element_markdown(family = "Roboto Condensed", 
                                      size = 20, hjust = 0.5),
        plot.caption = element_text(family = "Roboto Condensed",
                                    size = 14, hjust = 0),
        text = element_markdown(family = "Roboto Condensed"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  labs(title = glue::glue("
                          <b style ='color:#d00027'>{home_stats$TEAMS}</b> | Shots: {home_stats$SHOTS} | On Target: {home_stats$`ON-TARGET`} | xG per Shot: {round(home_stats$xG/home_stats$SHOTS, 2)}<br>
                          <b style='color:#063672'>{away_stats$TEAMS}</b> | Shots: {away_stats$SHOTS} | On Target: {away_stats$`ON-TARGET`} | xG per Shot: {round(away_stats$xG/away_stats$SHOTS, 2)}"),
       caption = " @R_by_Ryo                                                                                                                                                             Data: understat.com") +
  geom_point(aes(fill = team_name, shape = result),
             color = "black") +
  geom_point(data = match_shots_data_clean %>% 
      filter(result == "Goal"),
      aes(fill = team_name, shape = result), 
      stroke = 2.5, color = "black",
      show.legend = FALSE) +
  scale_size(guide = FALSE) +
  scale_shape_manual(values = shape_vals, 
                     name = "Result",
                     guide = guide_legend(override.aes = list(size = 5))) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_fill_manual(
    values = c("Liverpool" = "#d00027",
               "Arsenal" = "#063672"),
    guide = FALSE) +
  ## Home
  geom_rich_text(x = 440.25, y = 50, 
            color = "grey20", size = 10,
            fill = NA, label.color = NA,
            label = glue::glue("<b style ='color:#d00027'>{home_stats$TEAMS}</b>: {home_stats$xG} xG")) +
  ## Away
  geom_rich_text(x = 146.75, y = 50, 
            color = "grey20", size = 10,
            fill = NA, label.color = NA,
            label = glue::glue("<b style ='color:#063672'>{away_stats$TEAMS}</b>: {away_stats$xG} xG"))

shotxGShape_map


## ---- fig.width=8, fig.height = 10---------------------------------------
png(filename = here::here("Premier League 2019-2020/output/livars-shapesplot.png"), 
    width = 1600, height = 2000, res = 144, bg = "white")

grid.newpage()
grid.arrange(cumsum_xG_plot, shotxG_map)
dev.off()


## ---- fig.width=8, fig.height = 10---------------------------------------
png(filename = here::here("Premier League 2019-2020/output/livars-shapePlotONLY.png"), 
    width = 1600, height = 2000, res = 144, bg = "white")

grid.newpage()
grid.arrange(shotxGShape_map)
dev.off()


## ---- fig.height=8, fig.width=10-----------------------------------------
# http://colorbrewer2.org/#type=diverging&scheme=RdYlGn&n=4
fill_cols <- c("Goal" = "#1a9641",
               "Saved Shot" = "#a6d96a",
               "Blocked Shot" = "#fdae61",
               "Missed Shot" = "#d7191c")

fill_cols2 <- c("Goal" = "#1a9641",
               "Saved Shot" = "orange",
               "Blocked Shot" = "grey",
               "Missed Shot" = "black",
               "Own Goal" = "#d7191c",
               "Shot On Post" = "dodgerblue")

fill_cols3 <- c("Goal" = "#1a9641",
               "Saved Shot" = "#a6d96a",
               "Blocked Shot" = "#fdae61",
               "Missed Shot" = "#d7191c")

shotxG_map <- match_shots_data_clean %>% 
  ggplot(aes(x = x, y = y, 
             group = team_name)) +
  annotate_pitch(dimensions = pitch_custom) +
  theme_pitch(aspect_ratio = 373/587) +
  theme(plot.title = element_markdown(family = "Roboto Condensed", 
                                      size = 20, hjust = 0.5),
        plot.caption = element_text(family = "Roboto Condensed",
                                    size = 14, hjust = 0),
        text = element_markdown(family = "Roboto Condensed"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  labs(title = glue::glue("
                          <b style ='color:#d00027'>{home_stats$TEAMS}</b> | Shots: {home_stats$SHOTS} | On Target: {home_stats$`ON-TARGET`} | xG per Shot: {round(home_stats$xG/home_stats$SHOTS, 2)}<br>
                          <b style='color:#063672'>{away_stats$TEAMS}</b> | Shots: {away_stats$SHOTS} | On Target: {away_stats$`ON-TARGET`} | xG per Shot: {round(away_stats$xG/away_stats$SHOTS, 2)}"),
       caption = " @R_by_Ryo                                                                                                                                                             Data: understat.com") +
  geom_point(aes(size = xG, 
                 color = result),
             stroke = 1.5) +
  geom_point(data = match_shots_data_clean %>% 
      filter(result == "Goal"),
      aes(color = result,
          size = xG), 
      stroke = 1.5,
      show.legend = FALSE) +
  scale_size(guide = FALSE) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_color_manual(values = fill_cols2, name = "Result",
                    guide = guide_legend(override.aes = list(size = 4.55))) +
  ## Home
  geom_rich_text(x = 440.25, y = 50, 
            color = "grey20", size = 10,
            fill = NA, label.color = NA,
            label = glue::glue("<b style ='color:#d00027'>{home_stats$TEAMS}</b>: {home_stats$xG} xG")) +
  ## Away
  geom_rich_text(x = 146.75, y = 50, 
            color = "grey20", size = 10,
            fill = NA, label.color = NA,
            label = glue::glue("<b style ='color:#063672'>{away_stats$TEAMS}</b>: {away_stats$xG} xG"))

shotxG_map


## ---- fig.width=800, fig.height = 10-------------------------------------
png(filename = here::here("Premier League 2019-2020/output/livars_shotresult_plot.png"), 
    width = 1600, height = 2000, res = 144, bg = "white")

grid.newpage()
grid.arrange(cumsum_xG_plot, shotxG_map)
dev.off()


## ---- fig.width=8, fig.height = 10---------------------------------------
png(filename = here::here("Premier League 2019-2020/output/livars-colorPlotONLY.png"), 
    width = 1600, height = 2000, res = 144, bg = "white")

grid.newpage()
grid.arrange(shotxG_map)
dev.off()


## ---- fig.height=8, fig.width=10-----------------------------------------
# http://colorbrewer2.org/#type=diverging&scheme=RdYlGn&n=4
fill_cols <- c("Goal" = "#1a9641",
               "Saved Shot" = "#a6d96a",
               "Blocked Shot" = "#fdae61",
               "Missed Shot" = "#d7191c")

shotxG_map <- match_shots_data_clean %>% 
  ggplot(aes(x = x, y = y, 
             group = team_name)) +
  annotate_pitch(dimensions = pitch_custom) +
  theme_pitch(aspect_ratio = 373/587) +
  theme(plot.title = element_markdown(family = "Roboto Condensed", 
                                      size = 20, hjust = 0.5),
        plot.caption = element_text(family = "Roboto Condensed",
                                    size = 14, hjust = 0),
        text = element_markdown(family = "Roboto Condensed"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  labs(title = glue::glue("
                          <b style ='color:#d00027'>{home_stats$TEAMS}</b> | Shots: {home_stats$SHOTS} | On Target: {home_stats$`ON-TARGET`} | xG per Shot: {round(home_stats$xG/home_stats$SHOTS, 2)}<br>
                          <b style='color:#063672'>{away_stats$TEAMS}</b> | Shots: {away_stats$SHOTS} | On Target: {away_stats$`ON-TARGET`} | xG per Shot: {round(away_stats$xG/away_stats$SHOTS, 2)}"),
       caption = " @R_by_Ryo                                                                                                                                                             Data: understat.com") +
  geom_point(aes(alpha = xG, 
                 fill = result), size = 4,
             color = "black", shape = 21, stroke = 1.5) +
  scale_alpha(guide = FALSE) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_fill_manual(values = fill_cols, name = "Result",
                    guide = guide_legend(override.aes = list(size = 4.55))) +
  ## Home
  geom_rich_text(x = 440.25, y = 50, 
            color = "grey20", size = 10,
            fill = NA, label.color = NA,
            label = glue::glue("<b style ='color:#d00027'>{home_stats$TEAMS}</b>: {home_stats$xG} xG")) +
  ## Away
  geom_rich_text(x = 146.75, y = 50, 
            color = "grey20", size = 10,
            fill = NA, label.color = NA,
            label = glue::glue("<b style ='color:#063672'>{away_stats$TEAMS}</b>: {away_stats$xG} xG"))

shotxG_map


## ---- fig.width=8, fig.height = 10---------------------------------------
png(filename = here::here("Premier League 2019-2020/output/livars-alphaPlotONLY.png"), 
    width = 1600, height = 2000, res = 144, bg = "white")

grid.newpage()
grid.arrange(shotxG_map)
dev.off()


## ------------------------------------------------------------------------
format(round(away_stats$xG/away_stats$SHOTS, 3))

round(away_stats$xG/away_stats$SHOTS, 2)

away_stats$xG/away_stats$SHOTS %>% round(2)


## ---- fig.height=8, fig.width=10-----------------------------------------
shotxG_map <- match_shots_data_clean %>% 
  ggplot(aes(x = x, y = y, 
             size = xG, group = team_name)) +
  annotate_pitch(dimensions = pitch_custom) +
  theme_pitch(aspect_ratio = 373/587) +
  labs(title = glue::glue("
                          <b style ='color:#6C1D45'>{home_stats$TEAMS}</b> | Shots: {home_stats$SHOTS} | On Target: {home_stats$`ON-TARGET`} | xG per Shot: {round(home_stats$xG/home_stats$SHOTS, 2)}<br>
                          <b style='color:#d00027'>{away_stats$TEAMS}</b> | Shots: {away_stats$SHOTS} | On Target: {away_stats$`ON-TARGET`} | xG per Shot: {round(away_stats$xG/away_stats$SHOTS, 2)}"),
       caption = " @R_by_Ryo                                                                                                                                                             Data: understat.com") +
  geom_point(aes(color = team_name), show.legend = FALSE) +
  geom_point(data = match_shots_data_clean %>% 
      filter(result == "Goal"),
      aes(color = team_name), 
      color = "black", stroke = 2.5,
      show.legend = FALSE, shape = 21) +
  geom_point(data = match_shots_data_clean %>% 
      filter(result == "OwnGoal"),
      aes(color = team_name), 
      color = "black", stroke = 2.5,
      show.legend = FALSE, shape = 21) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_color_manual(
    values = c("Liverpool" = "#d00027",
               "Burnley" = "#6C1D45"),
    breaks = c("Liverpool", "Burnley"),
    labels = c("<b style ='color:#d00027'>Liverpool</b>",
               "<b style='color: #6C1D45'>Burnley</b>"),
    guide = FALSE) +
  theme(plot.title = element_markdown(family = "Roboto Condensed", 
                                      size = 20, hjust = 0.5),
        plot.caption = element_text(family = "Roboto Condensed",
                                    size = 14, hjust = 0))

shotxG_map


## ------------------------------------------------------------------------
# cowplot::plot_grid(cumsum_xG_plot, shotxG_map,
#                    ncol = 1, align = "hv", axis = "l")
library(grid)
library(gtable)

png(filename = here::here("Premier League 2019-2020/output/burnliv_plot2.png"), 
    width = 1600, height = 2000, res = 144, bg = "white")

one <- ggplotGrob(cumsum_xG_plot)
two <- ggplotGrob(shotxG_map)

gg <- rbind(one, two, size = "first")
#gg$widths <- unit.pmax(one$widths, two$widths)
#gg$heights <- unit.pmax(one$heights, two$heights)

grid.newpage()
grid.draw(gg)
dev.off()


## ---- fig.width=800, fig.height = 10-------------------------------------
library(gridExtra)

png(filename = here::here("Premier League 2019-2020/output/burnliv_plot3.png"), 
    width = 1600, height = 2000, res = 144, bg = "white")

grid.newpage()
grid.arrange(cumsum_xG_plot, shotxG_map)
dev.off()


## ---- fig.height=8, fig.width=10-----------------------------------------
shape_vals <- c("Goal" = "circle filled",
                "OwnGoal" = "asterisk",
                "Saved Shot" = "square filled",
                "Blocked Shot" = "diamond filled",
                "Missed Shot" = "triangle filled")

shotxGShape_map <- match_shots_data_clean %>% 
  ggplot(aes(x = x, y = y, 
             size = xG, group = team_name)) +
  annotate_pitch(dimensions = pitch_custom) +
  theme_pitch(aspect_ratio = 373/587) +
  theme(plot.title = element_markdown(family = "Roboto Condensed", 
                                      size = 20, hjust = 0.5),
        plot.caption = element_text(family = "Roboto Condensed",
                                    size = 14, hjust = 0),
        text = element_markdown(family = "Roboto Condensed"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  labs(title = glue::glue("
                          <b style ='color:#6C1D45'>{home_stats$TEAMS}</b> | Shots: {home_stats$SHOTS} | On Target: {home_stats$`ON-TARGET`} | xG per Shot: {round(home_stats$xG/home_stats$SHOTS, 2)}<br>
                          <b style='color:#d00027'>{away_stats$TEAMS}</b> | Shots: {away_stats$SHOTS} | On Target: {away_stats$`ON-TARGET`} | xG per Shot: {round(away_stats$xG/away_stats$SHOTS, 2)}"),
       caption = " @R_by_Ryo                                                                                                                                                             Data: understat.com") +
  geom_point(aes(fill = team_name, shape = result),
             color = "black") +
  geom_point(data = match_shots_data_clean %>% 
      filter(result == "Goal"),
      aes(fill = team_name, shape = result), 
      stroke = 2.5, color = "black",
      show.legend = FALSE) +
  scale_size(guide = FALSE) +
  scale_shape_manual(values = shape_vals, 
                     name = "Result",
                     guide = guide_legend(override.aes = list(size = 5))) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_fill_manual(
    values = c("Liverpool" = "#d00027",
               "Burnley" = "#6C1D45"),
    guide = FALSE) +
  ## Home
  geom_rich_text(x = 440.25, y = 50, 
            color = "grey20", size = 10,
            fill = NA, label.color = NA,
            label = glue::glue("<b style ='color:#6C1D45'>{home_stats$TEAMS}</b>: {home_stats$xG} xG")) +
  ## Away
  geom_rich_text(x = 146.75, y = 50, 
            color = "grey20", size = 10,
            fill = NA, label.color = NA,
            label = glue::glue("<b style ='color:#d00027'>{away_stats$TEAMS}</b>: {away_stats$xG} xG"))

shotxGShape_map


## ---- fig.width=800, fig.height = 10-------------------------------------
library(gridExtra)

png(filename = here::here("Premier League 2019-2020/output/burnliv_shotresultplot.png"), 
    width = 1600, height = 2000, res = 144, bg = "white")

grid.newpage()
grid.arrange(cumsum_xG_plot, shotxGShape_map)
dev.off()


## ---- fig.height=8, fig.width=10-----------------------------------------
# http://colorbrewer2.org/#type=diverging&scheme=RdYlGn&n=4
fill_cols2 <- c("Goal" = "#1a9641",
               "Saved Shot" = "orange",
               "Blocked Shot" = "grey",
               "Missed Shot" = "black",
               "Own Goal" = "#d7191c",
               "Shot On Post" = "dodgerblue")

shotxG_map <- match_shots_data_clean %>% 
  ggplot(aes(x = x, y = y, 
             group = team_name)) +
  annotate_pitch(dimensions = pitch_custom) +
  theme_pitch(aspect_ratio = 373/587) +
  theme(plot.title = element_markdown(family = "Roboto Condensed", 
                                      size = 20, hjust = 0.5),
        plot.caption = element_text(family = "Roboto Condensed",
                                    size = 14, hjust = 0),
        text = element_markdown(family = "Roboto Condensed"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  labs(title = glue::glue("
                          <b style ='color:#6C1D45'>{home_stats$TEAMS}</b> | Shots: {home_stats$SHOTS} | On Target: {home_stats$`ON-TARGET`} | xG per Shot: {round(home_stats$xG/home_stats$SHOTS, 2)}<br>
                          <b style='color:#d00027'>{away_stats$TEAMS}</b> | Shots: {away_stats$SHOTS} | On Target: {away_stats$`ON-TARGET`} | xG per Shot: {round(away_stats$xG/away_stats$SHOTS, 2)}"),
       caption = " @R_by_Ryo                                                                                                                                                             Data: understat.com") +
  geom_point(aes(size = xG, 
                 color = result),
             stroke = 1.5) +
  geom_point(data = match_shots_data_clean %>% 
      filter(result == "Goal"),
      aes(color = result,
          size = xG), 
      stroke = 1.5,
      show.legend = FALSE) +
  scale_size(guide = FALSE) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_color_manual(values = fill_cols2, name = "Result",
                    guide = guide_legend(override.aes = list(size = 4.55))) +
  ## Home
  geom_rich_text(x = 440.25, y = 50, 
            color = "grey20", size = 10,
            fill = NA, label.color = NA,
            label = glue::glue("<b style ='color:#6C1D45'>{home_stats$TEAMS}</b>: {home_stats$xG} xG")) +
  ## Away
  geom_rich_text(x = 146.75, y = 50, 
            color = "grey20", size = 10,
            fill = NA, label.color = NA,
            label = glue::glue("<b style ='color:#d00027'>{away_stats$TEAMS}</b>: {away_stats$xG} xG"))

shotxG_map


## ---- fig.width=800, fig.height = 10-------------------------------------
png(filename = here::here("Premier League 2019-2020/output/burnliv_shotresult_plot.png"), 
    width = 1600, height = 2000, res = 144, bg = "white")

grid.newpage()
grid.arrange(cumsum_xG_plot, shotxG_map)
dev.off()


## ---- fig.height=8, fig.width=10-----------------------------------------
# http://colorbrewer2.org/#type=diverging&scheme=RdYlGn&n=4
fill_cols <- c("Goal" = "#1a9641",
               "Saved Shot" = "#a6d96a",
               "Blocked Shot" = "#fdae61",
               "Missed Shot" = "#d7191c",
               "OwnGoal" = "darkred")

shotxG_map <- match_shots_data_clean %>% 
  ggplot(aes(x = x, y = y, 
             group = team_name)) +
  annotate_pitch(dimensions = pitch_custom) +
  theme_pitch(aspect_ratio = 373/587) +
  theme(plot.title = element_markdown(family = "Roboto Condensed", 
                                      size = 20, hjust = 0.5),
        plot.caption = element_text(family = "Roboto Condensed",
                                    size = 14, hjust = 0),
        text = element_markdown(family = "Roboto Condensed"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  labs(title = glue::glue("
                          <b style ='color:#6C1D45'>{home_stats$TEAMS}</b> | Shots: {home_stats$SHOTS} | On Target: {home_stats$`ON-TARGET`} | xG per Shot: {round(home_stats$xG/home_stats$SHOTS, 2)}<br>
                          <b style='color:#d00027'>{away_stats$TEAMS}</b> | Shots: {away_stats$SHOTS} | On Target: {away_stats$`ON-TARGET`} | xG per Shot: {round(away_stats$xG/away_stats$SHOTS, 2)}"),
       caption = " @R_by_Ryo                                                                                                                                                             Data: understat.com") +
  geom_point(aes(alpha = xG, 
                 fill = result), size = 4,
             color = "black", shape = 21, stroke = 1.5) +
  scale_alpha(guide = FALSE) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_fill_manual(values = fill_cols, name = "Result",
                    guide = guide_legend(override.aes = list(size = 4.55))) +
  ## Home
  geom_rich_text(x = 440.25, y = 50, 
            color = "grey20", size = 10,
            fill = NA, label.color = NA,
            label = glue::glue("<b style ='color:#6C1D45'>{home_stats$TEAMS}</b>: {home_stats$xG} xG")) +
  ## Away
  geom_rich_text(x = 146.75, y = 50, 
            color = "grey20", size = 10,
            fill = NA, label.color = NA,
            label = glue::glue("<b style ='color:#d00027'>{away_stats$TEAMS}</b>: {away_stats$xG} xG"))

shotxG_map


## ---- fig.width=800, fig.height = 10-------------------------------------
library(gridExtra)

png(filename = here::here("Premier League 2019-2020/output/burnliv_shotresultAlphaplot.png"), 
    width = 1600, height = 2000, res = 144, bg = "white")

grid.newpage()
grid.arrange(cumsum_xG_plot, shotxG_map)
dev.off()


## ---- fig.height=8, fig.width=10-----------------------------------------
shotxG_map <- match_shots_data_clean %>% 
  ggplot(aes(x = x, y = y, 
             size = xG, group = team_name)) +
  annotate_pitch(dimensions = pitch_custom) +
  theme_pitch(aspect_ratio = 373/587) +
  theme(plot.title = element_markdown(family = "Roboto Condensed", 
                                      size = 20, hjust = 0.5),
        plot.caption = element_text(family = "Roboto Condensed",
                                    size = 14, hjust = 0)) +
  labs(title = glue::glue("
                          <b style ='color:#d00027'>{home_stats$TEAMS}</b> | Shots: {home_stats$SHOTS} | On Target: {home_stats$`ON-TARGET`} | xG per Shot: {round(home_stats$xG/home_stats$SHOTS, 2)}<br>
                          <b style='color:#00a650'>{away_stats$TEAMS}</b> | Shots: {away_stats$SHOTS} | On Target: {away_stats$`ON-TARGET`} | xG per Shot: {round(away_stats$xG/away_stats$SHOTS, 2)}"),
       caption = " @R_by_Ryo                                                                                                                                                             Data: understat.com") +
  geom_point(aes(color = team_name), show.legend = FALSE) +
  geom_point(data = match_shots_data_clean %>% 
      filter(result == "Goal"),
      aes(color = team_name), 
      color = "black", stroke = 2.5,
      show.legend = FALSE, shape = 21) +
  geom_point(data = match_shots_data_clean %>% 
      filter(result == "OwnGoal"),
      aes(color = team_name), 
      color = "black", stroke = 2.5,
      show.legend = FALSE, shape = 21) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_color_manual(
    values = c("Liverpool" = "#d00027",
               "Norwich" = "#00a650"),
    breaks = c("Liverpool", "Norwich"),
    labels = c("<b style ='color:#d00027'>Liverpool</b>",
               "<b style='color: #00a650'>Norwich</b>"),
    guide = FALSE) +
  ## Home
  geom_rich_text(x = 440.25, y = 50, 
            color = "grey20", size = 10,
            fill = NA, label.color = NA,
            label = glue::glue("<b style ='color:#d00027'>{home_stats$TEAMS}</b>: {home_stats$xG} xG")) +
  ## Away
  geom_rich_text(x = 146.75, y = 50, 
            color = "grey20", size = 10,
            fill = NA, label.color = NA,
            label = glue::glue("<b style ='color:#00a650'>{away_stats$TEAMS}</b>: {away_stats$xG} xG"))

shotxG_map


## ---- fig.width=800, fig.height = 10-------------------------------------
library(grid)
library(gridExtra)

png(filename = here::here("Premier League 2019-2020/output/livnor_shotresultplot.png"), 
    width = 1600, height = 2000, res = 144, bg = "white")

grid.newpage()
grid.arrange(cumsum_xG_plot, shotxG_map)
dev.off()


## ---- fig.height=8, fig.width=10-----------------------------------------
# http://colorbrewer2.org/#type=diverging&scheme=RdYlGn&n=4
fill_cols2 <- c("Goal" = "#1a9641",
               "Saved Shot" = "orange",
               "Blocked Shot" = "grey",
               "Missed Shot" = "black",
               "Own Goal" = "#d7191c",
               "On Post" = "dodgerblue")

shotxG_map <- match_shots_data_clean %>% 
  ggplot(aes(x = x, y = y, 
             group = team_name)) +
  annotate_pitch(dimensions = pitch_custom) +
  theme_pitch(aspect_ratio = 373/587) +
  theme(plot.title = element_markdown(family = "Roboto Condensed", 
                                      size = 20, hjust = 0.5),
        plot.caption = element_text(family = "Roboto Condensed",
                                    size = 14, hjust = 0),
        text = element_markdown(family = "Roboto Condensed"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  labs(title = glue::glue("
                          <b style ='color:#d00027'>{home_stats$TEAMS}</b> | Shots: {home_stats$SHOTS} | On Target: {home_stats$`ON-TARGET`} | xG per Shot: {round(home_stats$xG/home_stats$SHOTS, 2)}<br>
                          <b style='color:#00a650'>{away_stats$TEAMS}</b> | Shots: {away_stats$SHOTS} | On Target: {away_stats$`ON-TARGET`} | xG per Shot: {round(away_stats$xG/away_stats$SHOTS, 2)}"),
       caption = " @R_by_Ryo                                                                                                                                                             Data: understat.com") +
  geom_point(aes(size = xG, 
                 color = result),
             stroke = 1.5) +
  geom_point(data = match_shots_data_clean %>% 
      filter(result == "Goal"),
      aes(color = result,
          size = xG), 
      stroke = 1.5,
      show.legend = FALSE) +
  scale_size(guide = FALSE) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_color_manual(values = fill_cols2, name = "Result",
                    guide = guide_legend(override.aes = list(size = 4.55))) +
  ## Home
  geom_rich_text(x = 440.25, y = 50, 
            color = "grey20", size = 10,
            fill = NA, label.color = NA,
            label = glue::glue("<b style ='color:#d00027'>{home_stats$TEAMS}</b>: {home_stats$xG} xG")) +
  ## Away
  geom_rich_text(x = 146.75, y = 50, 
            color = "grey20", size = 10,
            fill = NA, label.color = NA,
            label = glue::glue("<b style ='color:#00a650'>{away_stats$TEAMS}</b>: {away_stats$xG} xG"))

shotxG_map


## ---- fig.width=800, fig.height = 10-------------------------------------
png(filename = here::here("Premier League 2019-2020/output/livnor_shotresult_plot.png"), 
    width = 1600, height = 2000, res = 144, bg = "white")

grid.newpage()
grid.arrange(cumsum_xG_plot, shotxG_map)
dev.off()


## ---- fig.height=8, fig.width=10-----------------------------------------
# http://colorbrewer2.org/#type=diverging&scheme=RdYlGn&n=4
fill_cols2 <- c("Goal" = "#1a9641",
               "Saved Shot" = "orange",
               "Blocked Shot" = "grey",
               "Missed Shot" = "black",
               "Own Goal" = "#d7191c",
               "On Post" = "dodgerblue")

shotxG_map <- match_shots_data_clean %>% 
  ggplot(aes(x = x, y = y, 
             group = team_name)) +
  annotate_pitch(dimensions = pitch_custom2) +
  theme_pitch(aspect_ratio = 900/1100) +
  theme(plot.title = element_markdown(family = "Roboto Condensed", 
                                      size = 20, hjust = 0.5),
        plot.caption = element_text(family = "Roboto Condensed",
                                    size = 14, hjust = 0),
        text = element_markdown(family = "Roboto Condensed"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  labs(title = glue::glue("
                          <b style ='color:#d00027'>{home_stats$TEAMS}</b> | Shots: {home_stats$SHOTS} | On Target: {home_stats$`ON-TARGET`} | xG per Shot: {round(home_stats$xG/home_stats$SHOTS, 2)}<br>
                          <b style='color:#00a650'>{away_stats$TEAMS}</b> | Shots: {away_stats$SHOTS} | On Target: {away_stats$`ON-TARGET`} | xG per Shot: {round(away_stats$xG/away_stats$SHOTS, 2)}"),
       caption = " @R_by_Ryo                                                                                                                                                             Data: understat.com") +
  geom_point(aes(size = xG, 
                 color = result),
             stroke = 1.5) +
  geom_point(data = match_shots_data_clean %>% 
      filter(result == "Goal"),
      aes(color = result,
          size = xG), 
      stroke = 1.5,
      show.legend = FALSE) +
  scale_size(guide = FALSE) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_color_manual(values = fill_cols2, name = "Result",
                    guide = guide_legend(override.aes = list(size = 4.55))) +
  ## Home
  geom_rich_text(x = 75.25, y = 10, 
            color = "grey20", size = 10,
            fill = NA, label.color = NA,
            label = glue::glue("<b style ='color:#d00027'>{home_stats$TEAMS}</b>: {home_stats$xG} xG")) +
  ## Away
  geom_rich_text(x = 25.75, y = 10, 
            color = "grey20", size = 10,
            fill = NA, label.color = NA,
            label = glue::glue("<b style ='color:#00a650'>{away_stats$TEAMS}</b>: {away_stats$xG} xG"))

shotxG_map


## ---- fig.height=8, fig.width=10-----------------------------------------
shotxG_map <- match_shots_data_clean %>% 
  ggplot(aes(x = x, y = y, 
             size = xG, group = team_name)) +
  annotate_pitch(dimensions = pitch_custom) +
  theme_pitch(aspect_ratio = 373/587) +
  labs(title = glue::glue("
                          <b style ='color:#ed1a3b'>{home_stats$TEAMS}</b> | Shots: {home_stats$SHOTS} | On Target: {home_stats$`ON-TARGET`} | xG per Shot: {round(home_stats$xG/home_stats$SHOTS, 2)}<br>
                          <b style='color:#000000'>{away_stats$TEAMS}</b> | Shots: {away_stats$SHOTS} | On Target: {away_stats$`ON-TARGET`} | xG per Shot: {round(away_stats$xG/away_stats$SHOTS, 2)}"),
       caption = " @R_by_Ryo                                                                                                                                                             Data: understat.com") +
  geom_point(aes(color = team_name), show.legend = FALSE) +
  geom_point(data = match_shots_data_clean %>% 
      filter(result == "Goal"),
      aes(color = team_name), 
      color = "black", stroke = 2.5,
      show.legend = FALSE, shape = 21) +
  geom_point(data = match_shots_data_clean %>% 
      filter(result == "OwnGoal"),
      aes(color = team_name), 
      color = "black", stroke = 2.5,
      show.legend = FALSE, shape = 21) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_color_manual(
    values = c("Liverpool" = "#000000",
               "Southampton" = "#ed1a3b"),
    breaks = c("Liverpool", "Southampton"),
    labels = c("<b style ='color:#000000'>Liverpool</b>",
               "<b style='color:#ed1a3b'>Southampton</b>"),
    guide = FALSE) +
  theme(plot.title = element_markdown(family = "Roboto Condensed", 
                                      size = 20, hjust = 0.5),
        plot.caption = element_text(family = "Roboto Condensed",
                                    size = 14, hjust = 0))

shotxG_map


## ---- fig.width=800, fig.height = 10-------------------------------------
library(grid)
library(gridExtra)

png(filename = here::here("Premier League 2019-2020/output/sotliv_plot1.png"), 
    width = 1600, height = 2000, res = 144, bg = "white")

grid.newpage()
grid.arrange(cumsum_xG_plot, shotxG_map)
dev.off()


## ---- fig.height=8, fig.width=10-----------------------------------------
# http://colorbrewer2.org/#type=diverging&scheme=RdYlGn&n=4
fill_cols <- c("Goal" = "#1a9641",
               "Saved Shot" = "#a6d96a",
               "Blocked Shot" = "#fdae61",
               "Missed Shot" = "#d7191c")

shotxG_map <- match_shots_data_clean %>% 
  ggplot(aes(x = x, y = y, 
             size = xG, group = team_name)) +
  annotate_pitch(dimensions = pitch_custom) +
  theme_pitch(aspect_ratio = 373/587) +
  theme(plot.title = element_markdown(family = "Roboto Condensed", 
                                      size = 20, hjust = 0.5),
        plot.caption = element_text(family = "Roboto Condensed",
                                    size = 14, hjust = 0),
        text = element_markdown(family = "Roboto Condensed"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  labs(title = glue::glue("
                          <b style ='color:#ed1a3b'>{home_stats$TEAMS}</b> | Shots: {home_stats$SHOTS} | On Target: {home_stats$`ON-TARGET`} | xG per Shot: {round(home_stats$xG/home_stats$SHOTS, 2)}<br>
                          <b style='color:#000000'>{away_stats$TEAMS}</b> | Shots: {away_stats$SHOTS} | On Target: {away_stats$`ON-TARGET`} | xG per Shot: {round(away_stats$xG/away_stats$SHOTS, 2)}"),
       caption = " @R_by_Ryo                                                                                                                                                             Data: understat.com") +
  geom_point(aes(alpha = xG, 
                 fill = result), size = 4,
             color = "black", shape = 21, stroke = 1.5) +
  scale_alpha(guide = FALSE) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_fill_manual(values = fill_cols, name = "Result",
                    guide = guide_legend(override.aes = list(size = 4.55))) +
  ## Home
  geom_rich_text(x = 440.25, y = 50, 
            color = "grey20", size = 10,
            fill = NA, label.color = NA,
            label = glue::glue("<b style ='color:#ed1a3b'>{home_stats$TEAMS}</b>: {home_stats$xG} xG")) +
  ## Away
  geom_rich_text(x = 146.75, y = 50, 
            color = "grey20", size = 10,
            fill = NA, label.color = NA,
            label = glue::glue("<b style ='color:#000000'>{away_stats$TEAMS}</b>: {away_stats$xG} xG"))

shotxG_map


## ---- fig.width=800, fig.height = 10-------------------------------------
library(grid)
library(gridExtra)

png(filename = here::here("Premier League 2019-2020/output/sotliv_shotresultplot.png"), 
    width = 1600, height = 2000, res = 144, bg = "white")

grid.newpage()
grid.arrange(cumsum_xG_plot, shotxG_map)
dev.off()


## ---- fig.height=8, fig.width=10-----------------------------------------
# http://colorbrewer2.org/#type=diverging&scheme=RdYlGn&n=4
fill_cols2 <- c("Goal" = "#1a9641",
               "Saved Shot" = "orange",
               "Blocked Shot" = "grey",
               "Missed Shot" = "black",
               "Own Goal" = "#d7191c",
               "Shot On Post" = "dodgerblue")

shotxG_map <- match_shots_data_clean %>% 
  ggplot(aes(x = x, y = y, 
             group = team_name)) +
  annotate_pitch(dimensions = pitch_custom) +
  theme_pitch(aspect_ratio = 373/587) +
  theme(plot.title = element_markdown(family = "Roboto Condensed", 
                                      size = 20, hjust = 0.5),
        plot.caption = element_text(family = "Roboto Condensed",
                                    size = 14, hjust = 0),
        text = element_markdown(family = "Roboto Condensed"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  labs(title = glue::glue("
                          <b style ='color:#ed1a3b'>{home_stats$TEAMS}</b> | Shots: {home_stats$SHOTS} | On Target: {home_stats$`ON-TARGET`} | xG per Shot: {round(home_stats$xG/home_stats$SHOTS, 2)}<br>
                          <b style='color:#000000'>{away_stats$TEAMS}</b> | Shots: {away_stats$SHOTS} | On Target: {away_stats$`ON-TARGET`} | xG per Shot: {round(away_stats$xG/away_stats$SHOTS, 2)}"),
       caption = " @R_by_Ryo                                                                                                                                                             Data: understat.com") +
  geom_point(aes(size = xG, 
                 color = result),
             stroke = 1.5) +
  geom_point(data = match_shots_data_clean %>% 
      filter(result == "Goal"),
      aes(color = result,
          size = xG), 
      stroke = 1.5,
      show.legend = FALSE) +
  scale_size(guide = FALSE) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_color_manual(values = fill_cols2, name = "Result",
                    guide = guide_legend(override.aes = list(size = 4.55))) +
  ## Home
  geom_rich_text(x = 440.25, y = 50, 
            color = "grey20", size = 10,
            fill = NA, label.color = NA,
            label = glue::glue("<b style ='color:#ed1a3b'>{home_stats$TEAMS}</b>: {home_stats$xG} xG")) +
  ## Away
  geom_rich_text(x = 146.75, y = 50, 
            color = "grey20", size = 10,
            fill = NA, label.color = NA,
            label = glue::glue("<b style ='color:#000000'>{away_stats$TEAMS}</b>: {away_stats$xG} xG"))

shotxG_map


## ---- fig.width=8, fig.height = 10---------------------------------------
png(filename = here::here("Premier League 2019-2020/output/sotliv_shotresult_plot.png"), 
    width = 1600, height = 2000, res = 144, bg = "white")

grid.newpage()
grid.arrange(cumsum_xG_plot, shotxG_map)
dev.off()


## ---- fig.height=8, fig.width=10-----------------------------------------
# http://colorbrewer2.org/#type=diverging&scheme=RdYlGn&n=4
fill_cols2 <- c("Goal" = "#1a9641",
               "Saved Shot" = "orange",
               "Blocked Shot" = "grey",
               "Missed Shot" = "black",
               "Own Goal" = "#d7191c",
               "On Post" = "#004CFF")

shotxG_map <- match_shots_data_clean %>% 
  ggplot(aes(x = x, y = y, 
             group = team_name)) +
  annotate_pitch(dimensions = pitch_custom) +
  theme_pitch(aspect_ratio = 373/587) +
  theme(plot.title = element_markdown(family = "Roboto Condensed", 
                                      size = 20, hjust = 0.5),
        plot.caption = element_text(family = "Roboto Condensed",
                                    size = 14, hjust = 0),
        text = element_markdown(family = "Roboto Condensed"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  labs(title = glue::glue("
                          <b style ='color:{home_stats$home_team_color}'>{home_stats$TEAMS}</b> | Shots: {home_stats$SHOTS} | On Target: {home_stats$`ON-TARGET`} | xG per Shot: {round(home_stats$xG/home_stats$SHOTS, 2)}<br>
                          <b style='color:{away_stats$away_team_color}'>{away_stats$TEAMS}</b> | Shots: {away_stats$SHOTS} | On Target: {away_stats$`ON-TARGET`} | xG per Shot: {round(away_stats$xG/away_stats$SHOTS, 2)}"),
       caption = " @R_by_Ryo                                                                                                                                                             Data: understat.com") +
  geom_point(aes(size = xG, 
                 color = result),
             stroke = 1.5) +
  geom_point(data = match_shots_data_clean %>% 
      filter(result == "Goal"),
      aes(color = result,
          size = xG), 
      stroke = 1.5,
      show.legend = FALSE) +
  scale_size(guide = FALSE) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_color_manual(values = fill_cols2, name = "Result",
                    guide = guide_legend(override.aes = list(size = 4.55))) +
  ## Home
  geom_rich_text(x = 440.25, y = 50, 
            color = "grey20", size = 8,
            fill = NA, label.color = NA,
            label = glue::glue("<b style ='color:{home_stats$home_team_color}'>{home_stats$TEAMS}</b>: {home_stats$xG} xG")) +
  ## Away
  geom_rich_text(x = 146.75, y = 50, 
            color = "grey20", size = 8,
            fill = NA, label.color = NA,
            label = glue::glue("<b style ='color:{away_stats$away_team_color}'>{away_stats$TEAMS}</b>: {away_stats$xG} xG"))

shotxG_map


## ---- fig.width=800, fig.height = 10-------------------------------------
png(filename = here::here("Premier League 2019-2020/output/livnew_shotresult_plot.png"), 
    width = 1600, height = 2000, res = 144, bg = "white")

grid.newpage()
grid.arrange(cumsum_xG_plot, shotxG_map)
dev.off()


## ------------------------------------------------------------------------
match_squad_data <- get_data_element(match_data, "rostersData")

#msd <- jsonlite::stream_in(match_shots_data)

match_squad_data <- fix_json(match_squad_data)

# Home: Liverpool
liv_squad_data <- fromJSON(match_squad_data[1])

## add 'team_name' with home team name from 'h_team' var

# Away: Arsenal
ars_squad_data <- fromJSON(match_squad_data[2])

## add 'team_name' with away team name from 'a_team' var


## ------------------------------------------------------------------------
team_name <- "Liverpool"
season <- 2018
season_url <- stringr::str_glue("{home_url}/team/{team_name}/{season}")

season_url %>% 
  read_html() %>% 
  html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "match-info", " " ))]')


  html_nodes("match-info") %>% 
  html_attr("href")



## ------------------------------------------------------------------------
liv <- get_team_meta("Liverpool")

liv


## ------------------------------------------------------------------------
season_url %>% 
  read_html() %>% 
  html_nodes("div.calendar-date-container")


season_url %>% 
  read_html() %>% 
  html_nodes("a")

"div.calendar-date-container:nth-child(5) > div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"


"div.calendar-date-container:nth-child(5) > div:nth-child(2) > div:nth-child(1) > a:nth-child(1)"


## ------------------------------------------------------------------------
season_url %>% 
  read_html() %>% 
  print(width = 50000000)

