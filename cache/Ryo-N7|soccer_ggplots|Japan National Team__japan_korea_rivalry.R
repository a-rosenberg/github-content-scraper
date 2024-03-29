## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----message=FALSE-------------------------------------------------------
library(polite)
library(rvest)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(extrafont)
loadfonts()


## ------------------------------------------------------------------------
url <- "https://en.wikipedia.org/wiki/Japan%E2%80%93South_Korea_football_rivalry"

session <- bow(url)

jpn_kor_df_raw <- scrape(session) %>% 
  html_nodes(xpath = "//*[@id='mw-content-text']/div/table[3]") %>% 
  .[[1]] %>% 
  html_table()


## ------------------------------------------------------------------------
jpn_kor_df_raw %>% glimpse()


## ------------------------------------------------------------------------
jpn_kor_df_clean <- jpn_kor_df_raw %>% 
  janitor::clean_names() %>% 
  mutate(pen = score %>% 
           iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>% 
           str_extract("\\([^()]+\\)"),
         pen = gsub("[(PK)]", "", pen),
         score = score %>% 
           iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>% 
           str_remove("\\(.*\\)"),
         date = dmy(date)) %>% 
  separate("score", into = c("home_score", "away_score"), sep = "-") %>%
  separate("pen", into = c("home_score_pen", "away_score_pen"), sep = "-") %>% 
  mutate_at(vars(contains("score")), as.numeric)

jpn_kor_df_clean <- jpn_kor_df_clean %>% 
  mutate(date = replace(date, number == 53, "1993-10-25"))



## ------------------------------------------------------------------------
jpn_kor_test <- jpn_kor_df_clean %>% select(date, home_team, home_score, away_team, away_score)

jpn_kor_test <- jpn_kor_test %>% 
  unite("home", c("home_team", "home_score")) %>% 
  unite("away", c("away_team", "away_score")) %>% 
  gather(key = team_type, value = team, -date) %>% 
  mutate(goal = str_extract(team, "[^_]+$")) %>% 
  mutate(team = str_remove(team, "_[0-9]")) %>% 
  arrange(desc(date))
  
  #separate(value, into = c("team", "goals"), extra = "merge")

jpn_kor_test %>% 
  filter(date > "2000-01-01") %>% 
  ggplot(aes(date, goal)) +
  geom_point(aes(group = date, color = team)) +
  geom_path(aes(group = date, x = date, y = goal)) +
  theme_minimal()
  #scale_x_continuous(breaks = 10) +
  scale_x_date(limits = c(as.Date("2000-01-01"), as.Date("2018-01-01")),
               date_breaks = "1 day")


## ------------------------------------------------------------------------
jpn_kor_fix <- jpn_kor_test %>% 
  mutate(num = as.integer(factor(date)))

jpn_kor_fix %>% 
  ggplot(aes(num, goal)) +
  geom_point(aes(group = num, color = team)) +
  geom_path(aes(group = num, x = num, y = goal)) +
  scale_x_continuous(breaks = 10)
  scale_x_date(limits = c(as.Date("1954-01-01"), as.Date("1955-01-01")),
               date_breaks = "1 day")
  


## ---- fig.width=10, fig.height=7, fig.showtext=TRUE----------------------
jpn_kor_df_clean %>% 
  mutate(winner = case_when(
    home_score > away_score & home_team == "Japan" ~ "Japan",
    away_score > home_score & away_team == "Japan" ~ "Japan",
    home_score > away_score & home_team == "South Korea" ~ "South Korea",
    away_score > home_score & away_team == "South Korea" ~ "South Korea",
    home_score == away_score & home_score_pen > away_score_pen & home_team == "Japan" ~ 
      "Japan",
    home_score == away_score & home_score_pen < away_score_pen & away_team == "Japan" ~ 
      "Japan",
    home_score == away_score & home_score_pen > away_score_pen & home_team == "South Korea" ~ 
      "South Korea",
    home_score == away_score & home_score_pen < away_score_pen & away_team == "South Korea" ~ 
      "South Korea",
    TRUE ~ "Draw"
  )) %>% 
  mutate(pen = case_when(
    home_score == away_score & home_score_pen > away_score_pen & home_team == "Japan" ~ 
      "Pen: Japan",
    home_score == away_score & home_score_pen < away_score_pen & away_team == "Japan" ~ 
      "Pen: Japan",
    home_score == away_score & home_score_pen > away_score_pen & home_team == "South Korea" ~ 
      "Pen: South Korea",
    home_score == away_score & home_score_pen < away_score_pen & away_team == "South Korea" ~ 
      "Pen: South Korea",
    TRUE ~ "non-PK"
  )) %>% 
  select(date, home_team, home_score, away_team, away_score, winner, pen) %>% 
  unite("home", c("home_team", "home_score")) %>% 
  unite("away", c("away_team", "away_score")) %>% 
  gather(key = team_type, value = team, -date, - winner, -pen) %>% 
  mutate(goal = str_extract(team, "[^_]+$") %>% as.numeric()) %>% 
  mutate(team = str_remove(team, "_[0-9]")) %>% 
  arrange(desc(date)) -> jpn_kor_final
  


## ----fig.width=10, fig.height=7, fig.showtext=TRUE-----------------------
line_vals <- c("Japan" = "blue", "South Korea" = "red",
               "Draw" = "grey")

breaks <- as.Date(c("2000-01-01", "2005-01-01", "2010-01-01", "2015-01-01", "2018-01-01"))
labs <- c("2000", "2005", "2010", "2015", "2018")

flag_data <- data.frame(
  x = c(as.Date("2002-07-06"), as.Date("2005-03-12")),
  y = c(4.7, 4.7),
  team = c("japan", "south korea")
) %>% 
  mutate(image = team %>% countrycode::countrycode(., origin = "country.name", 
                                                   destination = "iso2c")) %>% 
  select(-team)

# PLOT

jpn_kor_final %>% 
  filter(date > "2000-01-01") %>% 
  ggplot() +
  geom_path(aes(group = date, x = date, y = goal, color = winner), 
            size = 1.5,
            show.legend = TRUE) +
  scale_color_manual(name = "", 
                     values = line_vals) +
  # Japan
  geom_point(data = jpn_kor_final %>% 
               filter(date > "2000-01-01", team == "Japan", 
                      winner == "Japan" | winner == "South Korea",
                      pen == "non-PK"), 
             aes(x = date, y = goal, group = date), 
             color = "blue", size = 4) +
  # Korea
  geom_point(data = jpn_kor_final %>% 
               filter(date > "2000-01-01", team == "South Korea", 
                      winner == "Japan" | winner == "South Korea",
                      pen == "non-PK"),
             aes(x = date, y = goal, group = date), 
             color = "red", size = 4) +
  geom_segment(aes(x = as.Date("2007-07-28"), xend = as.Date("2007-08-30"),
                   y = 0, yend = 0.2)) +
  geom_segment(aes(x = as.Date("2011-01-25"), xend = as.Date("2011-03-10"),
                   y = 2, yend = 2.2)) +
  # Draw
  geom_point(data = jpn_kor_final %>% 
               filter(date > "2000-01-01", winner == "Draw"),
             aes(x = date, y = goal, group = date), 
             color = "grey", size = 4.5, shape = 17) +
  geom_point(data = jpn_kor_final %>% 
               filter(date > "2000-01-01", pen == "Pen: Japan"),
             aes(x = date, y = goal, group = date), 
             color = "blue", size = 4.5, shape = 17) +
  geom_point(data = jpn_kor_final %>% 
               filter(date > "2000-01-01", pen == "Pen: South Korea"),
             aes(x = date, y = goal, group = date), 
             color = "red", size = 4.5, shape = 17) +
  scale_x_date(limits = c(as.Date("2000-01-01"), as.Date("2018-09-01")),
               breaks = breaks, labels = labs,
               expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0),
                   breaks = scales::pretty_breaks(4),
                   limits = c(-0.25, 4.95)) +
  labs(title = "Japan vs. South Korea: The Eternal East Asian Rivalry in the 21st Century",
       x = NULL, y = "Goals Scored",
       caption = glue::glue("
                            By @R_by_Ryo
                            Inspired by @tinafrank & @metropop_eu
                            Source: https://en.wikipedia.org/wiki/Japan-South_Korea_football_rivalry")) +
  # Custom annotations:
  annotate(geom = "label",
           x = as.Date("2007-07-28"), 
           y = 0.25,
           label = "2007 Asian Cup: 3rd Place Match (PK: 6-5)", 
           hjust = 0, 
           family = "Roboto Condensed") +
  annotate(geom = "label",
           x = as.Date("2011-01-25"), 
           y = 2.3,
           label = "2011 Asian Cup: Semi-Final (PK: 3-0)", 
           family = "Roboto Condensed",
           hjust = 0) +
  annotate(geom = "text",
           x = as.Date("2000-01-01"),
           y = 4.7,
           label = glue::glue(
         "16 Games:            5 Wins,             6 Wins"),
         hjust = 0, size = 6,
         family = "Roboto Condensed") +
  ggimage::geom_flag(data = flag_data,
                     aes(x = x, y = y,
                         image = image),       
                     size = c(0.06, 0.06)) +
  theme_minimal() +
  theme(text = element_text(family = "Roboto Condensed"),
        title = element_text(size = 18),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.title = element_blank(),
        legend.position = c(0.45, 0.93),
        legend.box.background = element_rect(color = "black"))
  


## ----fig.width=20, fig.height=10-----------------------------------------
line_vals <- c("Japan" = "blue", "South Korea" = "red",
               "Draw" = "grey")

breaks_all <- as.Date(c("1950-01-01", "1955-01-01", "1960-01-01", "1965-01-01", "1970-01-01",
                    "1975-01-01", "1980-01-01", "1985-01-01", "1990-01-01", "1995-01-01",
                    "2000-01-01", "2005-01-01", "2010-01-01", "2015-01-01", "2018-01-01"))
labs_all <- c("1950", "1955", "1960", "1965", "1970", "1975",
          "1980", "1985", "1990", "1995", "2000", "2005", 
          "2010", "2015", "2018")

flag_data_all <- data.frame(
  x = c(as.Date("1955-07-06"), as.Date("1962-12-12")),
  y = c(4.7, 4.7),
  team = c("japan", "south korea")
) %>% 
  mutate(image = team %>% countrycode::countrycode(., origin = "country.name", 
                                                   destination = "iso2c")) %>% 
  select(-team)


## ALL TIME
jpn_kor_final %>% 
  ggplot(aes(x = date, y = goal)) +
  geom_path(aes(group = date, x = date, y = goal, 
                color = winner), 
            size = 1.5,
            show.legend = TRUE) +
  # Japan
  geom_point(data = jpn_kor_final %>% 
               filter(team == "Japan", 
                      winner == "Japan" | winner == "South Korea",
                      pen == "non-PK"), 
             aes(x = date, y = goal, group = date), 
             color = "blue", size = 4) +
  # Korea
  geom_point(data = jpn_kor_final %>% 
               filter(team == "South Korea", 
                      winner == "Japan" | winner == "South Korea",
                      pen == "non-PK"),
             aes(x = date, y = goal, group = date), 
             color = "red", size = 4) +
  geom_segment(aes(x = as.Date("1992-08-29"), xend = as.Date("1992-09-30"),
                   y = 2, yend = 2.2)) +
  geom_segment(aes(x = as.Date("1995-02-26"), xend = as.Date("1995-05-11"),
                   y = 2, yend = 2.6)) +
  geom_segment(aes(x = as.Date("2007-07-28"), xend = as.Date("2007-08-30"),
                   y = 0, yend = 0.2)) +
  geom_segment(aes(x = as.Date("2011-01-25"), xend = as.Date("2011-03-10"),
                   y = 2, yend = 2.2)) +
  # Draw
  geom_point(data = jpn_kor_final %>% 
               filter(winner == "Draw"),
             aes(x = date, y = goal, group = date), 
             color = "grey", size = 4.5, shape = 17) +
  geom_point(data = jpn_kor_final %>% 
               filter(pen == "Pen: Japan"),
             aes(x = date, y = goal, group = date), 
             color = "blue", size = 4.5, shape = 17) +
  geom_point(data = jpn_kor_final %>% 
               filter(pen == "Pen: South Korea"),
             aes(x = date, y = goal, group = date), 
             color = "red", size = 4.5, shape = 17) +
  # Scales
  scale_x_date(limits = c(as.Date("1950-01-01"), as.Date("2019-08-01")),
               breaks = breaks_all, labels = labs_all,
               expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0),
                   breaks = scales::pretty_breaks(4),
                   limits = c(-0.25, 4.95)) +
  scale_color_manual(name = "", 
                     values = line_vals) +
  # scale_shape_manual(values = c(17, 16, 16)) +
  # scale_fill_manual(name = "", 
  #                    values = line_vals) +
  labs(title = "Japan vs. South Korea: The Eternal East Asian Rivalry",
       x = NULL, y = "Goals Scored",
       caption = glue::glue("
                            By @R_by_Ryo
                            Inspired by @tinafrank & @metropop_eu
                            Source: https://en.wikipedia.org/wiki/Japan-South_Korea_football_rivalry")) +
  # Custom annotations:
  annotate(geom = "label",
           x = as.Date("1992-08-29"), 
           y = 2.2,
           label = "1992 Dynasty Cup: Final (PK: 4-2)", 
           family = "Roboto Condensed",
           hjust = 0) +
  annotate(geom = "label",
           x = as.Date("1995-03-26"), 
           y = 2.6,
           label = "1995 Dynasty Cup: Final (PK: 5-3)", 
           family = "Roboto Condensed",
           hjust = 0) +
  annotate(geom = "label",
           x = as.Date("2007-07-28"), 
           y = 0.25,
           label = "2007 Asian Cup: 3rd Place Match (PK: 6-5)", 
           hjust = 0, 
           family = "Roboto Condensed") +
  annotate(geom = "label",
           x = as.Date("2011-01-25"), 
           y = 2.2,
           label = "2011 Asian Cup: Semi-Final (PK: 3-0)", 
           family = "Roboto Condensed",
           hjust = 0) +
  annotate(geom = "text",
           x = as.Date("1950-01-01"),
           y = 4.7,
           label = glue::glue(
         "78 Games:                      14 Wins,                       41 Wins"),
         hjust = 0, size = 6,
         family = "Roboto Condensed") +
  ggimage::geom_flag(data = flag_data_all,
                     aes(x = x, y = y,
                         image = image),
                     size = c(0.05, 0.05)) +
  theme_minimal() +
  theme(text = element_text(family = "Roboto Condensed"),
        title = element_text(size = 25),
        axis.text = element_text(size = 19),
        axis.title = element_text(size = 22),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        legend.position = c(0.3, 0.93),
        legend.box.background = element_rect(color = "black"))

