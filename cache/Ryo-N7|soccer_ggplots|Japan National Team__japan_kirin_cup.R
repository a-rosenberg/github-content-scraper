## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----message=FALSE-------------------------------------------------------
pacman::p_load(dplyr, tidyr, forcats, purrr, ggplot2,
               stringr, readr,
               scales, lubridate, ggrepel, stringi, magick, 
               glue, extrafont, rvest, ggtextures, cowplot, ggimage, polite)


## ------------------------------------------------------------------------
federation_files <- Sys.glob("../data/federation_affiliations/*")

df_federations = data.frame(country = NULL, federation = NULL)
for (f in federation_files) {
    federation = basename(f)
    content = read.csv(f, header=FALSE)
    content <- cbind(content,federation=rep(federation, dim(content)[1]))
    df_federations <- rbind(df_federations, content)
}

colnames(df_federations) <- c("country", "federation")

df_federations <- df_federations %>% 
  mutate(country = as.character(country) %>% str_trim(side = "both"))


## ---- message=FALSE------------------------------------------------------
results_raw <- read_csv("../data/results.csv")

results_japan_raw <- results_raw %>% 
  filter(home_team == "Japan" | away_team == "Japan") %>% 
  rename(venue_country = country, 
         venue_city = city) %>% 
  mutate(match_num = row_number())

# combine with federation affiliations
results_japan_home <- results_japan_raw %>% 
  left_join(df_federations, 
            by = c("home_team" = "country")) %>% 
  mutate(federation = as.character(federation)) %>% 
  rename(home_federation = federation) 

results_japan_away <- results_japan_raw %>% 
  left_join(df_federations, 
            by = c("away_team" = "country")) %>% 
  mutate(federation = as.character(federation)) %>% 
  rename(away_federation = federation)

# combine home-away
results_japan_cleaned <- results_japan_home %>% 
  full_join(results_japan_away)


## ------------------------------------------------------------------------
results_japan_cleaned <- results_japan_cleaned %>% 
  mutate(
    home_federation = case_when(
      home_team %in% c(
        "China", "Manchukuo", "Burma", "Korea Republic", "Vietnam Republic",
        "Korea DPR", "Brunei") ~ "AFC",
      home_team == "USA" ~ "Concacaf",
      home_team == "Bosnia-Herzegovina" ~ "UEFA",
      TRUE ~ home_federation),
    away_federation = case_when(
      away_team %in% c(
        "China", "Manchukuo", "Burma", "Korea Republic", "Vietnam Republic",
        "Korea DPR", "Brunei", "Taiwan") ~ "AFC",
      away_team == "USA" ~ "Concacaf",
      away_team == "Bosnia-Herzegovina" ~ "UEFA",
      TRUE ~ away_federation
    ))


## ------------------------------------------------------------------------
results_jp_asia <- results_japan_cleaned %>% 
  # filter only for Japan games and AFC opponents
  filter(home_team == "Japan" | away_team == "Japan",
         #home_federation == "AFC" & away_federation == "AFC"
         ) %>% 
  select(-contains("federation"), -contains("venue"),
         -neutral, -match_num,
         date, home_team, home_score, away_team, away_score, tournament) %>% 
  # reshape columns to Japan vs. opponent
  mutate(
    opponent = case_when(
      away_team != "Japan" ~ away_team,
      home_team != "Japan" ~ home_team),
    home_away = case_when(
      home_team == "Japan" ~ "home",
      away_team == "Japan" ~ "away"),
    japan_goals = case_when(
      home_team == "Japan" ~ home_score,
      away_team == "Japan" ~ away_score),
    opp_goals = case_when(
      home_team != "Japan" ~ home_score,
      away_team != "Japan" ~ away_score)) %>% 
  # label results from Japan's perspective
  mutate(
    result = case_when(
      japan_goals > opp_goals ~ "Win",
      japan_goals < opp_goals ~ "Loss",
      japan_goals == opp_goals ~ "Draw")) %>% 
  select(-contains("score"), -contains("team"))


## ------------------------------------------------------------------------
glimpse(results_jp_asia)


## ------------------------------------------------------------------------
results_jp_asia %>% filter(opponent == "Trinidad and Tobago") %>% knitr::kable()


## ------------------------------------------------------------------------
results_jp_asia %>% 
  filter(opponent == "Burma") %>% 
  knitr::kable()

