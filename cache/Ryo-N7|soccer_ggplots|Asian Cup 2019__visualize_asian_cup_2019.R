## ----setup, include=FALSE, warning=FALSE, message=FALSE------------------
knitr::opts_chunk$set(echo = TRUE)


## ----message=FALSE-------------------------------------------------------
pacman::p_load(tidyverse, scales, lubridate, ggrepel, stringi, magick, 
               glue, extrafont, rvest, ggtextures, cowplot, ggimage, polite)
# Roboto Condensed font (from hrbrmstrthemes or just Google it)
loadfonts()


## ------------------------------------------------------------------------
topg_url <- "https://en.wikipedia.org/wiki/AFC_Asian_Cup_records_and_statistics"

session <- bow(topg_url)

ac_top_scorers <- scrape(session) %>%
  html_nodes("table.wikitable:nth-child(29)") %>% ## 6/22/2019: it's (36) now and the players have changed...
  html_table() %>% 
  flatten_df() %>% 
  select(-Ref.) %>% 
  set_names(c("total_goals", "player", "country"))


## ------------------------------------------------------------------------
ac_top_scorers <- ac_top_scorers %>% 
  head(5) %>% 
  mutate(image = "https://www.emoji.co.uk/files/microsoft-emojis/activity-windows10/8356-soccer-ball.png")


## ----top goal scorers plot, fig.width=8, fig.height=6--------------------
ac_top_graph <- ac_top_scorers %>% 
  ggplot(aes(x = reorder(player, total_goals), y = total_goals,
             image = image)) +
  geom_isotype_col(img_width = grid::unit(1, "native"), img_height = NULL,
    ncol = NA, nrow = 1, hjust = 0, vjust = 0.5) +
  coord_flip() +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14),
                     expand = c(0, 0), 
                     limits = c(0, 15)) +
  ggthemes::theme_solarized() +
  labs(title = "Top Scorers of the Asian Cup",
       subtitle = "Most goals in a single tournament: 8 (Ali Daei, 1996)",
       y = "Number of Goals", x = NULL,
       caption = glue("
                      Source: Wikipedia
                      By @R_by_Ryo")) +
  theme(text = element_text(family = "Roboto Condensed"),
        plot.title = element_text(size = 22),
        plot.subtitle = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.line.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.y = element_blank())

ac_top_graph


## ----draw_image, fig.width=8, fig.height=6, fig.align='center'-----------
axis_image <- axis_canvas(ac_top_graph, axis = 'y') + 
  draw_image("https://upload.wikimedia.org/wikipedia/commons/c/ca/Flag_of_Iran.svg", 
             y = 13, scale = 1.5) +
  draw_image("https://upload.wikimedia.org/wikipedia/commons/0/09/Flag_of_South_Korea.svg", 
             y = 10, scale = 1.7) +
  draw_image("https://upload.wikimedia.org/wikipedia/en/9/9e/Flag_of_Japan.svg", 
             y = 7, scale = 1.7) +
  draw_image("https://upload.wikimedia.org/wikipedia/commons/f/f6/Flag_of_Iraq.svg", 
             y = 4, scale = 1.6) +
  draw_image("https://upload.wikimedia.org/wikipedia/commons/a/aa/Flag_of_Kuwait.svg", 
             y = 1, scale = 1.2)

ggdraw(insert_yaxis_grob(ac_top_graph, axis_image, position = "left"))


## ------------------------------------------------------------------------
acup_url <- "https://en.wikipedia.org/wiki/AFC_Asian_Cup"

session <- bow(acup_url)

acup_winners_raw <- scrape(session) %>% 
  html_nodes("table:nth-child(31)") %>% 
  html_table() %>% 
  flatten_df()


## ----clean winners df, warning=FALSE-------------------------------------
acup_winners_clean <- acup_winners_raw %>% 
  janitor::clean_names() %>% 
  slice(1:8) %>% 
  select(-fourth_place, -semi_finalists, -total_top_four) %>% 
  separate(winners, into = c("Champions", "first_place_year"), 
           sep = " ", extra = "merge") %>% 
  separate(runners_up, into = c("Runners-up", "second_place_year"), 
           sep = " ", extra = "merge") %>% 
  separate(third_place, into = c("Third Place", "third_place_year"), 
           sep = " ", extra = "merge") %>% 
  mutate_all(funs(str_replace_all(., "–", "0"))) %>% 
  mutate_at(vars(contains("num")), funs(as.numeric)) %>% 
  mutate(team = if_else(team == "Israel1", "Israel", team)) %>% 
  gather(key = "key", value = "value", -team, 
         -first_place_year, -second_place_year, -third_place_year) %>% 
  mutate(key = key %>% 
           fct_relevel(c("Champions", "Runners-up", "Third Place"))) %>% 
  arrange(key, value) %>% 
  mutate(team = as_factor(team),
         order = row_number())


## ---- fig.width=8, fig.height=6, fig.align='center'----------------------
acup_winners_clean %>% 
  ggplot(aes(value, team, color = key)) +
  geom_point(size = 5) +
  scale_color_manual(values = c("Champions" = "#FFCC33",
                                "Runners-up" = "#999999",
                                "Third Place" = "#CC6600"),
                     guide = FALSE) +
  labs(x = "Number of Occurrence",
       title = "Winners & Losers of the Asian Cup!",
       subtitle = glue("
                       Ordered by number of Asian Cup(s) won.
                       Four-time Champions, Japan, only won their first in 1992!"),
       caption = glue("
                      Note: Israel was expelled by the AFC in 1974 while Australia joined the AFC in 2006.
                      Source: Wikipedia
                      By @R_by_Ryo")) +
  facet_wrap(~key) +
  theme_minimal() +
  theme(text = element_text(family = "Roboto Condensed"),
        title = element_text(size = 18),
        plot.subtitle = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        plot.caption = element_text(hjust = 0, size = 10),
        panel.border = element_rect(fill = NA, colour = "grey20"),
        panel.grid.minor.x = element_blank(),
        strip.text = element_text(size = 16)) 


## ----GPG base links------------------------------------------------------
wiki_url <- "https://en.wikipedia.org"
session <- bow(wiki_url)
acup_url <- "https://en.wikipedia.org/wiki/AFC_Asian_Cup"
session_cup <- bow(acup_url)

cup_links <- scrape(session_cup) %>% 
  html_nodes("br+ i a") %>% 
  html_attr("href") %>% 
  magrittr::extract(-17:-18)

acup_df <- cup_links %>% 
  as_data_frame() %>% 
  mutate(cup = str_remove(value, "\\/wiki\\/") %>% str_replace_all("_", " ")) %>% 
  rename(link = value)


## ----goal info functions, warning=FALSE----------------------------------
goals_info <- function(x) {
  goal_info <- session %>% 
    jump_to(x) %>% 
    html_nodes(".vcalendar") %>% 
    html_table(header = FALSE) %>% 
    flatten_df() %>% 
    spread(key = X1, value = X2) %>% 
    select(`Goals scored`) %>% 
    mutate(`Goals scored` = str_remove_all(`Goals scored`, pattern = ".*\\(") %>% 
             str_extract_all("\\d+\\.*\\d*") %>% as.numeric)
}

team_num_info <- function(x) {
  team_num_info <- session %>% 
    jump_to(x) %>% 
    html_nodes(".vcalendar") %>% 
    html_table(header = FALSE) %>% 
    flatten_df() %>% 
    spread(key = X1, value = X2) %>% 
    select(`Teams`) %>% 
    mutate(`Teams` = as.numeric(`Teams`))
}

match_num_info <- function(x) {
  match_num_info <- session %>% 
    jump_to(x) %>% 
    html_nodes(".vcalendar") %>% 
    html_table(header = FALSE) %>% 
    flatten_df() %>% 
    spread(key = X1, value = X2) %>% 
    janitor::clean_names() %>% 
    select(matches_played) %>% 
    mutate(matches_played = as.numeric(matches_played))
}

# all together:
goals_data <- acup_df %>% 
  mutate(goals_per_game = map(acup_df$link, goals_info) %>% unlist,
         team_num = map(acup_df$link, team_num_info) %>% unlist,
         match_num = map(acup_df$link, match_num_info) %>% unlist)


## ----clean up------------------------------------------------------------
ac_goals_df <- goals_data %>% 
  mutate(label = cup %>% str_extract("[0-9]+") %>% str_replace("..", "'"),
         team_num = case_when(
           is.na(team_num) ~ 16,
           TRUE ~ team_num
         )) %>% 
  arrange(cup) %>% 
  mutate(label = factor(label, label),
         team_num = c(4, 4, 4, 5, 6, 6, 10, 10, 10, 8, 12, 12, 16, 16, 16, 16))

glimpse(ac_goals_df)


## ---- fig.align='center'-------------------------------------------------
plot <- ac_goals_df %>% 
  ggplot(aes(x = label, y = goals_per_game, group = 1)) +
  geom_line() +
  scale_y_continuous(limits = c(NA, 5.35),
                     breaks = c(1.5, 2, 2.5, 3, 3.5, 4, 4.5)) +
  labs(x = "Tournament (Year)", y = "Goals per Game") +
  theme_minimal() +
  theme(text = element_text(family = "Roboto Condensed"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12)) +
  annotate(geom = "label", x = "'56", y = 5.15, family = "Roboto Condensed",
           color = "black", 
           label = "Total Number of Games Played:", hjust = 0) +
  annotate(geom = "text", x = "'60", y = 4.9, 
           label = "6", family = "Roboto Condensed") +
  annotate(geom = "segment", x = 1, xend = 3, y = 4.8, yend = 4.8) +
  annotate(geom = "text", x = "'68", y = 4.9, 
           label = "10", family = "Roboto Condensed") +
  annotate(geom = "segment", x = 3.8, xend = 4.2, y = 4.8, yend = 4.8) +
  annotate(geom = "text", x = "'72", y = 4.9, 
           label = "13", family = "Roboto Condensed") +
  annotate(geom = "segment", x = 4.8, xend = 5.2, y = 4.8, yend = 4.8) +
  annotate(geom = "text", x = "'76", y = 4.9, 
           label = "10", family = "Roboto Condensed") +
  annotate(geom = "segment", x = 5.8, xend = 6.2, y = 4.8, yend = 4.8) +
  annotate(geom = "text", x = "'84", y = 4.9, 
           label = "24", family = "Roboto Condensed") +
  annotate(geom = "segment", x = 7, xend = 9, y = 4.8, yend = 4.8) +
  annotate(geom = "text", x = "'92", y = 4.9, 
           label = "16", family = "Roboto Condensed") +
  annotate(geom = "segment", x = 9.8, xend = 10.2, y = 4.8, yend = 4.8) +
  annotate(geom = "text", x = 11.5, y = 4.9, 
           label = "26", family = "Roboto Condensed") +
  annotate(geom = "segment", x = 11, xend = 12, y = 4.8, yend = 4.8) +
  annotate(geom = "text", x = 14.5, y = 4.9, 
           label = "32", family = "Roboto Condensed") +
  annotate(geom = "segment", x = 13, xend = 16, y = 4.8, yend = 4.8) +
  annotate(geom = "text", x = 9, y = 4, family = "Roboto Condensed",
           label = glue("
                        Incredibly low amount of goals in Group B
                        (15 in 10 Games) and in Knock-Out Stages
                        (4 goals in 4, only 1 scored in normal time)")) +
  annotate(geom = "segment", x = 9, xend = 9, y = 1.65, yend = 3.75,
           color = "red") +
  ggimage::geom_emoji(aes(image = '26bd'), size = 0.03) 

plot
ggsave(filename = paste0(here::here("Asian Cup 2019"), "/gpg_plot.png"), 
       width = 8, height = 7, dpi = 300)
plot <- image_read(paste0(here::here("Asian Cup 2019"), "/gpg_plot.png"))


## ---- fig.align='center'-------------------------------------------------
logo_raw <- image_read("https://upload.wikimedia.org/wikipedia/en/a/ad/2019_afc_asian_cup_logo.png")

logo_proc <- logo_raw %>% image_scale("600")

# create blank canvas
a <- image_blank(width = 1000, height = 100, color = "white")
# combine with logo image and shift logo to the right
b <- image_composite(image_scale(a, "x100"), image_scale(logo_proc, "x75"), 
                     offset = "+880+25")
# add in the title text
logo_header <- b %>% 
  image_annotate(text = glue("Goals per Game Throughout the History of the Asian Cup"),
                 color = "black", size = 24, font = "Roboto Condensed",
                 location = "+63+50", gravity = "northwest")

# combine it all together! 
final2_plot <- image_append(image_scale(c(logo_header, plot), "1000"), stack = TRUE)

# image_write(final2_plot,
#             glue("{here::here('Asian Cup 2019')}/gpg_plot_final.png"))

final2_plot


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
         home_federation == "AFC" & away_federation == "AFC") %>% 
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
      japan_goals == opp_goals ~ "Draw"),
    result = result %>% as_factor() %>% fct_relevel(c("Win", "Draw", "Loss"))) %>% 
  select(-contains("score"), -contains("team"))


## ------------------------------------------------------------------------
results_jp_asia %>% 
  filter(opponent == "Jordan",
         tournament == "AFC Asian Cup")


## ------------------------------------------------------------------------
japan_versus <- function(data, ...) {
  # filter 
  filter_vars <- enquos(...)
  
  jp_vs <- data %>% 
    filter(!!!filter_vars) %>% 
    # count results type per opponent
    group_by(result, opponent) %>% 
    mutate(n = n()) %>% 
    ungroup() %>% 
    # sum amount of goals by Japan and opponent
    group_by(result, opponent) %>% 
    summarize(j_g = sum(japan_goals),
              o_g = sum(opp_goals),
              n = n()) %>% 
    ungroup() %>% 
    # spread results over multiple columns
    spread(result, n) %>% 
    # 1. failsafe against no type of result against an opponent
    # 2. sum up counts per opponent
    group_by(opponent) %>% 
    mutate(Win = if("Win" %in% names(.)){return(Win)} else{return(0)},
         Draw = if("Draw" %in% names(.)){return(Draw)} else{return(0)},
         Loss = if("Loss" %in% names(.)){return(Loss)} else{return(0)}) %>% 
    summarize(Win = sum(Win, na.rm = TRUE),
              Draw = sum(Draw, na.rm = TRUE),
              Loss = sum(Loss, na.rm = TRUE),
              `Goals For` = sum(j_g),
              `Goals Against` = sum(o_g))
  
  return(jp_vs)
}


## ------------------------------------------------------------------------
japan_versus(data = results_jp_asia, 
             opponent == "China")


## ------------------------------------------------------------------------
japan_versus(data = results_jp_asia,
             home_away == "home",
             opponent %in% c("Palestine", "Vietnam", "India"))


## ---- fig.align='center'-------------------------------------------------
results_jp_asia %>% 
  japan_versus(opponent %in% c("Iran", "Korea Republic", "Saudi Arabia"),
               tournament == "AFC Asian Cup") %>% 
  knitr::kable(format = "html",
               caption = "Japan vs. Historic Rivals in the Asian Cup") %>% 
  kableExtra::kable_styling(full_width = FALSE) %>% 
  kableExtra::add_header_above(c(" ", "Result" = 3, "Goals" = 2))


## ------------------------------------------------------------------------
results_jp_asia %>% 
  japan_versus(opponent %in% c("Oman", "Uzbekistan", "Turkmenistan")) %>% 
  knitr::kable(format = "html",
               caption = "Japan's Record vs. Group F Teams") %>% 
  kableExtra::kable_styling(full_width = FALSE) %>% 
  kableExtra::add_header_above(c(" ", "Result" = 3, "Goals" = 2))

