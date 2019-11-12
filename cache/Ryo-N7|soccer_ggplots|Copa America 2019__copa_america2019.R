## ----setup, include=FALSE, message=FALSE, warning=FALSE------------------
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=FALSE)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
pacman::p_load(tidyverse, polite, scales, ggimage, ggforce, ggtextures, DT, 
               cowplot, rvest, glue, extrafont, ggrepel, magick)
loadfonts()


## ----eval=FALSE, message=FALSE, warning=FALSE----------------------------
## library(dplyr)        ## data wrangling
## library(tidyr)        ## data wrangling
## library(purrr)        ## data wrangling and iteration
## library(stringr)      ## data wrangling
## library(rvest)        ## webscraping
## library(polite)       ## webscraping (Github only pkg)
## library(ggplot2)      ## plotting
## library(scales)       ## plotting scales
## library(ggimage)      ## images for flags
## library(ggforce)      ## plotting text labels
## library(cowplot)      ## plotting grid
## library(glue)         ## text
## library(ggrepel)      ## plotting text labels
## library(magick)       ## plotting
## library(DT)           ## tables
## library(ggtextures)   ## soccer ball emoji as geom_col()
## library(extrafont)    ## fonts: Roboto Condensed
## 
## loadfonts()


## ------------------------------------------------------------------------
theme_copaAmerica <- function(
  title.size = 24,
  subtitle.size = 14,
  caption.size = 8,
  axis.text.size = 14,
  axis.text.x.size = 12,
  axis.text.y.size = 12,
  axis.title.size = 16,
  strip.text.size = 18,
  panel.grid.major.x = element_line(size = 0.5, color = "white"),
  panel.grid.major.y = element_line(size = 0.5, color = "white"),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  axis.ticks = element_line(color = "white")) {
  ## Theme:
  theme(text = element_text(family = "Roboto Condensed", color = "white"),
        plot.title = element_text(family = "Roboto Condensed", face = "bold", 
                                  size = title.size, color = "yellow"),
        plot.subtitle = element_text(size = subtitle.size),
        plot.caption = element_text(size = caption.size),
        panel.background = element_rect(fill = "#009b3a"),
        plot.background = element_rect(fill = "#002776"),
        axis.text = element_text(size = axis.text.size, color = "white"),
        axis.text.x = element_text(size = axis.text.x.size, color = "white"),
        axis.text.y = element_text(size = axis.text.y.size, color = "white"),
        axis.title = element_text(size = axis.title.size),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        panel.grid.major.x = panel.grid.major.x,
        panel.grid.major.y = panel.grid.major.y,
        panel.grid.minor.x = panel.grid.minor.x,
        panel.grid.minor.y = panel.grid.minor.y,
        strip.text = element_text(color = "yellow", face = "bold", 
                                  size = strip.text.size, 
                                  margin = margin(4.4, 4.4, 4.4, 4.4)),
        strip.background = element_blank(),
        axis.ticks = axis.ticks
        )
}


## ---- eval=FALSE---------------------------------------------------------
## url <- "https://es.wikipedia.org/wiki/Anexo:Estad%C3%ADsticas_de_la_Copa_Am%C3%A9rica"
## 
## session <- bow(url)
## 
## copa_top_scorers <- scrape(session) %>%
##   html_nodes(".mw-parser-output > table:nth-child(95)") %>%
##   html_table() %>%
##   flatten_df() %>%
##   set_names(c("player", "country", "goals")) %>%
##   mutate(image = "https://www.emoji.co.uk/files/microsoft-emojis/activity-windows10/8356-soccer-ball.png")


## ---- eval=FALSE, echo=FALSE---------------------------------------------
## saveRDS(copa_top_scorers, file = here::here("data/copa_top_scorers.RDS"))


## ---- echo=FALSE---------------------------------------------------------
copa_top_scorers <- readRDS(file = here::here("data/copa_top_scorers.RDS"))


## ------------------------------------------------------------------------
glimpse(copa_top_scorers)


## ---- fig.height = 8, fig.width = 10-------------------------------------
copa_goleadores_raw_plot <- copa_top_scorers %>% 
  head(5) %>% 
  ggplot(aes(x = reorder(player, goals), y = goals,
             image = image)) +
  geom_isotype_col(img_width = grid::unit(1, "native"), img_height = NULL,
    ncol = NA, nrow = 1, hjust = 0, vjust = 0.5) +
  geom_text(aes(label = goals, family = "Roboto Condensed", fontface = "bold"), 
            size = 7.5, color = "yellow",
            nudge_y = 0.5) +
  coord_flip() +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18),
                     expand = c(0, 0), 
                     limits = c(0, 19)) +
  labs(title = "Top Scorers of the Copa América",
       subtitle = glue("
                       Most goals in a single tournament: 9 
                       Humberto Maschio (Argentina), Javier Ambrois (Uruguay), Jair (Brazil)"),
       y = "Number of Goals", x = NULL,
       caption = glue("
                      Source: Wikipedia
                      By @R_by_Ryo")) +
  theme_copaAmerica(title.size = 26,
                    subtitle.size = 16,
                    caption.size = 12,
                    axis.text.size = 18,
                    axis.title.size = 18,
                    panel.grid.major.y = element_blank(),
                    axis.ticks = element_blank())

## Add flags to y-axis:
axis_image <- axis_canvas(copa_goleadores_raw_plot, axis = 'y') + 
  draw_image("https://upload.wikimedia.org/wikipedia/en/0/05/Flag_of_Brazil.svg", 
             y = 16.5, scale = 1.8) +
  draw_image("https://upload.wikimedia.org/wikipedia/commons/1/1a/Flag_of_Argentina.svg", 
             y = 12.5, scale = 1.8) +
  draw_image("https://upload.wikimedia.org/wikipedia/commons/f/fe/Flag_of_Uruguay.svg", 
             y = 9, scale = 1.8) +
  draw_image("https://upload.wikimedia.org/wikipedia/commons/d/df/Flag_of_Peru_%28state%29.svg", 
             y = 5.25, scale = 1.8) +
  draw_image("https://upload.wikimedia.org/wikipedia/en/0/05/Flag_of_Brazil.svg", 
             y = 1.5, scale = 1.8)

copa_goleadores_plot <- ggdraw(insert_yaxis_grob(copa_goleadores_raw_plot, axis_image, position = "left"))
copa_goleadores_plot


## ---- eval=FALSE, echo=FALSE---------------------------------------------
## ggsave(plot = copa_goleadores_plot, filename = here::here("Copa America 2019/output/copa_goleadores_plot.png"),
##        height = 8, width = 10)


## ---- warning=FALSE, eval=FALSE------------------------------------------
## url <- "https://es.wikipedia.org/wiki/Anexo:Estad%C3%ADsticas_de_la_Copa_Am%C3%A9rica"
## 
## session <- bow(url)
## 
## copa_campeones <- scrape(session) %>%
##   html_nodes(".mw-parser-output > table:nth-child(10)") %>%
##   html_table() %>%
##   flatten_df()
## 
## copa_campeones_limpia <- copa_campeones %>%
##   janitor::clean_names() %>%
##   slice(1:8) %>%
##   select(1:4) %>%
##   set_names(c("team", "winners", "runners_up", "third_place")) %>%
##   separate(winners, into = c("Champions", "first_place_year"),
##            sep = " ", extra = "merge") %>%
##   separate(runners_up, into = c("Runners-up", "second_place_year"),
##            sep = " ", extra = "merge") %>%
##   separate(third_place, into = c("Third Place", "third_place_year"),
##            sep = " ", extra = "merge") %>%
##   mutate_all(list(~str_replace_all(., "–", "0"))) %>%
##   mutate_at(vars(contains("num")), funs(as.numeric)) %>%
##   gather(key = "key", value = "value", -team,
##          -first_place_year, -second_place_year, -third_place_year) %>%
##   mutate(key = as.factor(key),
##          value = as.numeric(value),
##          team = team %>% str_replace(., "[A-Z]{3}", "") %>% str_trim(.),
##          team = case_when(team == "Brasil" ~ "Brazil",
##                           TRUE ~ team)) %>%
##   mutate(key = forcats::fct_relevel(key,
##                                     "Champions",
##                                     "Runners-up",
##                                     "Third Place")) %>%
##   arrange(key, desc(value)) %>%
##   mutate(team = forcats::as_factor(team),
##          order = row_number())


## ---- eval=FALSE, echo=FALSE---------------------------------------------
## saveRDS(copa_campeones_limpia, file = here::here("data/copa_campeones_clean.RDS"))


## ---- echo=FALSE---------------------------------------------------------
copa_campeones_limpia <- readRDS(file = here::here("data/copa_campeones_clean.RDS"))


## ---- fig.height = 6, fig.width = 8--------------------------------------
copa_ganadores_plot <- copa_campeones_limpia %>% 
  ggplot(aes(value, forcats::fct_rev(team), color = key)) +
  geom_point(size = 10) +        # 10
  geom_text(aes(label = value), 
            size = 5, color = "black",    # 5 
            family = "Roboto Condensed", fontface = "bold") +
  scale_color_manual(values = c("Champions" = "#FFCC33",
                                "Runners-up" = "#999999",
                                "Third Place" = "#CC6600"),
                     guide = FALSE) +
  scale_x_continuous(breaks = c(1, 5, 10, 15),
                     labels = c(1, 5, 10, 15),
                     limits = c(-1, 16)) +
  labs(x = "Number of Occurrence", y = NULL,
       title = "Most Successful Teams of the Copa América!",
       subtitle = str_wrap("Ordered by number of Copa América(s) won. Argentina missed the chance to leapfrog Uruguay after consecutive final losses in the previous two tournaments!", width = 80),
       caption = glue("
                      Source: Wikipedia
                      By @R_by_Ryo")) +
  facet_wrap(~key) +
  theme_copaAmerica(subtitle.size = 14, 
                    caption.size = 10)

copa_ganadores_plot


## ---- eval=FALSE, echo=FALSE---------------------------------------------
## ggsave(filename = here::here("Copa America 2019/output/copa_ganadores_plot.png"),
##        height = 6, width = 8)


## ---- eval=FALSE, echo=FALSE---------------------------------------------
## squad_url <- "https://en.wikipedia.org/wiki/2019_Copa_Am%C3%A9rica_squads"
## 
## session <- bow(squad_url)
## 
## xpaths <- 1:12 %>%
##   map(., ~glue("//*[@id='mw-content-text']/div/table[{.x}]"))
## 
## squads_df_raw <- scrape(session) %>%
##   html_node(xpath = '//*[@id="toc"]') %>%
##   html_text() %>%
##   str_split("\n") %>%
##   unlist() %>%
##   tibble::enframe() %>%
##   rename(country = value) %>%
##   filter(str_detect(country, "^[1-8]\\."), !str_detect(country, "Group")) %>%
##   separate(country, c("group", "delete", "country"), sep = c(1, 3)) %>%
##   slice(1:12) %>%
##   mutate(group = LETTERS[as.numeric(group)],
##          country = str_trim(country),
##          xpaths = xpaths,
##          squads = map(xpaths, ~ scrape(session) %>%
##                         html_node(xpath = .x) %>%
##                         html_table())) %>%
##   unnest(squads) %>%
##   filter(Player != "") %>%
##   mutate(country_league = scrape(session) %>%
##            html_nodes(".nat-fs-player .thumbborder") %>%
##            html_attr("alt"))


## ---- eval=FALSE, echo=FALSE---------------------------------------------
## saveRDS(squads_df_raw, file = here::here("data/copa_america2019_squads_raw.RDS"))


## ---- echo=FALSE---------------------------------------------------------
squads_df_raw <- readRDS(file = here::here("data/copa_america2019_squads_raw.RDS"))


## ------------------------------------------------------------------------
squads_df_clean <- squads_df_raw %>% 
  janitor::clean_names() %>% 
  select(-delete, squad_num = no, 
         position = pos, birth_age = date_of_birth_age) %>% 
  mutate(position = position %>% str_replace_all(., "[1-9]", ""),
         birth_age = birth_age %>% str_extract_all(., pattern = "\\([^()]+\\)")) %>%   unnest(birth_age) %>% 
  group_by(player) %>% 
  mutate(colnum = seq_along(player)) %>% 
  spread(key = colnum, value = birth_age) %>% 
  ungroup() %>% 
  select(everything(), dob = `1`, age = `2`) %>% 
  mutate(dob = dob %>% str_replace_all(., "[()]", "") %>% lubridate::as_date(),
         age = age %>% str_extract(., "[0-9]+") %>% as.integer,
         country = forcats::fct_relevel(country, 
                                    "Brazil", "Argentina", "Uruguay", 
                                    "Peru", "Qatar", "Chile",  
                                    "Venezuela", "Paraguay", "Japan", 
                                    "Bolivia", "Colombia", "Ecuador",
                                    ),
         club = case_when(
           club == "Barcelona" & country == "Ecuador" ~ "Barcelona (Ecuador)",
           TRUE ~ club))


## ---- eval=FALSE, echo=FALSE---------------------------------------------
## saveRDS(squads_df_clean, file = here::here("data/copa_america2019_squads_clean.RDS"))


## ---- echo=FALSE---------------------------------------------------------
squads_df_clean <- readRDS(file = here::here("data/copa_america2019_squads_clean.RDS"))


## ------------------------------------------------------------------------
glimpse(squads_df_clean)


## ---- fig.height = 8, fig.width = 8--------------------------------------
age_country_plot <- squads_df_clean %>% 
  group_by(country) %>% 
  mutate(median_age = median(age)) %>% 
  ungroup() %>% 
  ggplot(aes(x = age)) +
  geom_histogram(fill = "red", binwidth = 1) +
  geom_vline(aes(xintercept = median_age), size = 1.2) +
  geom_label(aes(x = median_age, y = 8, 
                label = glue::glue("Median: {median_age}")),
            nudge_x = 0.5, hjust = 0.1, size = 3,
            family = "Roboto Condensed", color = "black") +
  labs(title = "Age Distribution of Copa América squads",
       subtitle = "Columns ordered Group A to Group C",
       x = "Age", y = NULL,
       caption = glue::glue("
                            Source: Wikipedia
                            By: @R_by_Ryo")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = scales::pretty_breaks()) +
  theme_copaAmerica(title.size = 22,
                    subtitle.size = 14, 
                    caption.size = 8,
                    axis.text.size = 12,
                    axis.title.size = 16,
                    strip.text.size = 18,
                    panel.grid.minor.x = element_line(color = "white"),
                    panel.grid.minor.y = element_line(color = "white")) +
  facet_wrap(~country, ncol = 3)
  
age_country_plot


## ---- eval=FALSE, echo=FALSE---------------------------------------------
## ggsave(plot = age_country_plot, filename = here::here("Copa America 2019/output/age_country_plot.png"),
##        height = 8, width = 8)


## ---- fig.height = 8, fig.width = 8--------------------------------------
caps_country_plot <- squads_df_clean %>% 
  group_by(country) %>% 
  mutate(median_cap = median(caps)) %>% 
  ungroup() %>% 
  ggplot(aes(x = caps)) +
  geom_histogram(fill = "red", binwidth = 5) +
  geom_vline(aes(xintercept = median_cap), size = 1.25) +
  geom_label(aes(x = median_cap, y = 15, 
                label = glue::glue("Median: {median_cap}")),
            nudge_x = 0.5, hjust = 0.05, size = 3,
            family = "Roboto Condensed", color = "black") +
  labs(title = "Caps (Appearances) by Country", 
       subtitle = "Columns ordered Group A to Group C",
       x = "Caps", y = NULL,
       caption = glue::glue("
                            Source: Wikipedia
                            By: @R_by_Ryo")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_copaAmerica(
    title.size = 20,
    subtitle.size = 14, 
    caption.size = 10,
    axis.text.size = 10,
    axis.title.size = 16,
    strip.text.size = 18,
    panel.grid.major.x = element_line(color = "white", size = 0.25),
    panel.grid.major.y = element_line(color = "white", size = 0.25),
    panel.grid.minor.x = element_line(color = "white", size = 0.25),
    panel.grid.minor.y = element_line(color = "white", size = 0.25)) +
  facet_wrap(~country, ncol = 3)

caps_country_plot


## ---- eval=FALSE, echo=FALSE---------------------------------------------
## ggsave(plot = caps_country_plot, filename = here::here("Copa America 2019/output/caps_country_plot.png"),
##        height = 8, width = 8)


## ---- fig.height = 6, fig.width = 8--------------------------------------
goals_country_plot <- squads_df_clean %>% 
  filter(position %in% c("MF", "FW")) %>% 
  group_by(country) %>% 
  mutate(median = median(goals)) %>% 
  ungroup() %>% 
  ggplot(aes(x = goals, y = reorder(country, median))) +
  ggridges::geom_density_ridges(fill = "red", color = "white", scale = 1.1) +
  geom_point(aes(x = median, y = country), position = position_nudge(y = 0.25),
             color = "yellow", size = 3) +
  ggforce::geom_mark_hull(aes(filter = country == "Argentina" & goals == 67, label = "Lionel Messi: 67 goals"),
                          label.buffer = unit(15, "mm"), label.fontsize = 10, label.fill = "red",
                          label.family = "Roboto Condensed", label.colour = "white",
                          con.cap = unit(1, "mm"), con.type = "straight") +
  ggforce::geom_mark_hull(aes(filter = country == "Uruguay" & goals == 55, label = "Luis Suarez: 55 goals"),
                          label.buffer = unit(5, "mm"), label.fontsize = 10, label.fill = "red",
                          label.family = "Roboto Condensed", label.colour = "white",
                          con.cap = unit(1, "mm"), con.type = "straight") +
  ggforce::geom_mark_hull(aes(filter = country == "Japan" & goals == 50, label = "Shinji Okazaki: 50 goals"),
                          label.buffer = unit(2, "mm"), label.fontsize = 10, label.fill = "red",
                          label.family = "Roboto Condensed", label.colour = "white",
                          con.cap = unit(1, "mm"), con.type = "straight") +
  ggforce::geom_mark_hull(aes(filter = country == "Uruguay" & goals == 46, label = "Edinson Cavani: 46 goals"),
                          label.buffer = unit(25, "mm"), label.fontsize = 10, label.fill = "red",
                          label.family = "Roboto Condensed", label.colour = "white",
                          con.cap = unit(1, "mm"), con.type = "straight") +
  ggforce::geom_mark_hull(aes(filter = country == "Chile" & goals == 41, label = "Alexis Sanchez: 41 goals"),
                          label.buffer = unit(4, "mm"), label.fontsize = 10, label.fill = "red",
                          label.family = "Roboto Condensed", label.colour = "white",
                          con.cap = unit(1, "mm"), con.type = "straight") +
  scale_x_continuous(limits = c(0, 73),
                     expand = c(0.01, 0.01),
                     breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70),
                     labels = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70)) +
  expand_limits(y = 13.5) +
  labs(title = "Distribution of Goals Scored by Midfielders and Strikers",
       subtitle = "Copa América 2019 squads, Yellow dot = Median goals",
       x = "Goals", y = NULL,
       caption = glue::glue("
                            Source: Wikipedia
                            Data from prior to start of tournament
                            By: @R_by_Ryo")) +
  theme_copaAmerica(title.size = 18,
                    subtitle.size = 12, 
                    caption.size = 8,
                    axis.text.size = 14,
                    axis.title.size = 16,
                    strip.text.size = 18) 

goals_country_plot


## ---- eval=FALSE, echo=FALSE---------------------------------------------
## ggsave(plot = goals_country_plot, filename = here::here("Copa America 2019/output/goals_country_plot.png"),
##        height = 6, width = 8)


## ---- fig.height = 7, fig.width = 8--------------------------------------
player_contrib_league_plot <- squads_df_clean %>% 
  group_by(country_league) %>%
  summarize(n = n()) %>%
  ungroup() %>% 
  ggplot(aes(y = n, x = reorder(country_league, n))) +
  geom_col(fill = "red") +
  geom_text(aes(label = n, family = "Roboto Condensed", fontface = "bold"), 
            size = 4.5, color = "yellow",
            nudge_y = 0.5) +
  coord_flip() +
  scale_y_continuous(labels = c(0, 5, 10, 15, 20, 25),
                     breaks = c(0, 5, 10, 15, 20, 25),
                     limits = c(0, 30),
                     expand = c(0, 0)) +
  labs(title = "Breakdown of Player Contributions by League",
       subtitle = glue("
                       Shown as Country Name 
                       Mexico (Liga MX) contributed 27 players to South American squads"),
       x = "League (Country name)", y = "Number of players",
       caption = glue::glue("
                            Source: Wikipedia
                            By: @R_by_Ryo")) +
  theme_copaAmerica(title.size = 18,
                    subtitle.size = 12,
                    caption.size = 10, 
                    axis.text.size = 14,
                    axis.text.y.size = 11,
                    axis.title.size = 16,
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.x = element_line(color = "white"))

player_contrib_league_plot


## ---- eval=FALSE, echo=FALSE---------------------------------------------
## ggsave(plot = player_contrib_league_plot, filename = here::here("Copa America 2019/output/player_contrib_league_plot.png"),
##        height = 7, width = 8)


## ------------------------------------------------------------------------
squads_df_clean %>% 
  group_by(country, country_league) %>% 
  summarize(player_from_league = n()) %>% 
  filter(country == country_league) %>% 
  mutate(perc_from_domestic_league = percent(player_from_league / 23, accuracy = 0.1)) %>% 
  right_join(squads_df_clean %>% 
              group_by(country, country_league) %>% 
              summarize(player_from_league = n()) %>% 
              ungroup()) %>% 
  mutate(first = case_when(
    country == country_league ~ 1,
    TRUE ~ 0)) %>% 
  arrange(country, desc(first)) %>% 
  fill(perc_from_domestic_league) %>% 
  group_by(country) %>% 
  mutate(perc_from_league = percent(player_from_league / 23, accuracy = 0.1),
         country_league = glue::glue("{country_league} - league")) %>% 
  arrange(desc(player_from_league)) %>% 
  select(Country = country, `League (country name)` = country_league,
         `Number of players from league` = player_from_league,
         `Percentage of players from league` = perc_from_league,
         `Percentage of players from domestic league` = perc_from_domestic_league) %>% 
  head(10) %>% 
  knitr::kable(format = "html",
               caption = "Breakdown of Player Contribution by League") %>% 
  kableExtra::kable_styling(full_width = FALSE)


## ----fig.height = 6, fig.width = 8---------------------------------------
player_contrib_club_plot <- squads_df_clean %>% 
  group_by(club) %>% 
  summarize(n = n()) %>% 
  mutate(club = club %>% forcats::as_factor() %>% forcats::fct_reorder(n),
         midval = n / 2) %>% 
  arrange(desc(n)) %>% 
  slice(1:15) %>% 
  ggplot(aes(x = club, y = n)) +
  geom_col(fill = "red") +
  geom_text(aes(label = n, family = "Roboto Condensed", fontface = "bold"), 
            size = 7.5, color = "yellow",
            nudge_y = 0.5) +
  geom_text(aes(y = midval, label = club, 
                family = "Roboto Condensed", fontface = "bold"),
            size = 5, color = "white") +
  coord_flip() +
  scale_y_continuous(breaks = scales::pretty_breaks(),
                     expand = c(0, 0),
                     limits = c(0, 10.5)) +
  labs(title = "Top 15 Clubs contributing the most players to the Copa América",
       x = "Club", y = "Number of players",
       caption = "Source: Wikipedia") +
  theme_copaAmerica(
    title.size = 18,
    subtitle.size = 12, 
    caption.size = 8,
    axis.text.size = 14,
    axis.title.size = 16,
    strip.text.size = 18,
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_line(color = "white")) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

player_contrib_club_plot


## ---- eval=FALSE, echo=FALSE---------------------------------------------
## ggsave(plot = player_contrib_club_plot, filename = here::here("Copa America 2019/output/player_contrib_club_plot.png"),
##        height = 6, width = 8)


## ---- eval=FALSE---------------------------------------------------------
## squads_df_clean %>%
##     group_by(club, country) %>%
##     summarize(n = n()) %>% View()


## ---- message=FALSE, eval=FALSE------------------------------------------
## ## grab football federation affiliations data
## federation_files <- Sys.glob("../data/federation_affiliations/*")
## 
## df_federations = data.frame(country = NULL, federation = NULL)
## for (f in federation_files) {
##     federation = basename(f)
##     content = read.csv(f, header=FALSE)
##     content <- cbind(content,federation=rep(federation, dim(content)[1]))
##     df_federations <- rbind(df_federations, content)
## }
## 
## colnames(df_federations) <- c("country", "federation")
## 
## df_federations <- df_federations %>%
##   mutate(country = as.character(country) %>% str_trim(side = "both"))
## 
## results_raw <- readr::read_csv("../data/results.csv")
## 
## results_copa <- results_raw %>%
##   filter(tournament == "Copa América") %>%
##   rename(venue_country = country,
##          venue_city = city) %>%
##   mutate(match_num = row_number())
## 
## ## combine with federation affiliations
## results_copa_home <- results_copa %>%
##   left_join(df_federations,
##             by = c("home_team" = "country")) %>%
##   mutate(federation = as.character(federation)) %>%
##   rename(home_federation = federation)
## 
## results_copa_away <- results_copa %>%
##   left_join(df_federations,
##             by = c("away_team" = "country")) %>%
##   mutate(federation = as.character(federation)) %>%
##   rename(away_federation = federation)
## 
## ## combine home-away
## results_copa_cleaned <- results_copa_home %>%
##   full_join(results_copa_away)


## ---- eval=FALSE---------------------------------------------------------
## results_copa_cleaned <- results_copa_cleaned %>%
##   mutate(
##     home_federation = case_when(
##       home_team == "USA" ~ "Concacaf",
##       TRUE ~ home_federation),
##     away_federation = case_when(
##       away_team == "USA" ~ "Concacaf",
##       TRUE ~ away_federation)) %>%
##   select(-contains("federation"), -contains("venue"),
##          -neutral, date, home_team, home_score, away_team, away_score,
##          tournament, venue_city)


## ---- echo=FALSE, eval=FALSE---------------------------------------------
## saveRDS(results_copa_cleaned, file = here::here("data/results_copa_cleaned.RDS"))


## ---- echo=FALSE---------------------------------------------------------
results_copa_cleaned <- readRDS(file = here::here("data/results_copa_cleaned.RDS"))


## ------------------------------------------------------------------------
glimpse(results_copa_cleaned)


## ------------------------------------------------------------------------
copaAmerica_resultados <- function(data, team, versus = NA) {
  
  ## team of interest: ex. 'Brazil'
  team_var <- enquo(team)
  
  todos_partidos <- data %>% 
    ## filter only for results of team of interest
    filter(home_team == !!team_var | away_team == !!team_var) %>% 
    ## reshape columns to team vs. opponent
    mutate(
      opponent = case_when(
        away_team != !!team_var ~ away_team,
        home_team != !!team_var ~ home_team),
      home_away = case_when(
        home_team == !!team_var ~ "home",
        away_team == !!team_var ~ "away"),
      equipo_goals = case_when(
        home_team == !!team_var ~ home_score,
        away_team == !!team_var ~ away_score),
      opp_goals = case_when(
        home_team != !!team_var ~ home_score,
        away_team != !!team_var ~ away_score)) %>% 
    ## label results from team's perspective
    mutate(
      result = case_when(
        equipo_goals > opp_goals ~ "Win",
        equipo_goals < opp_goals ~ "Loss",
        equipo_goals == opp_goals ~ "Draw")) %>% 
    mutate(result = result %>% forcats::as_factor() %>% forcats::fct_relevel(c("Win", "Draw", "Loss"))) %>% 
    select(-contains("score"), -contains("team"), -match_num) %>% 
    rename(Date = date, Tournament = tournament, `Venue` = venue_city, Opponent = opponent, `Home / Away` = home_away,
           `Goals For` = equipo_goals, `Goals Against` = opp_goals, Result = result)
  
  if (is.na(versus) | is.null(versus)) {
    
    resultados_totalmente <- todos_partidos %>% 
      group_by(Result, Opponent) %>% 
      mutate(n = n()) %>% 
      ungroup() %>% 
      ## sum amount of goals by team and opponent
      group_by(Result, Opponent) %>% 
      summarize(e_g = sum(`Goals For`),
                o_g = sum(`Goals Against`),
                n = n()) %>% 
      ungroup() %>% 
      ## spread results over multiple columns
      spread(Result, n) %>% 
      mutate_if(is.integer, as.numeric)
    
    missing_cols <- c("Win", "Draw", "Loss") %>% 
      map_dfr( ~tibble(!!.x := numeric()))
    
    resultados_totalmente <- resultados_totalmente %>% 
      bind_rows(missing_cols) %>% 
      mutate(Win = if_else(is.na(Win), 0, Win),
             Draw = if_else(is.na(Draw), 0, Draw),
             Loss = if_else(is.na(Loss), 0, Loss)) %>%
      group_by(Opponent) %>% 
      summarize(Win = sum(Win, na.rm = TRUE),
                Draw = sum(Draw, na.rm = TRUE),
                Loss = sum(Loss, na.rm = TRUE),
                `Goals For` = sum(e_g),
                `Goals Against` = sum(o_g))
    
    return(list(resultados_totalmente, todos_partidos))
  } else { 
    ## opponent: ex. 'Argentina'
    todos_partidos <- todos_partidos %>% 
      filter(Opponent == versus)
    
    if (nrow(todos_partidos) == 0) {
     return(glue("{team} has never played {versus} at the Copa América!")) 
    } else {
    
    resultados_totalmente <- todos_partidos %>% 
      group_by(Result, Opponent) %>% 
      mutate(n = n()) %>% 
      ungroup() %>% 
      # sum amount of goals by team and opponent
      group_by(Result, Opponent) %>% 
      summarize(e_g = sum(`Goals For`),
                o_g = sum(`Goals Against`),
                n = n()) %>% 
      ungroup() %>% 
      # spread results over multiple columns
      spread(Result, n) %>% 
      mutate_if(is.integer, as.numeric) %>%       
      group_by(Opponent) %>% 
      summarize(Win = sum(Win, na.rm = TRUE),
                Draw = sum(Draw, na.rm = TRUE),
                Loss = sum(Loss, na.rm = TRUE),
                `Goals For` = sum(e_g),
                `Goals Against` = sum(o_g))
    
    return(list(resultados_totalmente, todos_partidos))
    }
  }
}


## ---- warning=FALSE------------------------------------------------------
copaAmerica_resultados(data = results_copa_cleaned, 
               team = "Japan", versus = "Brazil")


## ------------------------------------------------------------------------
resultados_japon <- copaAmerica_resultados(data = results_copa_cleaned, team = "Japan")

resultados_japon[[2]] %>% 
  knitr::kable(format = "html",
               caption = "Japan's record in the Copa América") %>% 
  kableExtra::kable_styling(full_width = FALSE)


## ------------------------------------------------------------------------
resultados_colombia <- copaAmerica_resultados(data = results_copa_cleaned, team = "Colombia") 

resultados_colombia[[2]] %>% 
  slice(87:92) %>% 
  knitr::kable(format = "html",
               caption = "Colombia's record in the Copa América") %>% 
  kableExtra::kable_styling(full_width = FALSE)


## ------------------------------------------------------------------------
resultados_de_brazil <- copaAmerica_resultados(data = results_copa_cleaned, 
               team = "Brazil", versus = "Argentina")

resultados_de_brazil[[1]] %>% 
  knitr::kable(format = "html",
               caption = "Brazil vs. Argentina in the Copa América") %>% 
  kableExtra::kable_styling(full_width = FALSE)


## ------------------------------------------------------------------------
resultados_de_brazil[[2]] %>% 
  tail(5) %>% 
  knitr::kable(format = "html",
               caption = "Brazil vs. Argentina in the Copa América") %>% 
  kableExtra::kable_styling(full_width = FALSE)


## ---- eval=FALSE---------------------------------------------------------
## player_codes <- c(2097, 2099, 813,   ## Messi, Neymar, Rondon
##                   498, 4299, 696,    ## Alexis, Farfan, Falcao
##                   3294, 2098, 5543,  ## Cavani, Suarez, G. Jesus
##                   482, 1148, 2249,   ## Bobby, Duvan, James
##                   1089, 3553, 488,   ## Cuadrado, Di Maria, Coutinho
##                   222)               ## Arturo Vidal
## 
## understat_data <- player_codes %>%
##   map(., ~ understatr::get_player_seasons_stats(.x)) %>%
##   reduce(bind_rows) %>%
##   select(-player_id, -position, -yellow, -red)


## ---- eval=FALSE, echo=FALSE---------------------------------------------
## saveRDS(understat_data, file = here::here("data/copa_america_understat.RDS"))


## ---- echo=FALSE---------------------------------------------------------
understat_data <- readRDS(file = here::here("data/copa_america_understat.RDS"))


## ------------------------------------------------------------------------
glimpse(understat_data)


## ---- fig.height = 6, fig.width = 8--------------------------------------
comparison_data <- understat_data %>% 
  filter(year == 2018) %>% 
  select(-games, -team_name, -year) %>% 
  rename(Shots = shots, KP = key_passes) %>% 
  gather(key = "key", value = "value", -player_name, -time) %>% 
  mutate(key = forcats::as_factor(key) %>% 
           forcats::fct_relevel(., 
                                "xG", "goals", "npxG", "npg", 
                                "xA", "assists", "xGChain", "xGBuildup",
                                "Shots", "KP"))

comparison_strikers_plot <- comparison_data %>%
  filter(key != "Shots", key != "KP",
         key != "xGBuildup", key != "xGChain") %>% 
  mutate(value = value / time * 90) %>% 
  ggplot(aes(x = key, y = value, fill = player_name)) +
  geom_jitter(shape = 21, size = 5, color = "black", width = 0.25, stroke = 1.1) +
  geom_vline(xintercept = 1.5, size = 2) +
  geom_vline(xintercept = 2.5, size = 2) +
  geom_vline(xintercept = 3.5, size = 2) +
  geom_vline(xintercept = 4.5, size = 2) +
  geom_vline(xintercept = 5.5, size = 2) +
  coord_flip() +
  scale_y_continuous(expand = c(0.01, 0.01),
                     limits = c(0, 1.26)) +
  scale_fill_manual(values = pals::glasbey(16), name = "Player") +
  labs(title = "Comparison: Top attackers at the Copa América",
       subtitle = "For select group of attacking players with data available from understat.com",
       x = NULL, y = "Metric per 90",
       caption = glue::glue("
                            data: understat.com
                            2018-2019 Season")) +
  theme_copaAmerica(title.size = 18,
                    subtitle = 12,
                    panel.grid.minor.x = element_line(color = "white"))

comparison_strikers_plot


## ---- eval=FALSE, echo=FALSE---------------------------------------------
## ggsave(plot = comparison_strikers_plot, filename = here::here("Copa America 2019/output/comparison_strikers_plot.png"),
##        height = 5, width = 8)


## ---- fig.height = 5, fig.width = 8--------------------------------------
expected_goal_plot <- understat_data %>% 
  filter(year == 2018) %>% 
  select(player_name, time, npxG, xG, goals) %>% 
  mutate_at(c("npxG", "xG", "goals"), ~. / time * 90) %>% 
  ggplot(aes(x = npxG, y = goals, fill = player_name)) +
  geom_abline(intercept = 0, slope = 1, color = "white", size = 1.1) +
  geom_point(shape = 21, size = 5, color = "black", stroke = 1.1) +
  scale_x_continuous(limits = c(0, 1.1),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1.3),
                     expand = c(0, 0)) +
  scale_fill_manual(values = pals::glasbey(16), name = "Player") +
  labs(title = "Expected vs. Actual Goals",
       subtitle = "For select group of attacking players with data available from understat.com",
       x = "Non-penalty xG per 90 minutes",
       y = "Goals per 90 minutes",
       caption = glue::glue("
                            data: understat.com
                            2018-2019 Season")) +
  theme_copaAmerica(panel.grid.minor.x = element_line(color = "white"),
                    panel.grid.minor.y = element_line(color = "white"),
                    subtitle.size = 11)

expected_goal_plot


## ---- eval=FALSE, echo=FALSE---------------------------------------------
## ggsave(plot = expected_goal_plot, filename = here::here("Copa America 2019/output/expected_goal_plot.png"),
##        height = 5, width = 8)


## ---- fig.height = 5, fig.width = 8--------------------------------------
expected_assists_plot <- understat_data %>% 
  filter(year == 2018) %>% 
  select(player_name, time, xA, assists) %>% 
  mutate_at(c("xA", "assists"), ~. / time * 90) %>% 
  ggplot(aes(x = xA, y = assists, fill = player_name)) +
  geom_abline(intercept = 0, slope = 1, color = "white", size = 1.1) +
  geom_point(shape = 21, size = 5, color = "black", stroke = 1.1) +
  labs(title = "Expected vs. Actual Assists",
       subtitle = "For select group of attacking players with data available from understat.com",
       x = "xA per 90 minutes",
       y = "Assists per 90 minutes",
       caption = glue::glue("
                            data: understat.com
                            2018-2019 Season")) +
  scale_x_continuous(limits = c(0, 0.55),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 0.55),
                     expand = c(0, 0)) +
  scale_fill_manual(values = pals::glasbey(16), name = "Player") +
  theme_copaAmerica(panel.grid.minor.x = element_line(color = "white"),
                    panel.grid.minor.y = element_line(color = "white"),
                    subtitle.size = 11)

expected_assists_plot


## ---- eval=FALSE, echo=FALSE---------------------------------------------
## ggsave(plot = expected_assists_plot, filename = here::here("Copa America 2019/output/expected_assists_plot.png"),
##        height = 5, width = 8)


## ---- fig.height=5, fig.width=7------------------------------------------
kp_shots_plot <- comparison_data %>%
  filter(key == "Shots" | key == "KP") %>% 
  mutate(value = value / time * 90) %>% 
  ggplot(aes(x = key, y = value, fill = player_name)) +
  geom_jitter(shape = 21, size = 5, color = "black", width = 0.25, stroke = 1.1) +
  coord_flip() +
  scale_y_continuous(expand = c(0.01, 0.01),
                     limits = c(0, 6),
                     breaks = c(0, 1, 2, 3, 4, 5, 6),
                     labels = c(0, 1, 2, 3, 4, 5, 6)) +
  scale_fill_manual(values = pals::glasbey(17), name = "Player") +
  geom_vline(xintercept = 1.5, size = 2) +
  labs(title = "Comparison: Stars of the Copa América",
       subtitle = glue("
                       KP = Key Passes
                       For select group of attacking players with data available from understat.com"),
       x = NULL, y = "Metric per 90",
       caption = glue::glue("
                            data: understat.com
                            2018-2019 Season")) +
  theme_copaAmerica(title.size = 18,
                    subtitle.size = 10,
                    panel.grid.minor.x = element_line(color = "white"))

kp_shots_plot


## ---- eval=FALSE, echo=FALSE---------------------------------------------
## ggsave(plot = kp_shots_plot, filename = here::here("Copa America 2019/output/kp_shots_plot.png"),
##        height = 5, width = 7)


## ---- fig.height=5, fig.width=7------------------------------------------
xgbuildup_xgchain_plot <- comparison_data %>%
  filter(key == "xGBuildup" | key == "xGChain") %>% 
  mutate(value = value / time * 90) %>% 
  ggplot(aes(x = key, y = value, fill = player_name)) +
  geom_jitter(shape = 21, size = 5, color = "black", width = 0.25, stroke = 1.1) +
  coord_flip() +
  scale_y_continuous(expand = c(0.01, 0.01),
                     limits = c(0, 1.55),
                     breaks = c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5),
                     labels = c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5)) +
  scale_fill_manual(values = pals::glasbey(17), name = "Player") +
  geom_vline(xintercept = 1.5, size = 2) +
  labs(title = "Comparison: Stars of the Copa América",
       subtitle = "For select group of attacking players with data available from understat.com",
       x = NULL, y = "Metric per 90",
       caption = glue::glue("
                            data: understat.com
                            2018-2019 Season")) +
  theme_copaAmerica(title.size = 18,
                    subtitle.size = 10,
                    panel.grid.minor.x = element_line(color = "white"))

xgbuildup_xgchain_plot


## ---- eval=FALSE, echo=FALSE---------------------------------------------
## ggsave(plot = xgbuildup_xgchain_plot, filename = here::here("Copa America 2019/output/xgbuildup_xgchain_plot.png"),
##        height = 5, width = 7)


## ---- fig.height=5, fig.width=7------------------------------------------
## keep colors for Colombians consistent with other plots
colombia_pal <- c("#000033", "#005300", "#009FFF", "#00FFBE")

comparison_colombia_plot <- comparison_data %>%
  filter(!key %in% c("xG", "goals", "npxG", "npg", "xA", "assists"),
         player_name %in% c("James Rodríguez", "Falcao", "Duván Zapata", "Juan Cuadrado")) %>% 
  mutate(value = value / time * 90) %>% 
  ggplot(aes(x = key, y = value, fill = player_name)) +
  geom_point(shape = 21, size = 5, color = "black", stroke = 1.1) +
  geom_vline(xintercept = 1.5, size = 2) +
  geom_vline(xintercept = 2.5, size = 2) +
  geom_vline(xintercept = 3.5, size = 2) +
  coord_flip() +
  scale_y_continuous(expand = c(0.05, 0.05),
                     breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4),
                     labels = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4)) +
  scale_fill_manual(values = colombia_pal, name = "Player") +
  labs(title = "Comparison: Stars of Colombia",
       subtitle = "KP: Key Passes",
       x = NULL, y = "Metric per 90",
       caption = glue::glue("
                            data: understat.com
                            2018-2019 Season")) +
  theme_copaAmerica(title.size = 20,
                    subtitle.size = 12,
                    panel.grid.minor.x = element_line(color = "white"))

comparison_colombia_plot


## ---- eval=FALSE, echo=FALSE---------------------------------------------
## ggsave(plot = comparison_colombia_plot, filename = here::here("Copa America 2019/output/comparison_colombia_plot.png"),
##        height = 5, width = 7)

