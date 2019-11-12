## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------
pacman::p_load(tidyverse,
               rvest, polite, lubridate)

library(tidyverse)


## ------------------------------------------------------------------------
url <- "https://www.premierleague.com/clubs/10/Liverpool/results?se=210"

.tabbedContent > div:nth-child(1) > div:nth-child(5) > section:nth-child(1)
div.fixtures__matches-list:nth-child(3) > ul:nth-child(2) > li:nth-child(1)


session <- bow(url)

prem_df_raw <- scrape(session) %>% 
  html_nodes(".tabbedContent") %>% 
  html_text()


prem_df_raw <- scrape(session) %>% 
  html_nodes(".shortname") %>% 
  html_text()



## ------------------------------------------------------------------------
library(understatr)

bobby_shots <- get_player_shots(482)


## ---- fig.height=8, fig.width=10-----------------------------------------
library(ggsoccer)
library(ggplot2)

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
library(stringi)
library(jsonlite)
library(stringr)
library(xml2)
library(qdapRegex)


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
match_id <- 11670
player_id <- 482


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
match_url <- stringr::str_glue("{home_url}/match/{match_id}")

match_page <- read_html(match_url)

match_data <- get_script(match_page)

match_shots_data <- get_data_element(match_data, "shotsData")

#msd <- jsonlite::stream_in(match_shots_data)

match_shots_data <- fix_json(match_shots_data)

# Home: Liverpool
liv_shots_data <- fromJSON(match_shots_data[1])

## add 'team_name' with home team name from 'h_team' var

# Away: Arsenal
ars_shots_data <- fromJSON(match_shots_data[2])

## add 'team_name' with away team name from 'a_team' var


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
match_url <- stringr::str_glue("{home_url}/match/{match_id}")

match_page <- read_html(match_url)

team_stats <- match_page %>% 
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
  janitor::row_to_names(row_number = 1)
  
  
  # rownames_to_column() %>% 
  # gather(key = var, value = thing, -rowname) %>% 
  # spread(rowname, thing)
  # 
  # 
  # t() %>% 
  # 
  # tibble::as_tibble()
  # tidyr::pivot_longer(names_to = )


## ------------------------------------------------------------------------
match_url <- stringr::str_glue("{home_url}/match/{match_id}")

match_page <- read_html(match_url)

team_stats <- match_page %>% 
  html_nodes("div.scheme-block:nth-child(4)") %>% 
  html_text()
#match-rosters > table:nth-child(3) #match-rosters > table:nth-child(3)
#match-rosters > table:nth-child(3) > tbody:nth-child(2)


stats_string <- team_stats %>% str_squish()
#  str_remove_all(., "\\n|\\t")
  # str_replace_all(., "\\n|\\t", " ") %>%
  # str_squish()

stats_string %>% 
  str_remove_all(., "CHANCES") %>% 
  str_remove_all(., "([0-9]{2,}%)") %>% 
  str_replace_all(., "SHOTS ON TARGET", "ON-TARGET") %>% 
  str_squish() %>% 
  read.table(text = ., header = FALSE, sep = " ", 
             col.names = c("TEAMS", "GOALS", "xG",
                           "SHOTS", "ON-TARGET", "DEEP",
                           "PPDA", "xPTS"))
  

team_stats %>%
  str_squish() %>% 
  str_remove_all(., "CHANCES") %>% 
  str_remove_all(., "([0-9]{2,}%)") %>% 
  str_replace_all(., "SHOTS ON TARGET", "ON-TARGET") %>% 
  str_squish() %>% 
  read.table(text = ., header = FALSE, sep = " ",
             col.names = c("var_name", "home", "away"))




  
asdf <- stats_string %>% 
  str_remove_all(., "CHANCES") %>% 
  str_remove_all(., "([0-9]{2,}%)") %>% 
  str_replace_all(., "SHOTS ON TARGET", "ON-TARGET") %>% 
  str_squish()

asdf %>% strsplit(split = " ")


  
stats_string %>% 
  read.table(text = ., header = FALSE, sep = " ",
             col.names = c("TEAMS", "CHANCES", "GOALS", "xG",
                           "SHOTS", "SHOTS ON TARGET", "DEEP",
                           "PPDA", "xPTS"))


all <- match_page %>% 
  html_nodes("div.scheme-block:nth-child(4)") %>% 
  .[[1]]

print(all, width = 50000)
print(team_stats, width = 50000)
print(stats_string, width = 50000)

asdf


## ------------------------------------------------------------------------
xml <- all %>% xml_find_all()


## ------------------------------------------------------------------------
library(XML)

all2 <- xmlTreeParse(all, asText = TRUE, useInternalNodes = TRUE)

m <- xpathSApply(all, "//div[@class = 'scheme-block is-hide'", function(node) {
  list(p = xmlValue(node[["progress-title"]]))
})


## ------------------------------------------------------------------------
do.call(rbind, lapply(all, function(x) {
  title <- tryCatch(xml_text(xml_node(x, "div.progress-title")))
  
  value <- tryCatch(xml_text(xml_node(x, "div.progress-home progress-over")))
  
  data.frame(title, value, stringsAsFactors = FALSE)
}))

