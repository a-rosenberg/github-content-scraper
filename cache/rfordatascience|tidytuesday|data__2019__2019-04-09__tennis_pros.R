## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------
library(tidyverse)
library(rvest)
library(lubridate)
library(janitor)


## ------------------------------------------------------------------------
raw_slams <- read_html("https://en.wikipedia.org/wiki/List_of_Grand_Slam_women%27s_singles_champions") %>% 
  html_table(fill = TRUE) %>% 
  .[[3]] %>% 
  janitor::clean_names()

clean_slams <- raw_slams %>% 
  filter(year >= 1968) %>%
  gather(key = "grand_slam", "winner", australian_open:us_open) %>% 
  separate(col = winner, sep = "\\(", into = c("winner", "win_count")) %>% 
  separate(col = win_count, sep = "/", into = c("rolling_win_count", "total_win_count")) %>% 
  mutate(winner = str_trim(winner),
         rolling_win_count = as.integer(rolling_win_count),
         total_win_count = as.integer(str_extract(total_win_count, "[:digit:]+"))) %>% 
  rename(name = winner) %>% 
  mutate(name = str_trim(str_remove(name, "‡")),
         name = str_trim(str_remove(name, "Open era tennis begins|Tournament date changed"))) %>% 
  filter(str_length(name) > 4) %>% 
  mutate(name = case_when(str_detect(name, "Goolagong") ~ "Evonne Goolagong Cawley",
                          TRUE ~ name)) %>% 
  mutate(tournament_date = case_when(grand_slam == "australian_open" ~ paste0(year, "-01-10"),
                                     grand_slam == "french_open" ~ paste0(year, "-06-09"),
                                     grand_slam == "us_open" ~ paste0(year, "-09-09"),
                                     grand_slam == "wimbledon" ~ paste0(year, "-07-14"),
                                     TRUE ~ NA_character_),
         tournament_date = lubridate::ymd(tournament_date),
         gender = "Female") %>% 
  group_by(name) %>% 
  arrange(tournament_date) %>% 
  mutate(rolling_win_count = row_number()) %>% 
  ungroup()


## ------------------------------------------------------------------------

raw_slams_men <- read_html("https://en.wikipedia.org/wiki/List_of_Grand_Slam_men%27s_singles_champions") %>% 
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[1]') %>% 
  html_table(fill = TRUE) %>% .[[1]] %>% janitor::clean_names()

clean_slams_men <- raw_slams_men %>% 
  filter(year >= 1968) %>%
  gather(key = "grand_slam", "winner", australian_open:us_open) %>% 
  separate(col = winner, sep = "\\(", into = c("winner", "win_count")) %>% 
  separate(col = win_count, sep = "/", into = c("rolling_win_count", "total_win_count")) %>% 
  separate(col = winner, into = c("country", "winner"), sep = ":", fill = "left") %>% 
  mutate(winner = str_trim(winner),
         rolling_win_count = as.integer(rolling_win_count),
         total_win_count = as.integer(str_extract(total_win_count, "[:digit:]+"))) %>% 
  rename(name = winner) %>% 
  mutate(name = str_trim(str_remove_all(name, "‡|†")),
         name = str_trim(str_remove(name, "Amateur era tennis ends|Open era tennis begins|Tournament date changed"))) %>% 
  filter(str_length(name) > 4) %>% 
  mutate(tournament_date = case_when(grand_slam == "australian_open" ~ paste0(year, "-01-10"),
                                     grand_slam == "french_open" ~ paste0(year, "-06-09"),
                                     grand_slam == "us_open" ~ paste0(year, "-09-09"),
                                     grand_slam == "wimbledon" ~ paste0(year, "-07-14"),
                                     TRUE ~ NA_character_),
         tournament_date = lubridate::ymd(tournament_date),
         gender = "Male") %>% 
  select(-country) %>% 
   group_by(name) %>% 
  arrange(tournament_date) %>% 
  mutate(rolling_win_count = row_number()) %>% 
  ungroup()



## ------------------------------------------------------------------------
clean_dob <- read_html("https://en.wikipedia.org/wiki/List_of_Grand_Slam_singles_champions_in_Open_Era_with_age_of_first_title") %>% 
  html_table(fill = TRUE) %>% 
  .[[2]] %>% 
  janitor::clean_names() %>% 
  select(name, "grand_slam" = tournament, date_of_birth, date_of_first_title) %>% 
  mutate(name = str_trim(str_remove(name, "\\*")),
         grand_slam = str_trim(str_remove(grand_slam, "[:digit:]+")),
         date_of_birth = lubridate::dmy(date_of_birth),
         date_of_first_title = lubridate::dmy(date_of_first_title),
         age = date_of_first_title - date_of_birth) %>% 
  mutate(name = case_when(str_detect(name, "Goolagong") ~ "Evonne Goolagong Cawley",
                          str_detect(name, "Reid") ~ "Kerry Melville Reid",
                          str_detect(name, "Vicario") ~ "Arantxa Sánchez Vicario",
                          TRUE ~ name)) %>% 
  bind_rows(tibble(name = c("Ann Haydon-Jones","Chris O'Neil"),
                   date_of_birth = c(lubridate::dmy("7 October 1938"), lubridate::dmy("19 March 1956"))))

dob_df <- clean_dob %>% 
  select(date_of_birth, name)


## ------------------------------------------------------------------------
age_slams <- left_join(clean_slams, dob_df, by = c("name")) %>% 
  mutate(age = tournament_date - date_of_birth) %>%
  group_by(name, age) %>% 
  summarize(counts = n()) %>% 
  group_by(name) %>% 
  mutate(total_wins = cumsum(counts)) %>% 
  arrange(desc(total_wins))


## ------------------------------------------------------------------------
clean_dob_men <- read_html("https://en.wikipedia.org/wiki/List_of_Grand_Slam_singles_champions_in_Open_Era_with_age_of_first_title") %>% 
  html_table(fill = TRUE) %>% 
  .[[1]] %>% 
  janitor::clean_names() %>% 
  select(name, "grand_slam" = tournament, date_of_birth, date_of_first_title) %>% 
  mutate(name = str_trim(str_remove(name, "\\*")),
         grand_slam = str_trim(str_remove(grand_slam, "[:digit:]+")),
         date_of_birth = lubridate::dmy(date_of_birth),
         date_of_first_title = lubridate::dmy(date_of_first_title),
         age = date_of_first_title - date_of_birth) %>% 
  bind_rows(tibble(name = "William Bowrey",
                   date_of_birth = lubridate::dmy("25 December 1943")))

dob_df_men <- clean_dob_men %>% 
  select(date_of_birth, name)


## ------------------------------------------------------------------------
age_slams_men <- left_join(clean_slams_men, dob_df_men, by = c("name")) %>% 
  mutate(age = tournament_date - date_of_birth) %>%
  group_by(name, age) %>% 
  summarize(counts = n()) %>% 
  group_by(name) %>% 
  mutate(total_wins = cumsum(counts)) %>% 
  arrange(desc(total_wins))

age_slams_men %>% 
  ggplot(aes(x = age, y = total_wins, group = name)) +
  geom_point() +
  geom_step()


## ------------------------------------------------------------------------
grand_slams <- bind_rows(clean_slams, clean_slams_men) %>% 
  select(-total_win_count)


## ------------------------------------------------------------------------
player_dob <- bind_rows(clean_dob, clean_dob_men)


## ------------------------------------------------------------------------
age_slams_comb <- left_join(grand_slams, player_dob, by = c("name")) %>% 
  mutate(age = tournament_date - date_of_birth) %>%
  group_by(name, age, gender) %>% 
  summarize(counts = n()) %>% 
  group_by(name) %>% 
  mutate(total_wins = cumsum(counts)) %>% 
  arrange(desc(total_wins))

# test plot
age_slams_comb %>% 
  ggplot(aes(x = age, y = total_wins, group = name)) +
  geom_point() +
  geom_step() +
  facet_wrap(~gender)


## ------------------------------------------------------------------------
write_csv(grand_slams, "grand_slams.csv")
write_csv(player_dob, "player_dob.csv")


## ------------------------------------------------------------------------
yr_1968_1970 <- read_html("https://en.wikipedia.org/wiki/Tennis_performance_timeline_comparison_(women)_(1884%E2%80%931977)") %>% 
  html_table(fill = TRUE) %>% 
  .[[12]]
clean_1968_1970 <- yr_1968_1970 %>% 
  set_names(nm = paste0(names(yr_1968_1970), "_", yr_1968_1970[1,])) %>% 
  filter(Player_Player != "Player") %>% 
  gather(key = year_tourn, value = outcome, `1964_AUS`:`1970_USA`) %>% 
  separate(col = year_tourn, into = c("year", "tournament"), sep = "_") %>% 
  rename(player = Player_Player) %>% 
  mutate(year = as.integer(year)) %>% 
  filter(year >= 1968)

yr_1971_1977 <- read_html("https://en.wikipedia.org/wiki/Tennis_performance_timeline_comparison_(women)_(1884%E2%80%931977)") %>% 
  html_table(fill = TRUE) %>% 
  .[[13]]

clean_1971_1977 <- yr_1971_1977 %>% 
  set_names(nm = paste0(names(yr_1971_1977), "_", yr_1971_1977[1,])) %>% 
  filter(Player_Player != "Player") %>% 
  gather(key = year_tourn, value = outcome, `1971_AUS`:`1977_AUSD`) %>% 
  separate(col = year_tourn, into = c("year", "tournament"), sep = "_") %>% 
  rename(player = Player_Player) %>% 
  mutate(year = as.integer(year))

names(yr_1968_1970) %>% unique() %>% .[. != "Player"] %>% as.integer()


## ------------------------------------------------------------------------

get_timeline <- function(table_num){
  
  Sys.sleep(5)
  url <- "https://en.wikipedia.org/wiki/Tennis_performance_timeline_comparison_(women)"
  
  df <- read_html(url) %>% html_table(fill = TRUE) %>% .[[table_num]]
  
  year_range <- names(df) %>% 
    unique() %>% 
    .[. != "Player"] %>% 
    as.integer()
  
  year_min <- min(year_range)
  year_max <- max(year_range)
  
  tourn_list <- df %>% janitor::clean_names() %>% slice(1) %>% unlist(., use.names = FALSE) %>% .[!is.na(.)]
  
  first_tourn <- tourn_list[2]
  last_tourn <- tourn_list[length(tourn_list)] 

  
  df %>%
    set_names(nm = paste0(df[1,], "_", names(df))) %>%
    filter(Player_Player != "Player") %>%
    gather(key = year_tourn, value = outcome,
           paste(first_tourn, year_min, sep = "_"):paste(last_tourn, year_max, sep = "_")) %>%
    separate(col = year_tourn, into = c("tournament", "year"), sep = "_") %>%
    rename(player = Player_Player) %>%
    mutate(year = as.integer(year))
}


## ------------------------------------------------------------------------

clean_1978_2012 <- 5:9 %>%
  map(get_timeline) %>%
  bind_rows()



## ------------------------------------------------------------------------
df_2013_2019 <- read_html("https://en.wikipedia.org/wiki/Tennis_performance_timeline_comparison_(women)")  %>% 
    html_table(fill = TRUE) %>% 
    .[[10]]

clean_2013_2019 <- df_2013_2019 %>% 
  set_names(nm = paste0(df_2013_2019[1,], "_", names(df_2013_2019))) %>%
  filter(Player_Player != "Player") %>%
  select(-31) %>% 
  gather(key = year_tourn, value = outcome,
         paste("AUS", "2013", sep = "_"):paste("AUS", "2019", sep = "_")) %>%
  separate(col = year_tourn, into = c("tournament", "year"), sep = "_") %>%
  rename(player = Player_Player) %>%
  mutate(year = as.integer(year)) %>% 
  select(-contains("2019"))


## ------------------------------------------------------------------------
final_timeline <- bind_rows(list(clean_1968_1970, clean_1971_1977, clean_1978_2012, clean_2013_2019)) %>% 
  mutate(outcome = case_when(outcome == "W" ~ "Won",
                             outcome == "F" ~ "Finalist",
                             outcome == "SF" ~ "Semi-finalist",
                             outcome == "QF" ~ "Quarterfinalist",
                             outcome == "4R" ~ "4th Round",
                             outcome == "3R" ~ "3rd Round",
                             outcome == "2R" ~ "2nd Round",
                             outcome == "1R" ~ "1st Round",
                             outcome == "RR" ~ "Round-robin stage",
                             outcome == "Q2" ~ "Qualification Stage 2",
                             outcome == "Q1" ~ "Qualification Stage 1",
                             outcome == "A" ~ "Absent",
                             str_detect(outcome, "Retired") ~ "Retired",
                             outcome == "-" ~ NA_character_,
                             outcome == "LQ" ~ "Lost Qualifier",
                             TRUE ~ NA_character_),
         tournament = case_when(str_detect(tournament, "AUS") ~ "Australian Open",
                                str_detect(tournament, "USA") ~ "US Open",
                                str_detect(tournament, "FRA") ~ "French Open",
                                str_detect(tournament, "WIM") ~ "Wimbledon",
                                TRUE ~ NA_character_)) %>% 
  filter(!is.na(tournament)) %>% 
  mutate(gender = "Female")



## ------------------------------------------------------------------------
final_timeline %>% group_by(tournament) %>% count(sort = TRUE)


## ------------------------------------------------------------------------
get_timeline_men <- function(table_num){
  Sys.sleep(5)
  
  url <- "https://en.wikipedia.org/wiki/Tennis_performance_timeline_comparison_(men)"
  
  df <- read_html(url) %>% html_table(fill = TRUE) %>% .[[table_num]]
  
  year_range <- names(df) %>% 
    unique() %>% 
    .[. != "Player"] %>%
    na.omit() %>% 
    as.integer()
  
  year_min <- min(year_range)
  year_max <- max(year_range)
  
  tourn_list <- df %>% janitor::clean_names() %>% slice(1) %>% unlist(., use.names = FALSE) %>% .[!is.na(.)]
  
  first_tourn <- tourn_list[2]
  last_tourn <- tourn_list[length(tourn_list)] 

  
  df %>%
    set_names(nm = paste0(df[1,], "_", names(df))) %>%
    janitor::clean_names("all_caps") %>% 
    select(-matches("NA")) %>% 
    select(player = PLAYER_PLAYER, matches("AUS|FRA|WIM|USA")) %>% 
    select(-matches("NA|`NA`")) %>% 
    filter(player != "Player") %>%
    gather(key = year_tourn, value = outcome,
           paste(first_tourn, year_min, sep = "_"):paste(last_tourn, year_max, sep = "_")) %>%
    separate(col = year_tourn, into = c("tournament", "year"), sep = "_") %>%
    mutate(year = as.integer(year))
}


## ------------------------------------------------------------------------
men_2013_2019 <- read_html("https://en.wikipedia.org/wiki/Tennis_performance_timeline_comparison_(men)")  %>% 
    html_table(fill = TRUE) %>% 
    .[[8]]

clean_2013_2019 <- df_2013_2019 %>% 
  set_names(nm = paste0(df_2013_2019[1,], "_", names(df_2013_2019))) %>%
  filter(Player_Player != "Player") %>%
  select(-31) %>% 
  gather(key = year_tourn, value = outcome,
         paste("AUS", "2013", sep = "_"):paste("AUS", "2019", sep = "_")) %>%
  separate(col = year_tourn, into = c("tournament", "year"), sep = "_") %>%
  rename(player = Player_Player) %>%
  mutate(year = as.integer(year)) %>% 
  select(-contains("2019"))


## ------------------------------------------------------------------------

clean_men_1967_2019 <- 3:10 %>% 
  map(get_timeline_men) %>% 
  bind_rows() %>% 
  filter(year > 1967)

final_timeline_men <- clean_men_1967_2019 %>%  
  mutate(outcome = case_when(outcome == "W" ~ "Won",
                             outcome == "F" ~ "Finalist",
                             outcome == "SF" ~ "Semi-finalist",
                             outcome == "QF" ~ "Quarterfinalist",
                             outcome == "4R" ~ "4th Round",
                             outcome == "3R" ~ "3rd Round",
                             outcome == "2R" ~ "2nd Round",
                             outcome == "1R" ~ "1st Round",
                             outcome == "RR" ~ "Round-robin stage",
                             outcome == "Q2" ~ "Qualification Stage 2",
                             outcome == "Q1" ~ "Qualification Stage 1",
                             outcome == "A" ~ "Absent",
                             str_detect(outcome, "Retired") ~ "Retired",
                             outcome == "-" ~ NA_character_,
                             outcome == "LQ" ~ "Lost Qualifier",
                             TRUE ~ NA_character_),
         tournament = case_when(str_detect(tournament, "AUS") ~ "Australian Open",
                                str_detect(tournament, "USA") ~ "US Open",
                                str_detect(tournament, "FRA") ~ "French Open",
                                str_detect(tournament, "WIM") ~ "Wimbledon",
                                TRUE ~ NA_character_)) %>% 
  filter(!is.na(tournament)) %>% 
  mutate(gender = "Male")



## ------------------------------------------------------------------------
both_timeline <- bind_rows(final_timeline, final_timeline_men) %>% 
  filter(str_length(player) > 4) %>% 
  filter(year <= 2019)

anti_timeline <- both_timeline %>% 
  filter(year == 2019 & tournament != "Australian Open")

combined_timeline <- anti_join(both_timeline, anti_timeline)


## ------------------------------------------------------------------------
write_csv(combined_timeline, "grand_slam_timeline.csv")

