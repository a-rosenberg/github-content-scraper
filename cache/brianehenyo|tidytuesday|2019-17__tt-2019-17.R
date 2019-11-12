## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(paletteer)
library(harrypotter)
library(ghibli)

raw_anime <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/raw_anime.csv")


## ------------------------------------------------------------------------
tidy_anime <- raw_anime %>% 
  mutate(producers = str_remove(producers, "\\["),
         producers = str_remove(producers, "\\]")) %>% 
  unnest(producers = strsplit(producers, ",")) %>% 
  mutate(producers = str_remove(producers, "\\'"),
         producers = str_remove(producers, "\\'")) %>% 
  mutate(genre = str_remove(genre, "\\["),
         genre = str_remove(genre, "\\]")) %>% 
  unnest(genre = strsplit(genre, ",")) %>% 
  mutate(genre = str_remove(genre, "\\'"),
         genre = str_remove(genre, "\\'")) %>% 
  mutate(studio = str_remove(studio, "\\["),
         studio = str_remove(studio, "\\]")) %>% 
  unnest(studio = strsplit(studio, ",")) %>% 
  mutate(studio = str_remove(studio, "\\'"),
         studio = str_remove(studio, "\\'")) %>% 
  mutate(aired = str_remove(aired, "\\{"),
         aired = str_remove(aired, "\\}"),
         aired = str_remove(aired, "'from': "),
         aired = str_remove(aired, "'to': "),
         aired = word(aired, start = 1, 2, sep = ",")) %>% 
  separate(aired, into = c("start_date", "end_date"), sep = ",") %>% 
  mutate(start_date = str_remove_all(start_date, "'"),
         start_date = str_sub(start_date, 1, 10),
         end_date = str_remove_all(start_date, "'"),
         end_date = str_sub(end_date, 1, 10)) %>%
  mutate(start_date = lubridate::ymd(start_date),
         end_date = lubridate::ymd(end_date)) %>% 
  # Drop unranked or unpopular series
  filter(rank != 0,
         popularity != 0) %>% 
  # Change text for ratings
  mutate(rating = case_when(rating == "G - All Ages" ~ "G", 
                             rating == "None" ~ "None",
                             rating == "PG - Children" ~ "PG",
                             rating == "PG-13 - Teens 13 or older" ~ "PG-13",
                             rating == "R - 17+ (violence & profanity)" ~ "R",
                             rating == "R+ - Mild Nudity" ~ "R+")) %>% 
  mutate(rating = as.character(rating)) %>% 
  mutate(rating = factor(rating, 
                          levels=c("None", "G", "PG",
                                   "PG-13","R","R+")))


## ------------------------------------------------------------------------
bg_color = paletteer_d(ghibli, MarnieLight1)[7]
text_color = paletteer_d(ghibli, MarnieLight1)[2]

tidy_anime %>% 
  filter(scored_by > 1000) %>%
  distinct(animeID, .keep_all = TRUE) %>% 
  ggplot(aes(x = favorites, y = score)) + 
  geom_point(alpha = 0.4, aes(color = type, size = scored_by)) +
  geom_text(aes(x = 215, 
                y = 2.32, 
                label = "Hametsu no Mars performs \npoorly even with 1 episode."), 
            size = 2, 
            colour = text_color, 
            hjust = 0, 
            family = "Anime Ace v02", 
            nudge_x = 0.05) +
  theme_tufte(ticks = FALSE) +
  scale_color_hp_d(option = "Ravenclaw", direction = -1) +
  scale_x_log10() +
  scale_size_continuous(breaks = seq(100000, 500000, 100000),
                        labels = paste0(seq(100, 500, 100), "k")) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
  labs(x = "Favorites (# of fans)", y = "Score",
       title = "So bad, it's good",
       subtitle = "High scoring animes usually appear in many fans' favorite lists but Hametsu no Mars is still a favorite\ndespite its low score of 2. It is allegedly a rip-off of the Neon Genesis Evangelion. ",
       caption = "Only included animes scored by >1,000 MyAnimeList members\nVis by Briane Samson",
       size = "Circles represent the number of scores received") +
  theme(text = element_text(color = "white", family = "Anime Ace v02"),
        plot.title = element_text(size = rel(1.25), colour = text_color),
        plot.subtitle = element_text(size = rel(.8), family = "Avenir", 
                                     colour = text_color),
        plot.background = element_rect(fill = bg_color),
        plot.caption = element_text(size = rel(.5), family = "Avenir", colour = text_color),
        panel.grid = element_line(color = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title = element_text(color = text_color, size = rel(.6)),
        axis.text = element_text(color = text_color, size = rel(.6)),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.background = element_rect(fill = bg_color, colour = text_color),
        legend.key = element_rect(fill = bg_color, colour = bg_color),
        legend.text = element_text(size = rel(0.5), colour = text_color),
        legend.title = element_text(size = rel(0.5), colour = text_color)) +
  guides(colour = FALSE,
         fill = FALSE,
         size = guide_legend(title.position = "top", title.hjust = 0.5, 
                             override.aes = list(colour = text_color, alpha = 1)))

ggsave("score_vs_favorite.png", width=8, height=5)


## ------------------------------------------------------------------------
bg_color = paletteer_d(ghibli, MarnieLight1)[2]

tidy_anime %>% 
  filter(scored_by > 1000) %>%
  distinct(animeID, .keep_all = TRUE) %>% 
  group_by(name) %>% 
  ggplot(aes(x = rating, y = score)) + 
  geom_jitter(aes(size = scored_by, fill = rating), 
              alpha = 0.3, width = 0.2, shape = 21, color = bg_color) +
  geom_boxplot(aes(fill = rating), colour = "white", show.legend = FALSE, 
               outlier.shape = NA, alpha = 0.4) +
  theme_tufte(ticks = FALSE) +
  scale_fill_hp_d(option = "NewtScamander") +
  scale_size_continuous(breaks = seq(100000, 500000, 100000),
                        labels = paste0(seq(100, 500, 100), "k")) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
  labs(x = "", y = "Score",
       title = "A Slice of life and violence",
       subtitle = "PG-13 and R animes are scored more and rank high among fans",
       caption = "Only included animes scored by >1,000 MyAnimeList members\nVis by Briane Samson",
       size = "Circles represent the number of scores received") +
  theme(text = element_text(color = "white", family = "Anime Ace v02"),
        plot.title = element_text(size = rel(1.25)),
        plot.subtitle = element_text(size = rel(.8), family = "Avenir"),
        plot.background = element_rect(fill = bg_color),
        plot.caption = element_text(size = rel(.5), family = "Avenir"),
        panel.grid = element_line(color = "gray20"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title = element_text(color = "white", size = rel(.6)),
        axis.text = element_text(color = "white", size = rel(.6)),
        legend.position = c(0.5, 0.135),
        legend.direction = "horizontal",
        legend.background = element_rect(fill = bg_color, colour = "gray40"),
        legend.key = element_rect(fill = bg_color, colour = bg_color),
        legend.text = element_text(size = rel(0.5)),
        legend.title = element_text(size = rel(0.5))) +
  guides(colour = FALSE,
         fill = FALSE,
         size = guide_legend(title.position = "top", title.hjust = 0.5, 
                             override.aes = list(colour = "white", alpha = 1)))

ggsave("score_vs_rating.png", width=8, height=5)


## ------------------------------------------------------------------------
bg_color = paletteer_d(ghibli, MarnieLight1)[7]
text_color = paletteer_d(ghibli, MarnieLight1)[2]

tidy_anime %>% 
  filter(scored_by > 1000) %>%
  distinct(animeID, .keep_all = TRUE) %>% 
  group_by(name) %>% 
  ggplot(aes(x = type, y = score)) + 
  geom_jitter(aes(size = scored_by, fill = type), 
              alpha = 0.3, width = 0.4, shape = 21, color = bg_color) +
  geom_boxplot(aes(fill = type), colour = text_color, show.legend = FALSE, 
               outlier.shape = NA, alpha = 0.4) +
  theme_tufte(ticks = FALSE) +
  scale_fill_hp_d(option = "LunaLovegood") +
  scale_size_continuous(breaks = seq(100000, 500000, 100000),
                        labels = paste0(seq(100, 500, 100), "k")) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
  labs(x = "", y = "Score",
       title = "Movies and TV shows reign",
       subtitle = "",
       caption = "Only included animes scored by >1,000 MyAnimeList members\nVis by Briane Samson",
       size = "Circles represent the number of scores received") +
  theme(text = element_text(color = text_color, family = "Anime Ace v02"),
        plot.title = element_text(size = rel(1.25)),
        plot.subtitle = element_text(size = rel(.8)),
        plot.background = element_rect(fill = bg_color),
        plot.caption = element_text(size = rel(.5)),
        panel.grid = element_line(color = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title = element_text(color = text_color, size = rel(.6)),
        axis.text = element_text(color = text_color, size = rel(.6)),
        legend.position = c(0.5, 0.135),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.key = element_rect(fill = bg_color, colour = bg_color),
        legend.text = element_text(size = rel(0.5)),
        legend.title = element_text(size = rel(0.5))) +
  guides(colour = FALSE,
         fill = FALSE,
         size = guide_legend(title.position = "top", title.hjust = 0.5, 
                             override.aes = list(colour = text_color, alpha = 1)))

ggsave("score_vs_type.png", width=8, height=5)

