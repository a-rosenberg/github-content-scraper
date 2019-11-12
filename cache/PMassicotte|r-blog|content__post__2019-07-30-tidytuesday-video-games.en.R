## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  cache = TRUE,
  dpi = 300,
  out.width = "100%",
  fig.align = "center",
  fig.width = 8,
  fig.asp = 0.618, # 1 / phi
  fig.show = "hold",
  dev = "svg",
  message = FALSE
)

library(tidyverse)
library(ggpmthemes)
library(glue)
theme_set(theme_poppins())


## ------------------------------------------------------------------------
video_games <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-30/video_games.csv") %>%
  mutate(release_date = as.Date(release_date, "%b %d, %Y")) %>%
  distinct(game, developer, publisher, .keep_all = TRUE)


## ------------------------------------------------------------------------
video_games %>%
  top_n(16, average_playtime) %>%
  mutate(game = glue("{game} ({lubridate::year(release_date)})")) %>%
  mutate(game = fct_reorder(game, average_playtime)) %>%
  ggplot(aes(x = game, y = average_playtime / 60)) +
  geom_col() +
  coord_flip() +
  xlab(NULL) +
  ylab("Average played time (hours)") +
  labs(title = str_wrap("Average played time for the last two weeks", 25)) +
  labs(subtitle = "Only the top 16 averaged played game are shown")


## ------------------------------------------------------------------------
equal_breaks <- function(n = 3, s = 0.05, ...) {
  function(x) {
    # rescaling
    d <- s * diff(range(x)) / (1 + 2 * s)
    seq(min(x) + d, max(x) - d, length = n)
  }
}

video_games %>%
  drop_na(metascore) %>%
  add_count(publisher) %>%
  filter(dense_rank(desc(n)) <= 6) %>%
  group_by(year = lubridate::year(release_date), publisher) %>%
  summarise(mean_metascore = mean(metascore), sd_metascore = sd(metascore)) %>%
  ggplot(aes(x = year, y = mean_metascore)) +
  geom_line(size = 2) +
  facet_wrap(~publisher, scale = "free_x") +
  scale_x_continuous(
    labels = function(x) floor(x),
    breaks = equal_breaks(n = 4, s = 0.05)
  ) +
  xlab(NULL) +
  ylab("Median metascore") +
  theme(legend.position = "none") +
  theme(panel.spacing = unit(2, "lines")) +
  labs(title = "Time series of metascore by publisher") +
  labs(subtitle = "Only the six publishers with the highest number of release are shown")


## ------------------------------------------------------------------------
video_games %>%
  drop_na(release_date) %>%
  group_by(year = lubridate::year(release_date), month = lubridate::month(release_date, label = TRUE)) %>%
  summarise(medan_price = median(price, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = month, fill = medan_price)) +
  geom_tile() +
  scale_fill_viridis_c(option = "A", labels = scales::dollar) +
  coord_equal() +
  scale_x_continuous(expand = c(0, 0), breaks = seq(2000, 2020, by = 2)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(fill = "Median\nprice (USD)") +
  xlab(NULL) +
  ylab(NULL) +
  labs(title = "Price evolution of games") +
  labs(subtitle = "Median price calculated monthly and yearly")


## ------------------------------------------------------------------------
video_games %>%
  filter(
    lubridate::year(release_date) == 2006 &
      lubridate::month(release_date) == 11
  ) %>%
  select(game, release_date, price) %>%
  arrange(desc(price)) %>%
  knitr::kable()


## ------------------------------------------------------------------------
video_games %>%
  top_n(5, price) %>%
  mutate(game = glue("{game} ({lubridate::year(release_date)})")) %>%
  mutate(game = str_wrap(game, 30)) %>%
  mutate(game = fct_reorder(game, price)) %>%
  ggplot(aes(x = game, y = price)) +
  geom_col() +
  coord_flip() +
  xlab(NULL) +
  ylab("Price (USD)") +
  labs(title = "Top priced games") +
  labs(subtitle = "Only shows the top 5 most expensive games") +
  scale_y_continuous(labels = scales::dollar)

