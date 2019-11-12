## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------
library(tidyverse)

theme_set(theme_light())


## ------------------------------------------------------------------------
small_trains <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-26/small_trains.csv") 


## ------------------------------------------------------------------------
glimpse(small_trains)


## ------------------------------------------------------------------------
trains_processed <- small_trains %>%
  mutate(date = lubridate::ymd(paste(year, month, 1, sep = "-")))

summary(trains_processed$date)


## ------------------------------------------------------------------------
trains_processed %>%
  group_by(date) %>%
  summarise(late_total = sum(num_arriving_late, na.rm = TRUE)) %>%
  ggplot(aes(date, late_total)) +
  geom_line() + 
  geom_smooth(method = "lm") +
  labs(x = "",
       y = "# of trains arriving late",
       title = "French trains arriving late",
       subtitle = "Increasing from Jan 2015 to Nov 2018",
       caption = "Designer: Tony Galvan @gdatascience1  |  Source: SNCF")


## ------------------------------------------------------------------------
trains_processed %>%
  group_by(date) %>%
  summarise(pct_late = sum(num_arriving_late, na.rm = TRUE) / 
              sum(total_num_trips, na.rm = TRUE)) %>%
  ggplot(aes(date, pct_late)) +
  geom_line() + 
  scale_y_continuous(labels = scales::percent_format()) +
  geom_smooth(method = "lm") +
  labs(x = "",
       y = "% of trains arriving late",
       title = "Percentage of French trains arriving late",
       subtitle = "Increasing from Jan 2015 to Nov 2018",
       caption = "Designer: Tony Galvan @gdatascience1  |  Source: SNCF")


## ------------------------------------------------------------------------
avg_pct_late <- sum(trains_processed$num_arriving_late, na.rm = TRUE) / 
  sum(trains_processed$total_num_trips, na.rm = TRUE)

p <- trains_processed %>%
  mutate(month_name = month.name[month]) %>%
  group_by(month_name) %>%
  summarise(pct_late = sum(num_arriving_late, na.rm = TRUE) / 
              sum(total_num_trips, na.rm = TRUE)) %>%
  mutate(month_name = fct_reorder(month_name, pct_late),
         above_or_below_avg = if_else(pct_late > avg_pct_late, "above", "below")) %>%
  ggplot(aes(month_name, pct_late, fill = above_or_below_avg)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("#0568ae", "#d2d2d2")) +
  geom_hline(yintercept = avg_pct_late, linetype = 2) +
  annotate("text", x = "January", y = avg_pct_late + 0.005, angle = 90, 
           size = 3, label = "13.5% average") +
  coord_flip() +
  labs(x = "",
       y = "% of trains arriving late",
       title = "% of French trains arriving late",
       subtitle = "Summer and Fall trains are late more often",
       caption = "Designer: Tony Galvan @gdatascience1  |  Source: SNCF")
p
ggsave("sncf.png", p)

