## ----setup---------------------------------------------------------------
if (! "fivethirtyeight" %in% rownames(installed.packages())) {
  install.packages("fivethirtyeight")
}
data(airline_safety, package = "fivethirtyeight")
library(tidyverse)


## ----calculate safety scores---------------------------------------------
airline_safety %>%
  # restructure variables
  gather("variable", "value", ends_with("_85_99"), ends_with("_00_14")) %>%
  mutate(variable = str_replace(variable, "([a-z])_([0-9])", "\\1___\\2")) %>%
  separate("variable", c("measure", "interval"), sep = "___") %>%
  # calculate airline safety scores
  group_by(measure) %>%
  mutate(meas_mean = mean(value), meas_cent = meas_mean - value) %>%
  # note: must include `measure` again here
  mutate(meas_mult = meas_cent * sqrt(avail_seat_km_per_week)) %>%
  mutate(meas_std = (meas_mult - mean(meas_mult)) / sd(meas_mult)) %>%
  ungroup() %>%
  # note: remove `value` to avoid confusion (`NA`s)
  select(-value, -meas_mean, -meas_cent, -meas_mult) ->
  airline_std


## ----reorganize data-----------------------------------------------------
airline_std %>%
  # generate scatterplots of safety scores
  spread(interval, meas_std) %>%
  rename_at(vars(matches("[0-9]{2}_[0-9]{2}")), funs(paste0("std_", .))) %>%
  # average standardized scores across all three measures
  group_by(airline) %>%
  summarize_at(c("std_85_99", "std_00_14"), mean) %>%
  ungroup() ->
  airline_scores


## ----generate scatterplots-----------------------------------------------
airline_scores %>%
  ggplot(aes(x = std_85_99, y = std_00_14, label = airline)) +
  theme_bw() +
  coord_fixed() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "1985-99", y = "2000-14") +
  geom_point(alpha = .5) +
  ggtitle("Safety scores: first versus second interval") ->
  airline_scatterplot
print(airline_scatterplot)


## ----save scatterplot----------------------------------------------------
ggsave("airline-scatterplot.jpg", airline_scatterplot, height = 4, width = 7)

