### Load libraries
library(tidyverse)
library(ggplot2)
library(extrafont)
library(ggthemes)
loadfonts(device = "win")

### Get the data
student_ratio <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-07/student_teacher_ratio.csv")

### Subset student_ratio
ratio_all <- student_ratio %>%
  rename(ratio = student_ratio) %>%
  select(-c(edulit_ind, country_code, flag_codes, flags)) %>%
  na.omit()

### Plot
ratio_all %>%
  filter(country %in% c("Italy", "Greece", "Spain", "Portugal")) %>%
  
  ggplot(aes(x = year, y = ratio, color = indicator)) +
  geom_line(size = 1) +
  geom_point(size = 3.5) +
  facet_wrap(~ country, scales = "free") +
  labs(
    title = "Student-teacher ratios in Greece, Italy, Portugal and Spain: 2012-2016",
    x = "Year",
    y = "Student-teacher ratio",
    caption = "Data source: UNESCO Institute of Statistics\n @_apuglisi #TidyTuesday Week 19") +
  theme_solarized_2() +
  theme(
    axis.title = element_text(size = 15, family = "Courier New", face = "bold"),
    axis.text = element_text(size = 13, family = "Courier New"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.spacing.x = unit(0.5, 'cm'),
    legend.text = element_text(size = 13, family = "Courier New"),
    panel.spacing.x = unit(0.5, 'cm'),
    panel.spacing.y = unit(1, 'cm'),
    plot.margin = unit(c(0.5,1,0.5,1), 'cm'),
    plot.title = element_text(size = 17, family = "Courier New", face = "bold"),
    strip.text.x = element_text(size = 15, family = "Courier New", face = "bold")
  )
  