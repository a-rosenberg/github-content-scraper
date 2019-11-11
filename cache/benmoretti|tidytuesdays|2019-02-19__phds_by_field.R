#' phds_by_field.R
#' 
#' R script for 19 February 2019 #TidyTuesday 
#'


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(collapsibleTree)
library(skimr)
library(magrittr)
library(tidyquant)


# Configuration -----------------------------------------------------------

# define source url
source_url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-19/phd_by_field.csv"


# Gather ------------------------------------------------------------------

# read + tidy
phd_by_field_tbl <- read_csv(source_url) %>%
  replace_na(list(n_phds = 0)) %>%
  mutate_if(is.character, factor)
  

# Visualise ---------------------------------------------------------------

# collapsible tree for top two levels in hierarchy
phd_by_field_tbl %>%
  group_by(broad_field, major_field) %>% 
  summarise(
    n_major_field_phds = sum(n_phds)
  ) %>%
  ungroup() %>%
  collapsibleTreeSummary(
    hierarchy = c("broad_field", "major_field"),
    attribute = "n_major_field_phds",
    nodeSize = "n_major_field_phds",
    fillFun = colorspace::rainbow_hcl,
    collapsed = FALSE
  )

# heat map Heatmap for number of PhDs in Agricultural sciences and natural resources
phd_by_field_tbl %>%
  mutate(
    date = lubridate::make_date(year=year)
  ) %>%
  filter(major_field == "Agricultural sciences and natural resources") %>%
  arrange(field) %>%
  ggplot(aes(date, field)) +
  geom_raster(aes(fill=n_phds)) +
  theme_tq() +
  scale_fill_gradient(low = palette_light()[[1]], high = palette_light()[[5]]) +
  labs(
    title = "#TidyTuesday, 19 February 2019",
    subtitle = "Heatmap for number of PhDs in Agricultural sciences and natural resources",
    x = "Year",
    y = "",
    caption = "By @benmoretti"
  )

# Annual relative difference plot of PhDs in Agricultural sciences and natural resources
phd_by_field_tbl %>%
  group_by(broad_field, major_field, field) %>%
  arrange(year) %>%
  mutate(
    previous_year_n_phds = lag(n_phds),
    annual_change_pc = (n_phds - previous_year_n_phds) / previous_year_n_phds
  ) %>%
  select(-previous_year_n_phds) %>%
  filter(! is.na(annual_change_pc)) %>%
  mutate(
    date = lubridate::make_date(year=year)
  ) %>%
  filter(major_field == "Agricultural sciences and natural resources") %>%
  ggplot(aes(date, annual_change_pc)) +
  geom_point() +
  geom_smooth(colour=palette_light()[[6]]) +
  geom_segment(aes(yend=annual_change_pc, xend=date)) +
  scale_y_continuous(labels=scales::percent) +
  theme_tq() +
  facet_wrap(vars(field), scales="free_y") +  
  labs(
    title = "#TidyTuesday, 19 February 2019",
    subtitle = "Annual relative difference plot of PhDs in Agricultural sciences and natural resources",
    x = "Year",
    y = "Change",
    caption = "By @benmoretti"
  )

