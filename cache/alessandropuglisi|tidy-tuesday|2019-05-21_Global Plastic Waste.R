## Load packages
library(tidyverse)
library(extrafont)

## Get the data
mismanaged_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv")
waste_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-plastic-waste-vs-gdp-per-capita.csv")

## Join dataframes, clean up column names, reorder columns and add a ratio
global_plastic_waste <- mismanaged_vs_gdp %>%
  left_join(waste_vs_gdp) %>%
  select(-c(Code, `GDP per capita, PPP (constant 2011 international $) (Rate)`)) %>%
  set_names(
    ~ str_to_lower(.) %>%
    str_remove_all("(\\(.+$|,.+$)") %>%
    str_remove_all(" $") %>%
    str_replace_all(" ", "_")
  ) %>%
  na.omit() %>%
  select(entity, year, per_capita_mismanaged_plastic_waste, 
         per_capita_plastic_waste, gdp_per_capita, total_population) %>%
  mutate(mismanaged_plastic_waste_ratio = round(per_capita_mismanaged_plastic_waste/per_capita_plastic_waste, digits = 3), 
         gdp_per_capita = round(gdp_per_capita, digits = 2))

## Plot
ggplot(global_plastic_waste, aes(x = gdp_per_capita, y = mismanaged_plastic_waste_ratio, label = entity)) +
  geom_jitter(colour = "#ce1e11") +
  scale_x_continuous(breaks = seq(0, 120000, 20000)) +
  labs(
    title = "(Mis)managing plastic waste around the world (2010)",
    caption = "Data source: https://ourworldindata.org/plastic-pollution \n @_apuglisi_ #TidyTuesday Week 21",
    x = "GDP per capita ($)",
    y = "Mismanaged plastic waste ratio"
  ) +
  theme(
    plot.margin = unit(c(0.5,0.5,0.3,0.5), 'cm'),
    plot.background = element_rect(fill = "#ce1e11"),
    panel.background = element_rect(fill = "#11ce44"),
    plot.title = element_text(family = "Berlin Sans FB", colour = "#ffffff", size = 17),
    plot.caption = element_text(family = "Berlin Sans FB", colour = "#ffffff", size = 10),
    axis.title.x =  element_text(family = "Berlin Sans FB", colour = "#ffffff", size = 13, vjust = -0.5),
    axis.title.y = element_text(family = "Berlin Sans FB", colour = "#ffffff", size = 13, vjust = 2),
    axis.text = element_text(family = "Berlin Sans FB", colour = "#ffffff"),
    axis.ticks = element_line(colour = "#ffffff")
  )
