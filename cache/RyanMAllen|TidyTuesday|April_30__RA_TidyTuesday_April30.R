library(tidyverse)
library(data.table)
set.seed(12)
library(ggplot2)
library(lubridate)
library(here)

bird_collisions <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-30/bird_collisions.csv")
mp_light <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-30/mp_light.csv")

sbird <- bird_collisions[bird_collisions$locality == 'MP']

dt <- na.omit(dplyr::left_join(mp_light, sbird, by = 'date'))

dt <- as.data.table(dt)
head(dt)
str(dt)

dt$date <- as.Date(dt$date, "%Y-%m-%d")
str(dt)

library(dplyr)
library(ggplot2)
dt %>% filter(date > "2001-01-01" & date <"2001-12-31") %>% group_by(genus) %>% 
  count(light_score, genus) %>% 
  ggplot(aes(x=light_score, y=n, col=genus)) + geom_point(aes(col=genus))+ geom_line(aes(col=genus))

dt %>% add_count(light_score) %>% 
  group_by(date) %>% 
  ggplot(aes(x=date, y=n)) + geom_point()+ geom_line() %>% 
  facet_wrap(facet = 'light_score', ncol = 3)

deadliest_days <- dt %>% 
  add_count(date) %>% 
  group_by(genus, date) %>% 
  select(date, light_score,genus, n) %>%
  arrange(desc(n)) %>% 
  slice(1:10)

ggplot(deadliest_days, aes(x=date, y= n, col = genus))+ geom_bar(stat = "identity")


# plot_data <- bird_collisions %>%
#   filter(locality == "CHI") %>% 
#   mutate(month = month(date),
#          year = year(date)) %>% 
#   unite("binomial_name", genus, species, sep = " ") %>% 
#   count(year, month, binomial_name) %>% 
#   complete(nesting(year, binomial_name), month = 1:12, fill = list(n = 0)) %>% 
#   group_by(year, binomial_name) %>% 
#   mutate(percent = n/sum(n)) %>% 
#   mutate(percent = ifelse(is.nan(percent), 0, percent))
# 
# flower <- ggplot(plot_data, aes(x = month, y = percent, fill = binomial_name)) +
#   geom_area(size = 0, position = position_dodge(), alpha = 0.2) +
#   scale_x_continuous(labels = month.abb, breaks = 1:12) +
#   scale_y_continuous(limits = c(0,1), breaks = c(0.5, 0.1)) +
#   scale_fill_viridis_d("Year", option = "plasma", direction = 1) +
#   scale_color_viridis_d(option = "plasma", direction = 1) +
#   guides(fill = guide_colorbar()) +
#   coord_polar() +
#   labs(x = NULL,
#        y = NULL,
#        title = "Overall") +
#   #theme_jk(dark = FALSE, grid = "X", strip_text_size = 10, plot_title_size = 14) +
#   theme(axis.text = element_blank(),
#         legend.position = "none")
# 
# petals <- flower +
#   aes(group = year) +
#   geom_path(aes(color = binomial_name), size = 0.2, show.legend = FALSE) +
#   labs(title = "By Species") +
#   facet_wrap(~binomial_name, labeller = label_wrap_gen(10), nrow = 7) 
# 
# legend <- plot_data %>% 
#   filter(binomial_name == "Setophaga fusca") %>% 
#   ggplot(aes(x = month, y = percent, fill = binomial_name, group = year)) +
#   geom_area(size = 0, position = position_dodge(), alpha = 0.1) +
#   geom_path(aes(color = binomial_name), size = 0.2, show.legend = FALSE) +
#   annotate("text", x = 11.2, y = 0.8, label = "One year of\ncollisions in October", family = "Scope One", size = 3, hjust = 0) +
#   annotate("segment", x = 10.8, y = 0.8, xend = 10, yend = 0.8, arrow = arrow(length = unit(0.2, "cm"))) +
#   annotate("text", x = 3.5, y = 0.8, label = "Multiple years of\ncollisions in May", family = "Scope One", size = 3) +
#   annotate("segment", x = 3.8, y = 0.8, xend = 5, yend = 0.8, arrow = arrow(length = unit(0.2, "cm"))) +
#   scale_x_continuous(labels = month.abb, breaks = 1:12) +
#   scale_y_continuous(limits = c(0,1), breaks = c(0.5, 0.1)) +
#   scale_fill_viridis_d("Year", option = "plasma", direction = 1) +
#   scale_color_viridis_d(option = "plasma", direction = 1) +
#   labs(x = NULL,
#        y = NULL,
#        title = "How to Interpret This Chart",
#        subtitle = str_wrap("A flower represents the recorded total collisions of each bird species with the individual petals representing the normalized events during each year (from 0-1).  The position of the petals indicates the month or months collisions occur, with overlaps indicating repeated year-over-year collisions.", 70)) +
#   guides(fill = guide_colorbar()) +
#   coord_polar(theta = "x", start = 0) +
#   #theme_jk(dark = FALSE, grid = "XY", plot_title_size = 14) +
#   theme(axis.text.y = element_blank(),
#         legend.position = "none")
# 
# out <- wrap_plots(flower / legend, petals, ncol = 2, widths = c(1, 2)) +
#   plot_annotation(title = "Seasonality of Bird Collisions in Chicago",
#                   subtitle = str_wrap("Presented below is a petal chart of of bird collisions, with instructions on how to interpret this chart in the lower left.  The upper left flower represents collisions recorded across all years and species, with individual species presented as small multiple flowers on the right.", 220),
#                   caption = "Data: Winger et al. (2019) Nocturnal flight-calling behaviour predicts vulnerability to artificial light in migratory birds. Proceedings of the Royal Society B 286(1900): 20190364. https://doi.org/10.1098/rspb.2019.0364 | Graphic: @jakekaupp")
