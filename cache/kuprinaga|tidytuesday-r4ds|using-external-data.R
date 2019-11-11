## how do major R events affect R4DS community activity?

library(tidyverse)
library(lubridate)
library(anytime)
library(magrittr)
library(grid)
library(gridExtra)
library(scales)

r_events <- read_csv('events_data.csv')
r4ds_members <- read_csv('slack_data.csv')

ggplot(data = r4ds_members) +
  geom_line(aes(x = date,
                y = weekly_active_members))

r_events %<>%
  mutate(date_string = str_extract(string = dates, 
                                   pattern = '^([:alpha:]+ *[:digit:]*)'),
         year_string = str_extract(string = year,
                            pattern = '^([:digit:]+)')) %>%
  unite(col = 'date_with_year', date_string, year_string, sep = ' ') %>%
  mutate(date_proper = as.Date(if_else(
      is.na(parse_date_time(date_with_year, '%b %Y')),
      parse_date_time(date_with_year, '%b %d %Y'),
      parse_date_time(date_with_year, '%b %Y')
    )),
  conf_name_no_numbers = str_replace(conf_name, '([:digit:]+)', ''),
  conf_name = str_squish(str_c(conf_name_no_numbers, 
                               ' ', year(date_proper))),
  conf_name_for_label = str_c('Start of ', conf_name)
  ) %>%
  select(conf_name, conf_name_for_label, date_proper) %>%
  mutate(date_proper = if_else(conf_name == 'rstudio::conf 2019', 
                              as.Date('2019-01-17'),
                              date_proper)) %>%
  add_row(date_proper = as.Date('2019-05-31'),
          conf_name = 'Annotations',
          conf_name_for_label = 'Mysterious increase #2') %>%
  add_row(date_proper = as.Date('2017-12-29'),
          conf_name = 'Annotations',
          conf_name_for_label = 'Mysterious increase #1')


data_for_plot <- r4ds_members %>%
  select(date, 
         weekly_active_members) %>%
  left_join(r_events, by = c('date' = 'date_proper'))

conferences_to_highlight <- c('Annotations',
                              'rstudio::conf 2019')

data_for_annotation <- data_for_plot %>%
  filter(conf_name %in% conferences_to_highlight)

useR_annotation <- data_for_plot %>% 
  filter(conf_name == 'useR! 2018')

p <- ggplot(data = data_for_plot) +
  geom_line(aes(x = date,
                y = weekly_active_members),
            color = 'grey',
            size = 1.5) +
  geom_point(data = data_for_plot %>%
                      na.omit(conf_name) %>%
               filter(conf_name != 'Annotations'),
             aes(x = date,
                 y = weekly_active_members),
             size = 10,
             shape = "|", 
             color = 'steelblue') +
  geom_curve(data = data_for_annotation,
             aes(x = date+10,
                 xend = date + 5,
                 y = weekly_active_members - 100,
                 yend = weekly_active_members - 10),
             arrow = arrow(length = unit(0.03, "npc")),
             color = 'slategrey'
             ) + 
  geom_text(data = data_for_annotation,
            aes(x = date + 6,
                y = weekly_active_members - 110,
                label = conf_name_for_label)
  ) +
  geom_curve(data = useR_annotation,
             aes(xend = date - 5,
                 yend = weekly_active_members + 10,
                 y = weekly_active_members + 100,
                 x = date + 10
                 ),
             arrow = arrow(length = unit(0.03, "npc")),
             color = 'slategrey'
             ) + 
  geom_text(data = useR_annotation,
            aes(x = date + 10,
                y = weekly_active_members + 110,
                label = conf_name_for_label)) + 
  scale_x_date(labels = date_format("%B %Y"),
               date_breaks = "3 months") +
  theme(legend.position = 'none') +
  labs(x = '', y = 'Weekly active R4DS members',
       caption = 'Data by tidytuesday and Jumping Rivers 
Visualisation: Anastasia Kuprina (@kuprinasha)',
       subtitle = 'Generally, activity in the community is not always tied to R events. rconf, however, increases activity massively! 
After rconf there were over x2 times more active members for a few weeks.
UseR conference in 2018 had some increase too, although something affected activity already a few days before.') +
  theme_classic()

title = c('Weekly active R4DS members over time with major R events marked in','blue')
colors = c('black', 'steelblue')

gp <- grid.arrange(p, 
             top = tableGrob(t(title), 
                             theme=ttheme_minimal(padding=unit(c(3,2,3,0),'mm'),
                                                  base_colour = colors,
                                                  base_size = 20)))

ggsave('plot.png', gp,
       width = 25,
       height = 15,
       units = 'cm')
