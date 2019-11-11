
library(tidyverse)
library(lubridate)
library(anytime)
library(magrittr)
library(scales)

r4ds_members <- read_csv('slack_data.csv')

annotations_df <- tibble(date = as.Date('2019-01-17'), 
       annotation = 'Start of rstudio::conf 2019') %>%
  add_row(date = as.Date('2019-05-31'),
          annotation = 'Mysterious increase #2') %>%
  add_row(date = as.Date('2017-12-29'),
          annotation = 'Mysterious increase #1') %>%
  add_row(date = as.Date('2018-07-10'),
          annotation = 'Start of userR 2018')


data_for_plot <- r4ds_members %>%
  select(date, 
         weekly_active_members) %>%
  left_join(annotations_df, by = c('date' = 'date'))


bottom_annotations <- data_for_plot %>%
  filter(annotation != 'Start of userR 2018')

user_annotation <- data_for_plot %>%
  filter(annotation == 'Start of userR 2018')

ggplot(data = data_for_plot) +
  geom_line(aes(x = date,
                y = weekly_active_members,
                ),
            color = 'grey',
            size = 1.5) +
  geom_point(data = data_for_plot %>%
               filter(is.na(annotation) == F),
             aes(x = date,
                 y = weekly_active_members),
             size = 2,
             color = 'steelblue') +
  geom_curve(data = bottom_annotations,
             aes(x = date+10,
                 xend = date + 5,
                 y = weekly_active_members - 100,
                 yend = weekly_active_members - 10),
             arrow = arrow(length = unit(0.03, "npc")),
             color = 'slategrey'
  ) + 
  geom_text(data = bottom_annotations,
            aes(x = date + 10,
                y = weekly_active_members - 110,
                label = annotation)
  ) +
  geom_curve(data = user_annotation,
             aes(xend = date - 5,
                 yend = weekly_active_members + 10,
                 y = weekly_active_members + 100,
                 x = date + 10
             ),
             arrow = arrow(length = unit(0.03, "npc")),
             color = 'slategrey'
  ) + 
  geom_text(data = user_annotation,
            aes(x = date + 10,
                y = weekly_active_members + 120,
                label = annotation)) + 
  scale_x_date(labels = date_format("%B %Y"),
               date_breaks = "3 months") +
  theme(legend.position = 'none') +
  labs(x = '', y = 'Weekly active R4DS members',
       title = 'Weekly active R4DS members over time',
       subtitle = 'There is certainly seasonality in the activity, mostly peaking in the beginning of the week.
Additionally, major events like rconf result in big increases of new joiners and weekly actives') +
  theme_classic()

