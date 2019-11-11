###############################
#                             #
#       Tidy Tuesday          #
#        2018-04-03           #
#                             #
###############################

library(tidyverse)
library(readxl)
library(janitor)
library(ggbeeswarm)
library(scales)
library(viridis)

tuition <-
  read_excel(".//data//us_avg_tuition.xlsx") %>% 
  clean_names()

tuition_tidy <-
  tuition %>%
  gather("year", "value", -1) %>% 
  mutate(year = paste0("20", str_replace(year, ".*_", "")))

tuition_plot <-
  tuition_tidy %>%
  ggplot(aes(y = value, 
             x = year, 
             color = value)) +
  geom_beeswarm(size = 2) +
  theme_minimal() +
  scale_color_viridis() + 
  scale_y_continuous(labels = dollar_format(prefix = "$")) +
  labs(x = "YEAR", 
       y = "TUITION", 
       title = "US TUITION COSTS",
       subtitle = "Distribution of States, 2005-2016",
       caption = "Source: https://onlinembapage.com/average-tuition-and-educational-attainment-in-the-united-states/") +
  theme(title = element_text(size = 16),
        plot.subtitle = element_text(size = 12, family = "serif", face = "italic"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text = element_text(size = 8, face = "italic"),
        plot.caption = element_text(size = 7, face = "italic"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.position = "none"
  )

tuition_plot

ggsave(filename = "./plot/tuition_plot.png", plot = tuition_plot, width = 17.04, height = 7.69)
