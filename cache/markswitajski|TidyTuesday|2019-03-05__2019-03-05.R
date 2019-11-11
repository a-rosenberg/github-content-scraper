setwd("~/TidyTuesday/2019-03-05")
library(tidyverse)
library(cowplot)

jobs_gender <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/jobs_gender.csv")
earnings_female <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/earnings_female.csv") 
employed_gender <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/employed_gender.csv") 

plot1 <- 
  ggplot(employed_gender, aes(x = year)) +
  geom_line(aes(y = full_time_female), color = "darkgreen", size = 2) +
  geom_line(aes(y = full_time_male), color = "goldenrod", size = 2) +
  geom_ribbon(aes(ymin = (full_time_female + 0.5), ymax = (full_time_male - 0.5)), fill = "lightblue", alpha = 0.5) +
  
  geom_text(aes(x = 1990, y = 95, label = "Men"), color = "goldenrod") + 
  geom_text(aes(x = 1990, y = 70, label = "Women"), color = "darkgreen") +
  
  ylim(0, 100) +
  
  theme_minimal(16) +
  theme(plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 13.5, color = "gray40", face = "bold"),
        axis.title.x = element_text(size = 15, color = "gray40", face = "bold"),
        axis.title.y = element_text(size = 15, color = "gray40", face = "bold")) +
  
  labs(
    x = '',
    y = 'Percent of Full-Time Workers'
  )
  

plot2 <- 
  ggplot(employed_gender, aes(x = year)) +
  geom_line(aes(y = part_time_female), color = "darkgreen", size = 2) +
  geom_line(aes(y = part_time_male), color = "goldenrod", size = 2) +
  geom_ribbon(aes(ymin = (part_time_male + 0.5), ymax = (part_time_female - 0.5)), fill = "lightblue", alpha = 0.5) +
  
  geom_text(aes(x = 1990, y = 7, label = "Men"), color = "goldenrod") + 
  geom_text(aes(x = 1990, y = 32, label = "Women"), color = "darkgreen") +
  
  ylim(0, 100) +
  
  theme_minimal(16) +
  theme(plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 13.5, color = "gray40", face = "bold"),
        axis.title.x = element_text(size = 15, color = "gray40", face = "bold"),
        axis.title.y = element_text(size = 15, color = "gray40", face = "bold")) +
  
  labs(
    x = '',
    y = 'Percent of Part-Time Workers'
  )

p <- plot_grid(plot1, plot2)

title <- ggdraw() + draw_label("Gender gap between full-time and part-time employees", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))


ggsave(filename = "20190305_Full_and_Part_Time_Rates_by_Gender.png", width=16, height=10, units="cm", scale=1.6)
