# Read in the dataset :
meteorites <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-11/meteorites.csv")

#Loading the libraries
library(tidyverse)
library(ggrepel)

# Create a map objet with ggplot2
world_map <- ggplot2::borders("world") # require {map} package installed

# Remove outbond longitude
cleaned_data <- meteorites %>% filter(long <300)


# Drawing the plot

options(repr.plot.height = 5.6, repr.plot.res = 450, repr.plot.width = 8) # Plot size and resolution
cleaned_data %>%
ggplot(aes(x = long, y = lat, col = fall)) +
geom_point(size = 0.8, alpha = 1/5) + theme_void() +
geom_label_repel(aes(label=ifelse(mass>2e07, name,''), 
                     fill = fall, size = mass), col = "black") + 
world_map +
labs(title = "World Map of Meteorites \n", subtitle = "The impact zones show where scientists have found meteorites, or the impact craters of meteorites, \n
some dating back as far as the year 2,300BC", caption = " Data source : Meteocritical Society") + 
guides(size = FALSE) +
theme(plot.background=element_rect(fill="#f4f8ff"),
      plot.subtitle = element_text(size = 8),
     plot.caption = element_text(size = 6))
