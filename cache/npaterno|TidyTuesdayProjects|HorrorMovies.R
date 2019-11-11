# Load Libraries
library(tidyverse)
library(extrafont)
library(ggthemes)

# Set up fonts
font_import()
loadfonts(device = "win")

# Read Data
raw_data <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-22/horror_movies.csv")

# Tidy runtime data
horror_movies<-raw_data%>%
  separate(movie_run_time, 
           into = c("movie_length", "drop"), 
           sep = " ", 
           convert = TRUE)%>%
  select(-drop)

# Plot heat map of length v rating
p<-ggplot(horror_movies, aes(movie_length, review_rating))+
  geom_hex(na.rm = TRUE)+
  scale_fill_gradient2_tableau()+
  theme_economist()+
  theme(text = element_text(family = "Rockwell"),
    plot.title = element_text(size = 15))+
  labs(title = "Movie Length V Rating",
       subtitle = "Horror Movies",
       x = "Movie Length (min)",
       y = "Review Rating",
       aesthetic = "Frequency",
       caption = "Source: imdb.com | Graph: @Mathl3t3")

ggsave("HorrorHeatmap.jpg")
