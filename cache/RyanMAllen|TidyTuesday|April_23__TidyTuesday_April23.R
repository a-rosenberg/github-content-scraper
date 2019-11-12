## ----global_options, include=FALSE---------------------------------------
knitr::opts_chunk$set(fig.width=12, fig.height=8, warning=FALSE, message=FALSE)


## ----libraries, include=FALSE--------------------------------------------
library(tidyverse)
library(data.table)
library(corrplot)


## ----data_read-----------------------------------------------------------
set.seed(42)

# Reading in the data
 tidy_anime <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/tidy_anime.csv")
tidy_anime

# Selecting the columns I will use
anime <- select(tidy_anime, name, type, genre, episodes, duration, score, rank, popularity) %>% 
    filter(complete.cases(tidy_anime))
# Shortcuts for me to remember
# ctrl shift m %>% %>% %>% 
# <- alt - gives you <- <- <- 

# Finding the top 10 genre's by inclusion
anime_genre <- anime %>% 
    
    group_by(genre) %>% 
    
    tally() %>% 
    
    arrange(desc(n))

anime_genre <- slice(anime_genre, 1:10)
anime_genre

# Filtering based on the top 10 genres.
anime_genre <- filter(anime, genre == c("Action", "Comedy", "School", "Romance", "Fantasy", 
                                        "Supernatural", "Drama", "Sci-Fi", "Adventure", "Shounen"))
# Top 10 characters accoring to average score
top_10 <- anime_genre %>% 
    
    group_by(name) %>% 
    
    summarise(Score = mean(score)) %>% 
    
    arrange(desc(Score))
top_10

# Selecting all the numeric columns, then realizing that duration was a character
numeric <- anime %>% 
    select(episodes,duration, score, rank, popularity)
numeric$duration <- as.numeric(gsub(" min per ep", "", numeric$duration))

# A tibble with count, average score, average popularity, all grouped by genre
genre_average <- anime %>%
    group_by(genre) %>%
    summarise(Count = n(), Average_Score = mean(score), Average_Popularity = mean(popularity))
genre_average

# Finding the r.squared value of the regression line
r_squared <- summary(lm(genre_average$Average_Popularity~genre_average$Average_Score))


## ----plots---------------------------------------------------------------
# Summary by boxplot of the top 10 genres' scores
g <- ggplot(anime_genre, aes(x=genre, y= score)) +
    geom_boxplot()+ theme(panel.grid.major = element_blank(), axis.ticks = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank()) + 
    ggtitle("Summaries of the Top 10 Genres by count")

g

# Correlation of each variable
corrplot(cor(numeric))

chart <- ggplot(data = genre_average, aes(x = Average_Popularity, y = Average_Score, col = genre))
chart + 
    geom_point() +
    geom_text(aes(label = genre), vjust=-0.5) + 
    geom_smooth(method = 'lm', col = "black") +
    theme(panel.grid.major = element_blank(), axis.ticks = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), legend.position = "none") +
    annotate("text", x= 1700, y= 7.5, label ="r.squared = .1709") + 
    ggtitle("Average Popularity vs Average Score by Genre", subtitle = "There is a negative trend line, as popularity increases
average score decreases. It would appear that genres like Spake, Music, Josei, Sports perhaps
are the most popular and highly ranked. Samurai scores well, but is not very popular.")

