## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
options(stringsAsFactors = FALSE)


## ---- message=FALSE, warning=FALSE---------------------------------------
library(tidyverse)
library(ggplot2)
library(cowplot)
library(ggthemes)


## ---- message=FALSE, warning=FALSE---------------------------------------
bird_counts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-18/bird_counts.csv")


## ------------------------------------------------------------------------
dim(bird_counts)
bird_counts <- bird_counts %>% filter(how_many_counted > 0)
dim(bird_counts)


## ------------------------------------------------------------------------
bird_species <- data.frame(table(bird_counts$species))
bird_species <- bird_species[order(bird_species$Freq, decreasing = TRUE), ]
head(bird_species, n = 25)


## ---- fig.width=12-------------------------------------------------------
plot_species <- plot_grid(
  head(bird_species, n = 15) %>% 
    ggplot(aes(x= factor(Var1, levels = rev(Var1)), y = Freq)) + 
    geom_bar(stat = "identity") +
    theme_wsj() +
    coord_flip() +
    ggtitle("Top 15 species\nobserved across years") +
    theme(
      plot.title = element_text(size = 14, hjust = 0.5),
      plot.caption = element_text(size = 10)
    ) +
    labs(caption = " "),
  bird_species %>% 
    filter(Freq > 1) %>% 
    tail(n = 15) %>% 
    ggplot(aes(x= factor(Var1, levels = rev(Var1)), y = Freq)) + 
    geom_bar(stat = "identity") +
    theme_wsj() +
    coord_flip() +
    ggtitle("Bottom 15 species\nobserved across years") +
    theme(
      plot.title = element_text(size = 14, hjust = 0.5),
      plot.caption = element_text(size = 10)
    ) + 
    labs(caption = "Source: Bird Studies Canada"),
  ncol = 2
)

ggsave(plot_species, file = "plot_species_count.png", width = 12)

plot_species


## ---- fig.width=12-------------------------------------------------------
plot_many_year <- function(dt, name, min, max) {
  dt %>% 
    filter(species == name) %>% 
    ggplot(aes(x = year, y = how_many_counted)) +
    geom_line() +
    ggtitle(name) +
    theme_wsj() + 
    ylim(min, max) +
    theme(
      plot.title = element_text(size = 14, hjust = 0.5),
      axis.title = element_text(family = "mono", size = 10)
    ) +
    xlab("Year") + ylab("How many counted")
}

plot_many_hour <- function(dt, name, min, max) {
  dt %>% 
    filter(species == name) %>%
    ggplot(aes(x = total_hours, y = how_many_counted_by_hour)) + 
    geom_point() +
    ggtitle(name) +
    theme_wsj() +
    ylim(min, max) +
    theme(
      plot.title = element_text(size = 14, hjust = 0.5),
      axis.title = element_text(family = "mono", size = 10),
      plot.caption = element_text(size = 10)
    ) +
    xlab("Total hours") + ylab("How many counted by hour")
}

plot_bird <- plot_grid(
  plot_many_year(bird_counts, "American Tree Sparrow", 0, 2700),
  plot_many_year(bird_counts, "Blue Jay", 0, 2700),
  plot_many_year(bird_counts, "Downy Woodpecker", 0, 2700),
  plot_many_hour(bird_counts, "American Tree Sparrow", 0, 15) +
    labs(caption = " "),
  plot_many_hour(bird_counts, "Blue Jay", 0, 4) + 
    labs(caption = " "),
  plot_many_hour(bird_counts, "Downy Woodpecker", 0, 2) + 
    labs(caption = "Source: Bird Studies Canada"),
  ncol = 3
)

ggsave(plot_bird, file = "plot_bird_count.png", width = 12)

plot_bird

