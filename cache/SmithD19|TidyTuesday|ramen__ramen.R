## ----setup, include=FALSE, dpi = 300-------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(ggfortify)
library(ggthemes)


## ----read in data--------------------------------------------------------
ramen_ratings <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-04/ramen_ratings.csv")
summary(ramen_ratings)


## ------------------------------------------------------------------------
# Top ten ramen manufacturers by country
CountryRatings <- 
  
  # collecting top ten factors
  ramen_ratings %>%
  mutate(top = fct_infreq((fct_lump(country, n =11)))) %>% 
  filter(top != "Other") %>% 
  mutate(Rating = case_when(stars >= 0 & stars <= 1 ~ "Very Poor",
                            stars >= 1.25 & stars <= 2 ~ "Poor",
                            stars >= 2.25 & stars <= 3 ~ "Average",
                            stars >= 3.25 & stars <= 4 ~ "Good",
                            stars >= 4.25 & stars <= 5 ~ "Very Good"),
         Rating = as.factor(Rating)) %>% 
  drop_na()

# change levels of Rating factor
levels(CountryRatings$Rating) <- 
  c("Very Poor", "Poor", "Average", "Good", "Very Good")

plot <- 
  CountryRatings %>% 
  # plot
  ggplot(aes(x = top, fill = Rating)) +
  geom_bar() +
  
  # styling
  ggthemes::theme_economist() +
  # bbplot::bbc_style() +
  coord_flip() +
  labs(title = "Number of Ramen Rated by Country of Origin",
       subtitle = "The top 10 countries producing ramen and their ratings distributions",
       caption = "Source - www.theramenrater.com") +
  ylab("Count") +
  xlab("") +
  scale_fill_viridis_d()
  
# values for hline
avgfreq <- as.numeric(ramen_ratings %>% 
  group_by(country) %>%
  tally() %>% summarise(mean = mean(n)))

# extras
plot + 
  geom_hline(aes(yintercept = avgfreq), col = "firebrick1") +
  
   # annotate the hline
 annotate(geom = "text",
           x = 7.5, y = 250,
           label = "World Average",
           vjust = .5, hjust = 0,
           family = "Helvetica",
           fontface = "bold",
           size = 3,
           lineheight = 1) +
    geom_curve(aes(x = 7.1, y = 250, xend = 5, yend = avgfreq + 2), 
             colour = "black", 
             size = .5, 
             curvature = -0.1,
             arrow = arrow(length = unit(0.03, "npc")))



ggsave(filename = "CountryRatings.png", plot = last_plot(), width = 7, height = 5, dpi = 300)


## ------------------------------------------------------------------------
# Top performing ramen brands
brand_rating <- 
  ramen_ratings %>% 
  mutate(brand = fct_lump(brand, prop = .01)) %>% 
  group_by(brand) %>% 
  summarise(mean_stars = mean(stars, na.rm = TRUE))

brand_rating %>% 
  filter(brand != "Other") %>% 
  ggplot(aes(x =fct_reorder(brand, -mean_stars), y = mean_stars)) +
  geom_bar(stat = "identity") +
  coord_flip()


## ------------------------------------------------------------------------
# How does ramen style change the rating?
style_stars <- 
  ramen_ratings %>%
  group_by(style) %>% 
  drop_na() %>% 
  summarise(mean_stars = mean(stars, na.rm = T),
            count = n(), 
            sd = sd(stars, na.rm = T),
            se = sd(stars, na.rm = T)/sqrt(n())) %>% 
  filter(count > 300)

ramen_ratings %>% 
  group_by(style) %>% 
  mutate(count = n()) %>% 
  filter(count > 300) %>% 
  ggplot(aes(x = country, y = stars, fill = style)) +
  geom_violin() +
  geom_boxplot(width = .2) +
  facet_wrap(~ style)

lm1 <- lm(stars ~ style, data = ramen_ratings)
autoplot(lm1)
# definitely need ordinal regression models... bet thats hard...


