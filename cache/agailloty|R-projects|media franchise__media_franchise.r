# Loading the packages
library(tidyverse)
library(hrbrthemes) # for nice ggplot2 themes
library(treemapify) # beautiful ggplot2 base treemaps


# Now let's read the dataset
media_franchises <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-02/media_franchises.csv")

# Set the default theme for the plots
theme_set(theme_ft_rc())


# First plot : Franchise by revenue category

options(repr.plot.height = 6.5, repr.plot.width = 10)
ggplot(media_franchises, 
       aes(area = revenue, fill = revenue_category, 
           label = franchise, subgroup = original_media)) +
  geom_treemap() +
  geom_treemap_text(grow = TRUE, reflow = TRUE, colour = "black") +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "bottom") +
  labs(
    title = "Franchise by revenue category",
    caption = "The area of each tile represents the 
revenue of the franchise as a proportion of all revenue from 1924 to 2013.",
    fill = "Revenue category"
  ) + theme(plot.title = element_text(size = 25))
  
  # Second plot : owners revenue by year
  
  options(repr.plot.height = 17, repr.plot.res = 350, repr.plot.width = 15)
ggplot(franchise, aes(x = year_created, y = owners)) +
geom_point(aes(size = revenue, col = revenue_category)) +
scale_fill_brewer(palette = "Set1") +
labs(title = "Owners revenue by year",
     subtitle = "From 1924 to 2014",
    y = "Owners", x = "Year") +
theme(plot.title = element_text(size = 30))

# Third plot : Original media by revenue category

options(repr.plot.height = 15, repr.plot.res = 350, repr.plot.width = 12)
media_franchises %>% 
group_by(original_media, revenue_category) %>%
summarize(revenue = sum(revenue)) %>%
ggplot(aes( x= reorder(original_media, revenue), y = revenue)) +
geom_col(fill = ft_cols$green, col = "black") + coord_flip() + 
facet_wrap(~revenue_category, scale = "free", nrow = 4) +
labs(title = "Original media worth by revenue category", 
     subtitle = "Total of revenue from 1920 to 2017",
    x = "Revenue category", y = "Revenue") +
theme(plot.title = element_text(size = 30))

  
