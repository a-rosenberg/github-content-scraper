# Read the dataset
bird_counts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-18/bird_counts.csv")

# The packages 
library(tidyverse) # of course
library(treemap)
library(ggthemes)

# The treemap
options(repr.plot.width = 8, repr.plot.height = 7, repr.plot.res = 500)
bird_counts %>% group_by(species, year) %>%
summarize(total_count = sum(how_many_counted), total_hours = sum(total_hours)) %>%
na.omit %>% filter(total_count>0, year %in% c(1930,1950,1960,1970,1990,2000,2010,2017)) %>%
treemap(index = c("year", "species"), 
        vSize = "total_count",
        palette = "Set1",
        title="Most observed bird species by year",
        fontsize.title = 14)
        
 # Comparison 1950 - 2017
 bird_counts %>%
group_by(year, species) %>%
summarize(total_count = sum(how_many_counted)) %>%
filter(total_count >0) %>%
spread(year, total_count) %>%
select(species, `1950`, `2017`) %>% na.omit %>%
ggplot() + geom_segment( aes(x=reorder(species, `1950`), xend=species, y=`1950`, yend=`2017`), color="#6E6A6A") +
geom_point( aes(species, y=`1950`), color="#F74B4B", size=3 ) +
  geom_point( aes(x=species, y=`2017`),color="#36ACD7", size=3 ) +
  coord_flip() + theme_solarized_2() +
labs(title = "Bird species counts in 1950 and 2017", x ="", y = "", subtitle = "Red : 1950 \nBlue : 2017")
