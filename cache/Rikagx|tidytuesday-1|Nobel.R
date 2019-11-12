## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)


## ----setup2, warning = FALSE, message=FALSE------------------------------
library(tidyverse)
library(janitor)
library(scales)
library(readr)
library(lubridate)
library(ggbeeswarm)
library(ggridges)
library(maps)




## ----import, warning = FALSE, message=FALSE------------------------------
nobel <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")


## ------------------------------------------------------------------------
glimpse(nobel)


## ----barchart------------------------------------------------------------
nobel %>% 
  mutate(age =  prize_year - year(birth_date)) %>% 
  filter(!is.na(gender)) %>% 
  ggplot(aes(x = category,
             fill = gender)) +
  geom_bar(width = 0.5)+ 
  theme_minimal()+
  scale_fill_manual(values=c("#BB1288", "#5867A6", "gray"))+
  labs(title = "Noble Prize Winners By Gender",
       colour = "Gender",
       x = "Category",
       y = "# of Winners")
      

## ------------------------------------------------------------------------
nobel %>% 
  filter(gender == "Female") %>%
  select(full_name, prize_year, category) %>% 
  group_by(category) %>% 
  count() %>% 
  arrange(desc(n))

## ------------------------------------------------------------------------
nobel %>% 
  filter(gender == "Female") %>% 
  select(full_name, prize_year, category) %>% head()


## ------------------------------------------------------------------------
nobel %>% filter(is.na(gender)) %>% select(full_name, category)


## ----beeswarm chart, warning = FALSE-------------------------------------
nobel %>% 
  mutate(age =  prize_year - year(birth_date)) %>% 
  ggplot(aes(x = category,
             y = age,
             colour = gender)) +
  ggbeeswarm::geom_beeswarm() +
  coord_flip() +
  scale_color_manual(values = c("#BB1288", "#5867A6")) +
  theme_minimal() +
  labs(title = "Noble Prize Winners By Gender and Age",
       colour = "Gender",
       x = "Category",
       y = "Age")


## ----ggridges, message = FALSE, warning=FALSE----------------------------
nobel %>% 
  mutate(age =  prize_year - year(birth_date)) %>% 
  ggplot(aes(x = age,
             y = category,
            fill = gender)) +
  geom_density_ridges(scale = 0.9) + 
  scale_x_continuous(limits = c(16, 100))+
   scale_fill_manual(values = c("#BB1288", "#5867A6")) +
  theme_minimal() +
  labs(title = "Noble Prize Winners By Age",
       colour = "Gender",
       x = "Age",
       y = "Category")


## ------------------------------------------------------------------------
map.world <- map_data('world') 
map_countries <- distinct(map.world, region)


## ------------------------------------------------------------------------
nobel_countries <- nobel %>% group_by(birth_country) %>% count() %>% filter(n >5, !is.na(birth_country)) %>% arrange(desc(n))

anti_join(nobel_countries, map.world, by = c('birth_country' = 'region')) %>% select(birth_country) %>% group_by(birth_country) %>% count(sort = T)


## ------------------------------------------------------------------------
nobel <- nobel %>% mutate(birth_country = recode(birth_country,
                                                 "United States of America" = "USA",
                                                 "United Kingdom" = "UK",
                                                 "Scotland" = "UK",
                                                 "Northern Ireland" = "Ireland",
                                                 "Germany (Poland)" = "Poland",
                                                 "Prussia (Germany)" = "Germany",
                                                 "Prussia (Poland)" = "Poland"
                                                  )
                            )


## ------------------------------------------------------------------------


country_list <- nobel_countries$birth_country

nobel_small <- nobel %>% filter(birth_country %in% country_list)


## ------------------------------------------------------------------------
nobel_small <- left_join(nobel_small, nobel_countries, by ="birth_country") %>% rename("num_winners" = "n") #this add the count of winners

nobel_map <- left_join( map.world, nobel_small, by = c("region"= "birth_country"))


## ------------------------------------------------------------------------
ggplot(nobel_map, aes( x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = num_winners))+
  scale_fill_gradientn(colours = c('#461863','#404E88','#2A8A8C','#7FD157','#F9E53F')
                       ,values = scales::rescale(c(5, 50, 100, 200, 300))
                       ,labels = comma
                       ,breaks = c(5, 50, 100, 200, 300)
  ) +
  labs(fill = '# of Nobel Winners'
              ,title = 'U.S. and Europe Win Most Nobel Prizes'
       ,x = NULL
       ,y = NULL) +
  theme(text = element_text(family = 'Gill Sans', color = '#EEEEEE')
        ,plot.title = element_text(size = 20)
        ,plot.subtitle = element_text(size = 14)
        ,axis.ticks = element_blank()
        ,axis.text = element_blank()
        ,panel.grid = element_blank()
        ,panel.background = element_rect(fill = '#333333')
        ,plot.background = element_rect(fill = '#333333')
        ,legend.position = c(.18,.36)
        ,legend.background = element_blank()
        ,legend.key = element_blank()
  ) +
  annotate(geom = 'text'
           ,label = 'Source: Kaggle | Plot by @RikaGorn '
           ,x = 18, y = -55
           ,size = 3
           ,family = 'Gill Sans'
           ,color = '#CCCCCC'
           ,hjust = 'left'
  )

