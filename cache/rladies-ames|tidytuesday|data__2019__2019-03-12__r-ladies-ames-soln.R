## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align = "center", out.width = "75%", message = FALSE, warning = FALSE)


## ------------------------------------------------------------------------
library(tidyverse)
bgdat <- read_csv("board_games.csv")
glimpse(bgdat)


## ------------------------------------------------------------------------
ggplot(data = bgdat) + 
  geom_linerange(aes(x = year_published, y = (min_playtime + max_playtime)/2, ymin = min_playtime, ymax = max_playtime, group = game_id)) + 
  labs(x = "Year Published", y = "Playtime (minutes)")


## ------------------------------------------------------------------------
bgdat %>% filter(min_playtime > 10000) %>% glimpse()


## ------------------------------------------------------------------------
bgdat <- bgdat %>% filter(min_playtime < 10000)
ggplot(data = bgdat) + 
  geom_linerange(aes(x = year_published, y = (min_playtime + max_playtime)/2, ymin = min_playtime, ymax = max_playtime, group = game_id)) + 
  labs(x = "Year Published", y = "Playtime (minutes)")


## ------------------------------------------------------------------------
bgdat$category %>% head()


## ------------------------------------------------------------------------
bgdat$category %>% str_count(",") %>% max(na.rm=T)


## ------------------------------------------------------------------------
bgdat %>% mutate(ncat = str_count(category, ",") + 1) %>% 
  arrange(desc(ncat)) %>% select(name, category, ncat) %>% head


## ------------------------------------------------------------------------
bgdat2 <- bgdat %>% separate(category, into = c("firstcat", "secondcat"), sep = ",")
bgdat2 %>% count(firstcat)


## ----out.width= "100%"---------------------------------------------------
bgdat2 <- bgdat2 %>% mutate(firstcat = ifelse(str_detect(firstcat, "War"), "War", firstcat), secondcat = ifelse(str_detect(secondcat, "War"), "War", secondcat))
bgdat2 %>% count(firstcat)
bgdatcats <- bgdat2 %>% count(firstcat)
bgdatcats %>%  
  ggplot(aes(x = reorder(firstcat, n), weight = n)) + 
  geom_bar() + 
  coord_flip()
# only use categories with > 250 games (at least 2.5% of the games but have the category to be used)
bgdatcats2 <- bgdatcats %>% filter(n > 250)
bgdat3 <- bgdat2 %>% filter(firstcat %in% bgdatcats2$firstcat)
# facet by firstcat
ggplot(data = bgdat3) + 
  geom_linerange(aes(x = year_published, ymin = min_playtime, ymax = max_playtime, group = game_id)) + 
  labs(x = "Year Published", y = "Playtime (minutes)") + 
  facet_wrap(~firstcat, scales = "free")
# what about the mean of the min & max? 
ggplot(data = bgdat3) + 
  geom_point(aes(x=year_published , y = (min_playtime + max_playtime)/2, color = firstcat))
bgdat3 %>% mutate(mean_play = (min_playtime + max_playtime)/2) %>% 
  filter(mean_play < 1440) %>% # only look at games with less than a day of play time 
ggplot(aes(x = year_published, y = mean_play)) + 
  geom_point() + 
  geom_smooth() + 
  facet_wrap(~firstcat, scales = "free")

