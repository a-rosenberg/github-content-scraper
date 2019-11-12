## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = FALSE,warning=FALSE,error=FALSE)

library(tidyverse)
library(scales)
library(broom)
library(ggthemes)
library(plotly)
library(here)
library(rtweet)

theme_set(theme_light())


## ---- include=FALSE------------------------------------------------------

githubURL <- ("https://github.com/rfordatascience/tidytuesday/blob/master/data/2019/2019-01-01/tidytuesday_tweets.rds?raw=true")
download.file(githubURL,"tidytuesday_tweets.rds")
tidytuesday <- read_rds("tidytuesday_tweets.rds")


githubURL <- ("https://github.com/rfordatascience/tidytuesday/blob/master/data/2019/2019-01-01/rstats_tweets.rds?raw=true")
download.file(githubURL,"rstats_tweets.rds")
rstats <- read_rds("rstats_tweets.rds")



## ------------------------------------------------------------------------

library(ghibli)

toppeople <- rstats %>% 
  select(screen_name,created_at,followers_count,is_retweet,favorite_count, retweet_count,text) %>%
  filter(created_at >= as.Date("2018-01-01"),
         is_retweet == FALSE)  %>%
  group_by(screen_name) %>% 
  summarise(total_retweets = sum(retweet_count),
            n=n()) %>%
  arrange(desc(total_retweets)) %>%
  head(5)

myColors <- as.list(ghibli_palette("PonyoMedium",n=5))
names(myColors) <- toppeople$screen_name

toppeople %>% 
  mutate(screen_name = fct_reorder(screen_name,total_retweets)) %>%
  ggplot(aes(y = total_retweets, x = screen_name, fill=screen_name)) +
  geom_col() +
  coord_flip() +
  theme_light() +
  labs(title = "Who had the most retweeted #rstats tweets in 2018?",
       y = "Number of retweets of #rstats tweets in 2018",
       x = "",
       caption = "(Data from the rtweet package)") +
  theme(legend.position = "none",
        text = element_text(family = "Andale Mono"),
        plot.margin = unit(c(.5,1,.5,0),"cm")) +
  scale_fill_manual(name = "screen_name",values = myColors) +
  geom_text(aes(label = paste(n,c(rep("tweets",5)))), 
            position = position_stack(vjust = 0.5),
            family = "Andale Mono",
            size = 2,
            col="white")

#ggsave(filename = "rstatsretweets.png", path = here("Week20190101_files"),width = 7, height = 4)



## ------------------------------------------------------------------------

library(ghibli)

toppeople <- rstats %>% 
  select(screen_name,created_at,followers_count,is_retweet,favorite_count, retweet_count,text) %>%
  filter(created_at >= as.Date("2018-01-01"),
         is_retweet == FALSE)  %>%
  group_by(screen_name) %>%
  summarise(total_favs = sum(favorite_count),
            n=n()) %>%
  arrange(desc(total_favs)) %>%
  head(5)

toppeople %>% 
  mutate(screen_name = fct_reorder(screen_name,total_favs)) %>%
  ggplot(aes(y = total_favs, x = screen_name, fill=screen_name)) +
  geom_col() +
  coord_flip() +
  theme_light() +
  labs(title = "Who had the most liked #rstats tweets in 2018?",
       y = "Number of favorites of #rstats tweets in 2018",
       x = "",
       caption = "(Data from the rtweet package)") +
  theme(legend.position = "none",
        text = element_text(family = "Andale Mono"),
        plot.margin = unit(c(.5,1,.5,0),"cm")) +
  scale_fill_manual(name = "screen_name",values = myColors) +
  geom_text(aes(label = paste(n,c(rep("tweets",5)))), 
            position = position_stack(vjust = 0.5),
            family = "Andale Mono",
            size = 2,
            col="white")

#ggsave(filename = "rstatslikes.png", path = here("Week20190101_files"),width = 7, height = 4)




## ------------------------------------------------------------------------
topliked <- rstats %>% 
  select(screen_name,created_at,followers_count,is_retweet,favorite_count, retweet_count,text,favorite_count) %>%
  filter(created_at >= as.Date("2018-01-01"),
         is_retweet == FALSE)  %>%
  group_by(screen_name) %>%
  summarise(total_fav_ret = sum(retweet_count,favorite_count)) %>%
  arrange(desc(total_fav_ret)) %>%
  head(10)

topliked %>% 
  mutate(screen_name = fct_reorder(screen_name,total_fav_ret)) %>%
  ggplot(aes(y = total_fav_ret, x = screen_name, fill=screen_name)) +
  geom_col() +
  coord_flip() +
  theme_light() +
  labs(title = "Users with the most retweeted and favorited #rstats tweets in 2018",
       y = "Total #rstats retweets and favorites in 2018",
       x = "") +
  theme(legend.position = "none")

#ggsave(filename="rstats.png",path=here("Week20190101_files"),width = 9, height = 6)


## ------------------------------------------------------------------------
topliked <- tidytuesday %>% 
  select(screen_name,created_at,followers_count,is_retweet,favorite_count, retweet_count,text,favorite_count) %>%
  filter(created_at >= as.Date("2018-01-01"),
         is_retweet == FALSE)  %>%
  group_by(screen_name) %>%
  summarise(total_fav_ret = sum(retweet_count,favorite_count)) %>%
  arrange(desc(total_fav_ret)) %>%
  head(10)

topliked %>% 
  mutate(screen_name = fct_reorder(screen_name,total_fav_ret)) %>%
  ggplot(aes(y = total_fav_ret, x = screen_name, fill=screen_name)) +
  geom_col() +
  coord_flip() +
  theme_light() +
  labs(title = "Users with the most retweeted and favorited #TidyTuesday tweets in 2018",
       y = "Total #TidyTuesday retweets and favorites in 2018",
       x = "") +
  theme(legend.position = "none")

#ggsave(filename="tidytuesday.png",path=here("Week20190101_files"),width = 9, height = 6)


## ------------------------------------------------------------------------
# install.packages("devtools")
#devtools::install_github("hadley/emo")
#devtools::install_github("GuangchuangYu/emojifont")

toptweet <- tidytuesday %>% 
  select(screen_name,created_at,is_retweet,favorite_count, retweet_count,text,status_id) %>%
  filter(created_at >= as.Date("2018-01-01"),
         is_retweet == FALSE)  %>%
  mutate(fav_ret = retweet_count+favorite_count) %>%
  arrange(desc(fav_ret)) %>%
  head(10)

toptweet %>%
  mutate(tex = paste0(str_sub(text,1,60)," ... (",format(created_at,format = "%b %d %Y"),")"),
         screen_name = paste0("@",screen_name)) %>% 
  mutate(tex = fct_reorder(tex,fav_ret)) %>%
  ggplot(aes(y = fav_ret, x = tex, fill=screen_name)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(title = "The most retweeted and favorited #TidyTuesday tweets of 2018",
       y = "Number of retweets and favorites combined",
       x = "",
       fill="",
       caption = "Data from the rtweet package.") +
  theme(legend.position = "bottom",
        axis.text=element_text(size=7),
        legend.text=element_text(size=7),
        title=element_text(size=8),
        text=element_text(family = "Tahoma")) 

#ggsave(filename="tidytuesdaytweets.png",path=here("Week20190101_files"),width = 8, height = 4)



## ------------------------------------------------------------------------

rstats %>% 
  ts_plot(by = "weeks")

tidytuesday %>%
  ts_plot(by = "days")



## ------------------------------------------------------------------------

toppeople <- rstats %>% 
  select(screen_name,created_at,followers_count,is_retweet,favorite_count, retweet_count,text) %>%
  filter(created_at >= as.Date("2018-01-01"),
         is_retweet == FALSE)  %>%
  group_by(screen_name) %>% 
  summarise(mean_retweets = mean(retweet_count),
            n = n()) %>%
  filter(n>19) %>%
  arrange(desc(mean_retweets)) %>%
  head(7)

toppeople %>% 
  mutate(screen_name = fct_reorder(screen_name,mean_retweets)) %>%
  ggplot(aes(y = mean_retweets, x = screen_name, fill=n)) +
  geom_col() +
  coord_flip() +
  theme_light() +
  labs(title = "Whose #rstats tweets were retweeted most on average in 2018?",
       subtitle = "(of those with at least 10 tweets)",
       y = "Mean number of retweets of #rstats tweets in 2018",
       x = "",
       caption = "(Data from the rtweet package)") +
  theme(legend.position = "none",
        text = element_text(family = "Andale Mono"),
        plot.margin = unit(c(.5,1,.5,0),"cm"),
        axis.text = element_text(size = 10)) +
  geom_text(aes(label = paste(n,c(rep("tweets",7)))), 
            position = position_stack(vjust = 0.5),
            family = "Andale Mono",
            size = 3,
            col="white") +
  scale_fill_gradient(low = myColors[1], high = myColors[5])

#ggsave(filename = "rstatsavgretweets.png", path = here("Week20190101_files"),width = 8.5, height = 4)



## ------------------------------------------------------------------------

library(ghibli)

toppeople <- rstats %>% 
  select(screen_name,created_at,followers_count,is_retweet,favorite_count, retweet_count,text) %>%
  filter(created_at >= as.Date("2018-01-01"),
         is_retweet == FALSE)  %>%
  group_by(screen_name) %>%
  summarise(mean_favs = mean(favorite_count),
            n=n()) %>%
  filter(n>19) %>%
  arrange(desc(mean_favs)) %>%
  head(7)

toppeople %>% 
  mutate(screen_name = fct_reorder(screen_name,mean_favs)) %>%
  ggplot(aes(y = mean_favs, x = screen_name, fill=n)) +
  geom_col() +
  coord_flip() +
  theme_light() +
  labs(title = "Whose #rstats tweets were liked most on average in 2018?",
       subtitle = "(of those with at least 10 tweets)",
       y = "Mean number of favorites for #rstats tweets in 2018",
       x = "",
       caption = "(Data from the rtweet package)") +
  theme(legend.position = "none",
        text = element_text(family = "Andale Mono"),
        plot.margin = unit(c(.5,1,.5,0),"cm"),
        axis.text = element_text(size = 10)) +
  scale_fill_gradient(low = myColors[1], high = myColors[5]) +
  geom_text(aes(label = paste(n,c(rep("tweets",7)))), 
            position = position_stack(vjust = 0.5),
            family = "Andale Mono",
            size = 3,
            col="white")

#ggsave(filename = "rstatsavglikes.png", path = here("Week20190101_files"),width = 8.5, height = 4)




## ----echo=FALSE----------------------------------------------------------
knitr::knit_exit()


## ------------------------------------------------------------------------

toptweet <- tidytuesday %>% 
  select(screen_name,created_at,is_retweet,favorite_count, retweet_count,text,status_id) %>%
  filter(created_at >= as.Date("2018-01-01"),
         is_retweet == FALSE)  %>%
  arrange(desc(favorite_count)) %>%
  head(10)

toptweet %>%
  mutate(tex = paste0(str_sub(text,1,60)," ... (",format(created_at,format = "%b %d %Y"),")"),
         screen_name = paste0("@",screen_name)) %>% 
  mutate(tex = fct_reorder(tex,favorite_count)) %>%
  ggplot(aes(y = favorite_count, x = tex, fill=screen_name)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(title = "The most liked #TidyTuesday tweets of 2018",
       y = "Number of likes",
       x = "",
       fill="",
       caption = "Data from the rtweet package.") +
  theme(legend.position = "bottom",
        axis.text=element_text(size=7),
        legend.text=element_text(size=7),
        title=element_text(size=8),
        text=element_text(family = "Tahoma")) 

#ggsave(filename="tidytuesdaytweets.png",path=here("Week20190101_files"),width = 8, height = 4)


