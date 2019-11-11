library(tidyverse)
library(scales)
library(broom)
library(ggthemes)
library(plotly)
library(here)
library(rtweet)

install.packages("yaml")
install.packages("rtweet")

install.packages("htmlwidgets")

trace(utils:::unpackPkgZip, edit=TRUE)

update.packages()

.libPaths()

#githubURL <- ("https://github.com/rfordatascience/tidytuesday/blob/master/data/2019/2019-01-01/tidytuesday_tweets.rds?raw=true")
#download.file(githubURL,"tidytuesday_tweets.rds")
tidytuesday <- read_rds("C:/Users/AKASHR/Documents/tidytuesday_tweets.rds")


#githubURL <- ("https://github.com/rfordatascience/tidytuesday/blob/master/data/2019/2019-01-01/rstats_tweets.rds?raw=true")
#download.file(githubURL,"rstats_tweets.rds")
rstats <- readRDS("C:/Users/AKASHR/Documents/rstats_tweets.rds")
rstats

## 1. Whose #rstats tweets were retweeted the most in 2018?

install.packages("ghibli")
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

View(rstats)
View(toppeople)

library(ggplot2)
theme_set(theme_classic())

# Plot

ggplot(toppeople, aes(x=screen_name, y=n, label=n))+ 
  geom_point(stat='identity', fill="black", size=12)  +
  geom_segment(aes(y = 0, 
                   x = `screen_name`, 
                   yend = n, 
                   xend = `screen_name`), 
               color = "black") +
  geom_text(color="white", size=4) +
  labs(title="Lollipop Chart", 
       subtitle="Whose #rstats tweets were retweeted the most in 2018?")
       + 
  coord_flip()


## 2. Whose #rstats tweets were liked the most in 2018?


toplikes <- rstats %>% 
  select(screen_name,created_at,followers_count,is_retweet,favorite_count, retweet_count,text) %>%
  filter(created_at >= as.Date("2018-01-01"),
         is_retweet == FALSE)  %>%
  group_by(screen_name) %>%
  summarise(total_favs = sum(favorite_count),
            n=n()) %>%
  arrange(desc(total_favs)) %>%
  head(5)


View(toplikes)

theme_set(theme_classic())

# Plot
ggplot(toplikes, aes(x=screen_name, y=total_favs)) + 
  geom_point(col="tomato2", size=3) + # Draw points
  geom_segment(aes(x=screen_name, 
                   xend=screen_name, 
                   y=min(total_favs), 
                   yend=max(total_favs)), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  labs(title="Dot Plot", 
       subtitle="Whose #rstats tweets were liked the most in 2018", 
       caption="source: mpg") +  
  coord_flip()


## 3. Who had the most liked and retweeted #tidytuesday tweets of 2018?

View(tidytuesday)


toplikedtt <- tidytuesday %>% 
  select(screen_name,created_at,followers_count,is_retweet,favorite_count, retweet_count,text,favorite_count) %>%
  filter(created_at >= as.Date("2018-01-01"),
         is_retweet == FALSE)  %>%
  group_by(screen_name) %>%
  summarise(total_favr = sum(retweet_count,favorite_count)) %>%
  arrange(desc(total_favr)) %>%
  head(10)

View(toplikedtt)

tt5 <- toplikedtt %>% head(5)
View(tt5)

library(ggplot2)
library(gganimate)
library(gapminder)
theme_set(theme_dark())  # pre-set the bw theme.

g <- ggplot(tt5, aes(screen_name, total_favr)) + 
  labs(subtitle="Who had the most liked and retweeted #tidytuesday tweets of 2018?",
       title="Bubble chart")

g + geom_jitter(aes(col=screen_name, size=total_favr)) + 
  geom_smooth(aes(col=screen_name), method="lm", se=F)

## 4. Total #rstats and #tidytuesday tweets over time in 2018 ?


rstats1 <- rstats %>% mutate(tweet_month = as.POSIXct(rstats$created_at))

rstats1$tweet_month <- format(rstats1$tweet_month,"%B")

View(rstats1)
View(rtotal)

rtotal <- rstats1 %>% 
  select(created_at, tweet_month) %>%
  filter(created_at >= as.Date("2018-01-01")) %>%
  group_by(tweet_month) %>%
  summarise(month_tweets=n()) %>%
  arrange(desc(month_tweets))# %>%

theme_set(theme_light())

ggplot(rtotal, aes(tweet_month, month_tweets)) +
  geom_linerange(
    aes(x = tweet_month, ymin = 0, ymax = month_tweets), 
    color = "lightgray", size = 1.5
  )+
  geom_point(aes(color = tweet_month), size = 2)
  #ggpubr::color_palette("jco")


## TIdy tuesday 

View(tidytuesday)

ttotal <- tidytuesday %>% mutate(tweet_month = as.POSIXct(tidytuesday$created_at))

ttotal$tweet_month <- format(ttotal$tweet_month,"%B")

View(ttotal)

ttotal <- ttotal %>% 
  select(created_at, tweet_month) %>%
  filter(created_at >= as.Date("2018-01-01")) %>%
  group_by(tweet_month) %>%
  summarise(month_tweets=n()) %>%
  arrange(desc(month_tweets))# %>%

View(ttotal)

theme_set(theme_dark())

# plot 
ggplot(ttotal , aes(x = tweet_month, y = month_tweets)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = month_tweets), vjust = -0.3) + 
  labs(title="which month had the most #tidytuesday tweets of 2018?")
