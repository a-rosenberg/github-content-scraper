## ---- results='hide', message=FALSE, warning=FALSE-----------------------
library(tidyverse)
library(lubridate)


## ---- results='hide'-----------------------------------------------------
tweet_1 <- read_csv("russian-troll-tweets/IRAhandle_tweets_1.csv", col_types = "ccccccciiicciic")
tweet_2 <- read_csv("russian-troll-tweets/IRAhandle_tweets_2.csv", col_types = "ccccccciiicciic") 
tweet_3 <- read_csv("russian-troll-tweets/IRAhandle_tweets_3.csv", col_types = "ccccccciiicciic")
tweet_4 <- read_csv("russian-troll-tweets/IRAhandle_tweets_4.csv", col_types = "ccccccciiicciic")
tweet_5 <- read_csv("russian-troll-tweets/IRAhandle_tweets_5.csv", col_types = "ccccccciiicciic")
tweet_6 <- read_csv("russian-troll-tweets/IRAhandle_tweets_6.csv", col_types = "ccccccciiicciic") 
tweet_7 <- read_csv("russian-troll-tweets/IRAhandle_tweets_7.csv", col_types = "ccccccciiicciic")
tweet_8 <- read_csv("russian-troll-tweets/IRAhandle_tweets_8.csv", col_types = "ccccccciiicciic") 
tweet_9 <- read_csv("russian-troll-tweets/IRAhandle_tweets_9.csv", col_types = "ccccccciiicciic") 


## ------------------------------------------------------------------------
tweets <- bind_rows(list(tweet_1, tweet_2, tweet_3, tweet_4, tweet_5, tweet_6, tweet_7, tweet_8, tweet_9))


## ------------------------------------------------------------------------
top_20_handles <- tweets %>% 
    group_by(author, account_category) %>% 
    summarise(n = n()) %>% 
    arrange(desc(n)) 
top_20_handles <- top_20_handles[1:20, ]


## ---- dpi=200------------------------------------------------------------
top_20_handles %>% 
    ggplot(aes(x = reorder(author, n), y = n)) + 
    geom_col(aes(fill = account_category)) + 
    coord_flip() + 
    labs(x = "Authors", y = "Tweets", title = "Top 20 tweeters", fill = "") + 
    theme(legend.position = "bottom")


## ------------------------------------------------------------------------
categ_count <- tweets %>% 
    select(author, account_category) %>% 
    distinct() %>% 
    group_by(account_category) %>% 
    summarise(n = n())


## ---- dpi=200------------------------------------------------------------
categ_count %>% 
    ggplot(aes(x = reorder(account_category, n), y = n)) + 
    geom_col(aes(fill = account_category)) + 
    coord_flip() + 
    labs(x = "Category", y = "Accounts", title = "Number of handles per category") + 
    guides(fill = FALSE)


## ------------------------------------------------------------------------
top_tweets <- tweets %>% 
    filter(author %in% top_20_handles$author) %>% 
    separate(col = publish_date, into = c("pub_date", "pub_time"), sep = " ") %>% 
    mutate(pub_date = as_date(pub_date, tz = "UTC", format = "%d/%m/%Y"))


## ------------------------------------------------------------------------
top_tweets_grouped <- top_tweets %>% 
    group_by(pub_date, account_category) %>% 
    summarise(n = n()) %>% 
    filter(!is.na(pub_date))


## ---- dpi=200------------------------------------------------------------
top_tweets_grouped %>% 
    ggplot(aes(x = pub_date, y = n)) + 
    geom_line(aes(color = account_category)) + 
    labs(x = "Date", y = "Tweets", color = "", title = "Tweeting activity", subtitle = "Top 20 handles")


## ------------------------------------------------------------------------
tweets_dates <- tweets %>% 
    separate(col = publish_date, into = c("pub_date", "pub_time"), sep = " ") %>% 
    mutate(pub_date = as_date(pub_date, tz = "UTC", format = "%d/%m/%Y"))


## ------------------------------------------------------------------------
tweets_dates_grouped <- tweets_dates %>% 
    filter(pub_date >= "2015/01/01", pub_date <= "2018/01/01") %>% 
    group_by(pub_date, account_category) %>% 
    summarise(n = n()) %>% 
    filter(!is.na(pub_date))


## ---- dpi=200------------------------------------------------------------
tweets_dates_grouped %>% 
    ggplot(aes(x = pub_date, y = n)) + 
    geom_line(aes(color = account_category)) + 
    labs(x = "Date", y = "Tweets", title = "Tweeting activity", subtitle = "All handles") + 
    facet_wrap(~ account_category, nrow = 4) + 
    guides(color = FALSE)


## ------------------------------------------------------------------------
tweets_foll_grouped <- tweets_dates %>% 
    filter(pub_date >= "2015/01/01", pub_date <= "2018/01/01") %>% 
    group_by(pub_date, account_category) %>% 
    summarise(followers = sum(followers), following = sum(following)) %>% 
    filter(!is.na(pub_date))


## ---- dpi=200------------------------------------------------------------
tweets_foll_grouped %>% 
    ggplot(aes(x = pub_date)) + 
    geom_line(aes(y = log(followers), color = "red")) + 
    geom_line(aes(y = log(following), color = "blue")) + 
    labs(x = "Date", y = "Accounts (log)", title = "Followers and Followed accounts") + 
    facet_wrap(~ account_category, nrow = 4) + 
    scale_colour_manual(name = "", values = c("red" = "red", "blue" = "blue"), labels = c("Followed Accounts", "Followers")) + 
    theme(legend.position = "bottom")


## ------------------------------------------------------------------------
sessionInfo()

