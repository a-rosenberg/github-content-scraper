## ----message = F, warning = F--------------------------------------------
library(tidyverse)
library(ggthemes)


## ------------------------------------------------------------------------
library(data.table)
files <- list.files(path = "../data/week20/", pattern = "*.csv", full.names = T)
DT <- do.call(rbind, lapply(files[-5], fread))


## ----message = F, warning = F--------------------------------------------
#df04 <- read_csv("../data/week20/IRAhandle_tweets_04.csv")
#df05 <- read_csv("../data/week20/IRAhandle_tweets_05.csv", 
#                 col_types = "dcccccciiicciciddcccc")
#dfna <- df05 %>% filter(is.na(external_author_id)) %>% knitr::kable()


## ------------------------------------------------------------------------
DT %>% 
  count(account_category) %>% 
  arrange(desc(n))


## ----fig.width=10, fig.height=6------------------------------------------
library(lubridate)
dday <- DT %>% 
  select(publish_date, account_category) %>% 
  mutate(publish_date = mdy_hm(publish_date),
         date = date(publish_date))
nday <- dday %>% count(account_category, date) 
tday <- dday %>% count(date) %>% mutate(account_category = "Total") %>% 
  select(account_category, date, n)

cday <- bind_rows(tday, nday)

p0 <- tday %>% 
  filter(date > ymd("2015-07-01"),
         date < ymd("2018-01-01")) %>% 
  ggplot(aes(x = date, y = n)) +
  geom_bar(stat = "identity", fill="orange") +
  theme_fivethirtyeight() +
  labs(title = "Russian troll tweets by day",
       subtitle = "Nearly 3 million tweets sent by trolls",
       caption = "Source: FiveThirtyEight") +
  NULL
p0
ggsave("total.png", p0, width = 6, height = 4, dpi = 300)


## ----fig.width=10, fig.height=8------------------------------------------
library(gghighlight)
library(forcats)
cat4 <- c("RightTroll", "LeftTroll", "NewsFeed", "HashtagGamer")
lab4 <- c("Right Troll", "Left Troll", "News Feed", "Hashtag Gamer")

df4 <- DT %>% 
  select(publish_date, account_category) %>% 
  mutate(publish_date = mdy_hm(publish_date),
         date = date(publish_date)) %>% 
  filter(account_category %in% cat4) %>% 
  mutate(account_category = factor(account_category,
                                   levels = cat4,
                                   labels = lab4)) %>% 
  filter(date > ymd("2015-07-01"),
         date < ymd("2018-01-01")) 
p1 <- df4 %>% 
  ggplot(aes(date, fill = account_category)) +
  geom_histogram(binwidth = 1) +
  theme_fivethirtyeight() +
  labs(title = "Not all trolls are the same",
       subtitle = "Tweets sent by trolls, as categorized by Clemson Researchs",
       caption = "Source: FiveThirtyEight") +
  NULL
p2 <- p1 + gghighlight() +
  facet_wrap(~ account_category)
p2
ggsave("cat4.png", p2, width = 6, height = 6, dpi = 300)

