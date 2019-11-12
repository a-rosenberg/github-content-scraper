## ---- message = FALSE----------------------------------------------------
# LIBRARIES -------------------------------------------------------------------#
library(tidyverse)
library(tidytext)
library(lubridate)


## ------------------------------------------------------------------------
# IMPORT DATA -----------------------------------------------------------------#
raw_metadata <- read_csv(file ="medium_datasci.csv", quote='"')
head(raw_metadata)


## ------------------------------------------------------------------------
# CLEAN -----------------------------------------------------------------------#
metadata <- raw_metadata %>% 
  distinct(title, author, .keep_all = TRUE) %>%
  drop_na(title) 
head(metadata)

dim(raw_metadata)
dim(metadata)


## ------------------------------------------------------------------------
metadata <- metadata %>%
  mutate(date = ymd(paste(year, month, day, sep= '-')), 
         weekday = wday(as.Date(date,'%Y-%m-%d'), label = TRUE, abbr = FALSE))


## ------------------------------------------------------------------------
# ANALYSIS --------------------------------------------------------------------#
metadata %>%
  ggplot(mapping = aes(x = date)) + 
  geom_line(stat = "bin", bins = 25) + 
  theme_bw() + 
  ggtitle(label = "Medium's data science article publication output doubled in 1 year")


## ------------------------------------------------------------------------
metadata %>%
  add_count(as.factor(weekday)) %>% 
  distinct(weekday, n) %>%
  ggplot(mapping = aes(x = weekday, y = n, fill = as.factor(weekday))) + 
  theme_bw() + 
  geom_bar(stat = "identity") +
  ggtitle(label = "Medium's data science articles snooze on weekends")


## ------------------------------------------------------------------------
no_clap_percentage <- round(100 * sum(metadata$claps == 0)/nrow(metadata), 0)

metadata %>% 
  filter(claps < 250) %>% 
    ggplot(mapping = aes(x = claps)) + 
    theme_bw() + 
    geom_histogram(bins = 50) +
    ggtitle(label = paste(no_clap_percentage, "% of data science articles go without applause", sep = ""))


## ------------------------------------------------------------------------
metadata %>%
    ggplot(mapping = aes(x = reading_time, y = claps)) + 
    geom_point(na.rm = TRUE) + 
    geom_vline(xintercept = 7, 
               linetype = "dashed", 
               color = "red",
               size = 1) + 
    theme_bw() + 
    geom_jitter() + 
    ggtitle(label = "Sweet spot for claps is a 7 minute read")



## ------------------------------------------------------------------------
metadata %>%
  add_count(as.factor(author)) %>%
    ggplot(mapping = aes(y = claps, x = n)) + 
    geom_point() + 
    theme_bw() +
    scale_colour_gradient(low = "white", high = "red") + 
    ggtitle(label = "Writing more articles on Medium does not necessarily increase engagement")


## ------------------------------------------------------------------------
metadata %>%
  filter(claps > 10000) %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  filter(n > 3) %>%
  ggplot(mapping = aes(x = word, y = n/sum(n))) +
  theme_bw() + 
  geom_bar(stat = "identity") + 
  ylab("Frequency") + 
  ggtitle(label = "Most common words in popular articles")


metadata %>%
  filter(claps < 10) %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  filter(n > 1500) %>%
  ggplot(mapping = aes(x = word, y = n/sum(n))) +
  theme_bw() + 
  geom_bar(stat = "identity") + 
  ylab("Frequency") + 
  ggtitle(label = paste("Most common words in unpopular articles"))



## ------------------------------------------------------------------------
metadata %>%
  filter(claps > 10000) %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  filter(n > 3) %>%
  ggplot(mapping = aes(x = fct_reorder(word, n, .desc = TRUE), y = n/sum(n))) +
  theme_bw() + 
  geom_bar(stat = "identity") + 
  ylab("Frequency") + 
  xlab("word") + 
  ggtitle(label = "Most common words in popular articles")


metadata %>%
  filter(claps < 10) %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  filter(n > 1500) %>%
  ggplot(mapping = aes(x = fct_reorder(word, n, .desc = TRUE), y = n/sum(n))) +
  theme_bw() + 
  geom_bar(stat = "identity") + 
  ylab("Frequency") + 
  xlab("word") + 
  ggtitle(label = "Most common words in unpopular articles")

