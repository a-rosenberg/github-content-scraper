## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/README-",
  message = FALSE
)
suppressPackageStartupMessages(library(ggplot2))
theme_set(theme_light())


## ---- eval=FALSE---------------------------------------------------------
## install.packages("tidytext")


## ---- eval=FALSE---------------------------------------------------------
## library(remotes)
## install_github("juliasilge/tidytext")


## ------------------------------------------------------------------------
library(janeaustenr)
library(dplyr)

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(line = row_number()) %>%
  ungroup()

original_books


## ------------------------------------------------------------------------
library(tidytext)
tidy_books <- original_books %>%
  unnest_tokens(word, text)

tidy_books


## ------------------------------------------------------------------------
tidy_books <- tidy_books %>%
  anti_join(get_stopwords())


## ------------------------------------------------------------------------
tidy_books %>%
  count(word, sort = TRUE) 


## ---- fig.width=8, fig.height=10-----------------------------------------
library(tidyr)
get_sentiments("bing")

janeaustensentiment <- tidy_books %>%
  inner_join(get_sentiments("bing"), by = "word") %>% 
  count(book, index = line %/% 80, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

janeaustensentiment


## ---- fig.width=7, fig.height=7, warning=FALSE---------------------------
library(ggplot2)

ggplot(janeaustensentiment, aes(index, sentiment, fill = book)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")


## ------------------------------------------------------------------------
library(tm)
data("AssociatedPress", package = "topicmodels")
AssociatedPress


## ------------------------------------------------------------------------
tidy(AssociatedPress)


## ------------------------------------------------------------------------
ap_sentiments <- tidy(AssociatedPress) %>%
  inner_join(get_sentiments("bing"), by = c(term = "word")) %>%
  count(document, sentiment, wt = count) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  arrange(sentiment)


## ---- fig.height=8, fig.width=8------------------------------------------
comparison <- tidy(AssociatedPress) %>%
  count(word = term) %>%
  rename(AP = n) %>%
  inner_join(count(tidy_books, word)) %>%
  rename(Austen = n) %>%
  mutate(AP = AP / sum(AP),
         Austen = Austen / sum(Austen))

comparison

library(scales)
ggplot(comparison, aes(AP, Austen)) +
  geom_point(alpha = 0.5) +
  geom_text(aes(label = word), check_overlap = TRUE,
            vjust = 1, hjust = 1) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")

