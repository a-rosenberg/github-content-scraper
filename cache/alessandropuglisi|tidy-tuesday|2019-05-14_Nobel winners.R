### Load packages
library(tidyverse)
library(tidytext)
library(patchwork)

### Get the data
nobel_winners <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")

### Select distinct values of the column Motivation
motivations <- nobel_winners %>%
  filter(!is.na(motivation)) %>%
  select(motivation) %>%
  distinct()

### Tokenize motivations and remove stop words
motivations_tokenized <- motivations %>%
  unnest_tokens(word, motivation) %>%
  anti_join(stop_words)

### Plot 1 - Most used words
motivations_most_used_words <- motivations_tokenized %>%
  count(word) %>%
  arrange(desc(n)) %>%
  head(10) %>%
  
    ggplot(aes(x = reorder(word, n), y = n, fill = word, label = n)) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(label = n), hjust = 1.2, color = "white") +
    coord_flip() +
    labs(
      title = "10 most-used words",
      x = "",
      y = "Word frequency"
    ) +
    theme(
      panel.background = element_rect(color = "#e6cb00")
  )

### Plot 2 - Most used POS
motivations_pos <- motivations_tokenized %>%
  inner_join(parts_of_speech) %>%
  na.omit() %>%
  count(pos) %>%
  arrange(desc(n)) %>%
  head(3) %>%
  
    ggplot(aes(x = reorder(pos, n), y = n, fill = pos)) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(label = n), vjust = 1.2, color = "white") +
    labs(
      title = "3 most-used parts of speech",
      x = "Parts of speech",
      y = ""
    ) +
  theme(
    panel.background = element_rect(color = "#322bff")
  )

### Plot 3 - Sentiment
motivations_sentiment <- motivations_tokenized %>%
  inner_join(sentiments) %>%
  filter(lexicon == "nrc") %>%
  count(sentiment) %>%
  arrange(desc(n)) %>%
  head(5) %>%
  
    ggplot(aes(x = reorder(sentiment, n), y = n, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(label = n), vjust = 1.2, color = "white") +
    labs(
      title = "Top 5 sentiments",
      x = "Sentiment",
      y = ""
    ) +
    theme(
      panel.background = element_rect(color = "#ff2f2b")
    )

### Final plot
motivations_most_used_words + motivations_pos / motivations_sentiment +
  plot_annotation(
    title = "Nobel Prize's motivations: a text analysis",
    caption = "Data source: The Nobel Foundation via Kaggle \n @_apuglisi_ #TidyTuesday Week 20"
  )
