## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
options(width = 120)


## ---- results='hide', message=FALSE, warning=FALSE-----------------------
library(tidyverse)
library(magrittr)
library(skimr)
library(wordcloud)
library(RColorBrewer)
library(caret)


## ---- eval=FALSE---------------------------------------------------------
## download.file("https://github.com/rfordatascience/tidytuesday/raw/master/data/2019/2019-01-01/tidytuesday_tweets.rds", "data/tidytuesday_tweets.rds")


## ------------------------------------------------------------------------
df <- read_rds("data/tidytuesday_tweets.rds")


## ---- message=FALSE, warning=FALSE---------------------------------------
skim(df)


## ------------------------------------------------------------------------
handlers <- df %>% 
    group_by(screen_name) %>% 
    summarise(n = n()) 
handlers


## ---- dpi=200------------------------------------------------------------
set.seed(420)
wordcloud(words = handlers$screen_name, freq = handlers$n, 
          scale = c(2, 0.5))


## ---- dpi=200------------------------------------------------------------
top_handlers <- handlers %>% 
    filter(!screen_name %in% c("thomas_mock", "R4DScommunity"),
           n >= 5)

set.seed(420)
wordcloud(words = top_handlers$screen_name, freq = top_handlers$n, 
          colors = brewer.pal(12, "Paired"), random.order = F, 
          scale = c(2, 0.4))


## ---- dpi=200------------------------------------------------------------
bott_handlers <- handlers %>% 
    filter(n < 5)

set.seed(420)
wordcloud(words = bott_handlers$screen_name, freq = bott_handlers$n, 
          colors = brewer.pal(12, "Paired"), random.order = F, 
          scale = c(1, 0.1))


## ---- dpi=200------------------------------------------------------------
preprocess_params <- preProcess(bott_handlers, method = c("range"))
scaled_handlers <- predict(preprocess_params, bott_handlers)

set.seed(420)
wordcloud(words = scaled_handlers$screen_name, freq = scaled_handlers$n, 
          colors = brewer.pal(12, "Paired"), random.order = F, 
          scale = c(1, 0.1))


## ------------------------------------------------------------------------
sessionInfo()

