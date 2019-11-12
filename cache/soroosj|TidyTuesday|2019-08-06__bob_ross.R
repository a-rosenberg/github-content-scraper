## ----source, warning = TRUE, results = FALSE, message = FALSE------------

   library (tidyverse)
   library (janitor) 

   bob_ross_raw <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-06/bob-ross.csv") %>%
      clean_names () 


## ----transform, message = F----------------------------------------------

   library (tidytext)

   bob_ross <- bob_ross_raw %>%
      select (episode, title) %>%
      unnest_tokens(word, title) %>%
      count(word, sort = TRUE) %>%
      rename (freq = n) %>%
      anti_join(stop_words) %>%
      inner_join (get_sentiments("bing"))


## ------------------------------------------------------------------------

   library (wordcloud2)
   png('filename.png')
   #https://www.datacamp.com/community/tutorials/R-nlp-machine-learning
   letterCloud(bob_ross, word = "BOB ROSS", size = 1.3)
   dev.off ()

