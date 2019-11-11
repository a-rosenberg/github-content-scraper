require(tidyverse)
require(tidytext)
require(ggthemes)

# April 23, 2019
# tidy tuesday dataset

tidy_anime <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/tidy_anime.csv")

# sentiment analysis
# 'sentiments' dataset taken from tidytext
# afinn lexicon :continuous 
afinn <- sentiments %>% 
           dplyr::filter(lexicon == "AFINN") %>% 
           dplyr::select(word, score) 

# nrc lexicon: categorical
nrc <- sentiments %>% 
          dplyr::filter(lexicon == "nrc") %>%
          dplyr::select(word, sentiment)

tokens <- tidy_anime %>% 
            tidytext::unnest_tokens(word, synopsis) %>% 
            dplyr::count(genre, word, sort = T)

total_words <- tokens %>%
                dplyr::group_by(genre) %>%
                dplyr::summarize(total = sum(n))

synopsis_words <- tokens %>% 
  dplyr::left_join(., total_words) 

# ti-idf

ti_idf <- synopsis_words %>%
            tidytext::bind_tf_idf(word, genre, n) %>%
            dplyr::arrange(desc(tf_idf)) 

# sentiment analysis
anime_sentiment <- ti_idf %>%
  dplyr::left_join(., afinn, by = "word") %>% 
  dplyr::left_join(., nrc, by = "word") 



ggplot2::theme_set(ggthemes::theme_economist())

afinn_barplot <- anime_sentiment %>%
                    dplyr::filter(!is.na(score)) %>%
                    dplyr::select(genre, word, tf_idf, score) %>%
                    dplyr::group_by(genre) %>%
                    dplyr::summarize(mean_sentiment = mean(score)) %>% 
                    dplyr::mutate(genre = reorder(genre, mean_sentiment)) %>%
                    ggplot2::ggplot(ggplot2::aes(x=genre, 
                                                 y = mean_sentiment,
                                                 fill = mean_sentiment)) +
                    ggplot2::geom_col() +
                    ggplot2::coord_flip() + 
                    ggplot2::labs(title = "Average Sentiment Rating by Genre using AFINN lexicon",
                                  x = "",
                                  y = "Sentiment (negative to positive)",
                                  caption = "figure by @EmilyHHo") +
                    ggplot2::scale_fill_gradient(low = "black", high = "red")
  
# using nrc sentiment dictionary
nrc_biplot <- anime_sentiment %>% 
                dplyr::select(genre, sentiment) %>% 
                na.omit() %>%
                table(.) %>%
                FactoMineR::CA(., graph = F) %>%
                factoextra::fviz_ca_biplot(., repel = T, labelsize = 5,
                                           title = "Correspondence Analysis of Genre and Sentiment using NRC lexicon")

gridExtra::grid.arrange(afinn_barplot, nrc_biplot, ncol = 2)

 


