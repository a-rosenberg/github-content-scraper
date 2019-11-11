#by: David C
#TidyTuesday for 7/30/19
library(tidyverse)
library(ggplot2)
library(tokenizers)
library(RColorBrewer)
video_games <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-30/video_games.csv")

#formatting dates
  video_games$release_date<-as.Date(strftime(strptime(video_games$release_date,"%b %d, %Y"),"%Y-%m-%d"))
#viz of count of releases over time
ggplot(data=video_games, aes(x=release_date)) + geom_bar(aes(fill=..count..), stat="bin", binwidth=5)+labs(title = "After SimCity",subtitle = "Count of video game releases over time" )

#formating  owners to see games with 1M owners
video_games$owners<-as.factor(video_games$owners)
t<-unique(video_games$owners) #list of groupings
video_games$million.Owners<-"no"
video_games$million.Owners[which(video_games$owners %in% t[1:4])]<-"yes"
#list of games with 1M+ owners
blockbusters<-video_games[which(video_games$million.Owners =="yes"),] 

#basic view of pricing for games with over 1,000,000 owners
p <- ggplot(data = blockbusters, aes(x = release_date, y=price,  na.rm = TRUE)) + 
  geom_col(color = "#00AFBB", size =1)
p +labs(title = "It costs $x9.99", subtitle = "Hit video game pricing over time (games with over 1,000,000 owners)") + scale_y_continuous(breaks=c(9.99,19.99, 29.99, 39.99,49.99,59.99,89.99))

#wordcloud of title words
text <- paste(video_games$game, collapse = '\n') 

#tokenize the individual words
words <- tokenizers::tokenize_words(text) #list of words from text
#add words to a table
tab <- table(words[[1]])
#turn obj into a data frame
tab <- data_frame(word = names(tab), count = as.numeric(tab))
#remove numbers & ULRS
tab$word <- tab$word %>% gsub('[0-9]', '', .) %>% gsub('http\\S+\\s*', '',.)%>% gsub('[a-z]+[.]+[a-z]', '', .)

#read in an English language word frequncy table (a local file)
wf <- read_csv('word_frequency.csv')
#join frequncy table to table of words
tab.jned<- inner_join(tab, wf)

#filter common words and stop words based on fequency and remove language columm
tab.flter<-filter(tab.jned, frequency < 0.3) %>% select(.,-language)
#find any words from original table that did not join to English language frequency table
t<-setdiff(tab$word,tab.jned$word)
tab.other<-tab %>% filter(word %in% t) %>%add_column(frequency=0) %>% arrange(., desc(count))
#of other possible words, short words removed
tab.other<- tab.other %>% filter(nchar(word)>3)

#add those word to the filtered word list
tab.final <-rbind(tab.flter,tab.other) 
#arrange by count of occurances
tab.final<-arrange(tab.final, desc(count))
head(tab.final,10)
#finally, the wordcloud
wordcloud::wordcloud(words = tab.final$word, freq = tab.final$count, scale=c(9,.5),min.freq = 1,
                     max.words=100, random.order=FALSE, rot.per=0.35, 
                     colors=brewer.pal(8, 'Dark2'))



