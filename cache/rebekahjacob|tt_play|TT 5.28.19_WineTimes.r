#libraries
library(dplyr)
library(ggplot2)

#load data
wine<- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv")

#grab year from title function
yearExtract <- function(string) {
  t <- regmatches(string, regexec("[0-9]{4}", string))
  sapply(t, function(x) {
    if(length(x) > 0){
      return(as.numeric(x))
    } else {
      return(NA)    
    }
  })
}

#prices under 1000 and extract year
wine<- wine %>%
  filter(price<1000) %>%
  mutate(balance=ifelse(grepl('balanced', description), 'balanced', 'unbalanced')) %>%
  mutate(year=yearExtract(wine$title)) %>%
  filter(between(year, 1800, 2019))

#checking range of years
range(wine$year, na.rm=TRUE)

#plot points by year among balanced vs unbalanced wine
ggplot(wine, aes(y=points, x=year, col=balance)) + geom_smooth(se=F) + ggtitle("Points awarded by vintage and 'balance' mentioned in description")

#plot price by year among balanced vs unbalanced wine
ggplot(wine, aes(y=price, x=year, col=balance)) + geom_smooth(se=F) + ggtitle("Price awarded by vintage and 'balance' mentioned in description")

#points by price among balanced vs unbalanced wine
ggplot(wine, aes(y=price, x=points, col=balance)) + geom_smooth(se=F) + ggtitle("Price by Points and 'balance' mentioned in description")