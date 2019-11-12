## ----setup, include=TRUE-------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
library(lubridate)
library(ggplot2)


## ----read and explore data-----------------------------------------------
# read in data
tvData <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-08/IMDb_Economist_tv_ratings.csv")

### examine basic characteristics of data
dim(tvData)
head(tvData)

# we should convert the date info from an integer to a date format
tvData$dateFormatted <- date(tvData$date)

# then we can save just the year, to simplify future viewing
tvData$year <- year(tvData$dateFormatted)

# what range does the data span?
range(tvData$dateFormatted)

# how many unique shows?
length(unique(tvData$title))

# how many genres?
length(unique(tvData$genres))
# 97! but this includes "combo genres"


## ----create genre categorizations----------------------------------------
# create list of all genres
genres <- unique(unlist(strsplit(as.character(tvData$genres), ",")))

# looping through each genre to create dummy codes
for (i in 1:length(genres)) {
  tvData[,genres[i]] <- as.numeric(grepl(genres[i], as.character(tvData$genres)))
}

# examine overall rates of all categories
colMeans(tvData[,genres])

# save the most common secondary categories
commonCats <- which(colMeans(tvData[,genres]) > .10)

# create a data set that contains the proportion of genre by year
genreProps <- tvData %>%
  group_by(year) %>%
  summarise_at(mean, .vars=genres) %>%
  gather(key="genreCat", value="Proportion", genres[2:length(genres)])


## ----plot----------------------------------------------------------------
ggplot(data=genreProps[which(genreProps$genreCat %in% names(commonCats)),], aes(y=Proportion, x=year))+
  geom_line(aes(color=genreCat))+
  geom_point(aes(color=genreCat))+
  geom_line(stat="smooth", method="loess", se=FALSE, color="black", lty=1, alpha=0.75)+
  theme_bw()+
  facet_grid(.~genreCat)+
  geom_hline(yintercept=0.25, lty=2, color="red")+
  theme(legend.position="none", plot.title=element_text(hjust=0.5))+
  labs(title="Common Secondary Categorizations \n of Dramas from '90 to '18")+
  scale_x_continuous(breaks=c(1990,2000,2010),
                     labels=c("'90","'00","'10"))

