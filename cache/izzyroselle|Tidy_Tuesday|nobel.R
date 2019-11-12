## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---- results = 'hide'---------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)


## ------------------------------------------------------------------------
1595
nobel_winner_all_pubs <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winner_all_pubs.csv")
nobel_winners <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")


## ------------------------------------------------------------------------
n <- nobel_winners
np <- nobel_winner_all_pubs
head(np)
ne <- aggregate(pub_year ~ laureate_name, data = np, min)
g <- np[which(np$is_prize_winning_paper == 'YES'),]
g2 <- g[, c(1,2,3, 5),]
nbig <- aggregate(pub_year ~ laureate_name, data = np, max)
b <- left_join(g2, ne, by = 'laureate_name') %>% left_join(., nbig, by = 'laureate_name')
names (b) [4] <- 'prizepub'
names (b) [5] <- 'firstpub'
names (b) [6] <- 'lastpub'
f <- np[, c(2, 11)]
b2 <- left_join(b, f, by = 'laureate_name')
b3 <- b2[which(!duplicated(b2$laureate_id)),]


## ----cars----------------------------------------------------------------

ggplot(b3, aes(x = prizepub, y = prize_year, color = category)) + geom_point() + coord_cartesian(xlim = c(1880, 2020), ylim = c(1880, 2020)) + geom_abline(slope = 1, intercept = 0) + ggtitle('Delay in Nobel Prizes') + xlab('Publication Year of Winning Paper') + ylab('Year Prize Awarded')







## ------------------------------------------------------------------------
b3[which(b3$prizepub > 1960 & b3$prize_year < 1920),]



## ------------------------------------------------------------------------
b3[which(b3$prizepub > b3$prize_year),]


## ------------------------------------------------------------------------
np[which(np$laureate_id == 10189),]


## ------------------------------------------------------------------------
ggplot(b3[which(b3$prizepub - b3$firstpub < 50),], aes(x = category, y = prizepub - firstpub, fill = category)) + geom_boxplot() + ylab('Years Between First Publication and Winning Publication') + xlab('Category') + labs(subtitle = 'In physics, Nobel laureates hit their stride soon after first publication.') + ggtitle('Incubation Period of the Lesser Spotted Nobel Laureate') + coord_flip() + theme(legend.position = 'none') + theme_classic()


## ------------------------------------------------------------------------
ggplot(b3[which(b3$prizepub - b3$firstpub < 50),], aes(x = category, y = (prizepub - firstpub)/(lastpub - firstpub), fill = category)) + geom_boxplot() + ylab('Winning paper publication date as fraction of time from first to last paper') + xlab('Category') + labs(subtitle = 'In physics, Nobel laureates hit their stride soon after first publication.') + ggtitle("How far through a laureate's career is their winning paper published?") + coord_flip()  + theme_classic() + theme(legend.position = 'none')


## ------------------------------------------------------------------------
n2 <- separate(n, birth_date, c('birthyear', NA, NA), sep = "-") 


## ------------------------------------------------------------------------
ggplot(b3, aes(x = category, y = prizepub)) + geom_boxplot()


## ------------------------------------------------------------------------
head(b3)
b4 <- gather(b3, Type, Year, prize_year:lastpub)
head(b4)


## ------------------------------------------------------------------------
ggplot(b4[which(b3$prize_year %in% c(2000:2019) & b4$category == 'medicine' & b3$prizepub - b3$firstpub < 50),], aes(x = Year, y = laureate_name, color = Type, size = 2)) + geom_point()


## ------------------------------------------------------------------------
priz <- b3[, c(1,3)]
b5 <- left_join(b4, priz, by = 'laureate_id')


## ------------------------------------------------------------------------
ggplot(b5[which(b3$prize_year %in% c(2000:2019) & b5$category == 'medicine' & b3$prizepub - b3$firstpub < 50),], aes(x = Year, y = reorder(laureate_name, prize_year), color = Type)) + geom_point() 


## ------------------------------------------------------------------------
ggplot(b5[which(b3$prize_year %in% c(2000:2019) & b5$category == 'medicine' & b3$prizepub - b3$firstpub < 50),], aes(x = Year, y = reorder(laureate_name, prize_year), color = Type, size = 2, alpha = .8)) + geom_point() + guides(size = FALSE, alpha = FALSE) + ylab('Laureate name') + ggtitle('Milestones in Nobel Laureate Publications')

