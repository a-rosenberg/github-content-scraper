## ----message=FALSE, warning=FALSE----------------------------------------
library(tidyverse)
library(RColorBrewer)

ramen <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-04/ramen_ratings.csv")

ramen


## ----message=FALSE, warning=FALSE----------------------------------------
ramen %>%
  group_by(brand) %>%
  summarise(n=length(brand)) %>%
  arrange(desc(n))


## ----message=FALSE, warning=FALSE----------------------------------------
brand.df<- ramen %>%
  filter(brand==c("Nissin", "Nongshim", "Maruchan") & !is.na(stars))
brand.df


## ----message=FALSE, warning=FALSE----------------------------------------
p<- ggplot(brand.df, aes(x=factor(brand), y=stars, fill=factor(style))) + geom_boxplot()
p + ggtitle("Ratings of the most popular ramen by style") + xlab("Brand") + ylab("Stars") + scale_fill_brewer(palette="Dark2") + scale_fill_discrete(name="Style")


