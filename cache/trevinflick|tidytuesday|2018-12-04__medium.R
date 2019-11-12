## ----setup, include=FALSE, warning=FALSE, message=FALSE------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)


## ------------------------------------------------------------------------
medium <- read_csv("Medium_Clean.csv")


## ------------------------------------------------------------------------
medium %>%
  sample_n(10) %>%
  View()


## ------------------------------------------------------------------------
medium %>%
  sample_n(10000, replace = FALSE) %>%
  ggplot(aes(Reading_Time, Claps)) +
  geom_point() +
  labs(x = "Reading Time in Minutes",
       y = "Claps",
       title = "There is a sweet spot around 5-10 minutes for number of claps",
       subtitle = "(Random sample of 10,000 articles)") 


## ------------------------------------------------------------------------
n_tags <- medium %>%
  select(Tag_writing:Tag_ai) %>%
  summarise_all(sum) %>%
  gather()


## ------------------------------------------------------------------------
medium %>%
  filter(Tag_food == 1) %>%
  summarise(avg_reading_time = mean(Reading_Time),
            avg_claps = mean(Claps))


## ------------------------------------------------------------------------
medium$Reading_Time <- as.numeric(medium$Reading_Time) 


## ------------------------------------------------------------------------
medium_tags <- medium %>%
  select(Reading_Time, Claps, Tag_ai:Tag_writing) %>%
  group_by_if(is.integer) %>%
  summarise(claps = mean(Claps),
            reading = mean(Reading_Time))

medium_tags$n_tags <- rowSums( medium_tags[,1:95] )

medium_tags <- medium_tags %>%
  filter(n_tags == 1) %>%
  select(-n_tags)

medium_tags[medium_tags == 0] <- NA

medium_tags <- medium_tags %>%
  select(Tag_writing:Tag_ai, claps, reading) %>%
  gather(na.rm = TRUE)

tag_data <- cbind(medium_tags[1:95,], medium_tags[96:190,])
tag_data <- cbind(tag_data, medium_tags[191:285,])

colnames(tag_data) <- c("tag", "x", "y", "claps", "z", "reading")

tag_data <- tag_data %>%
  select(tag, claps, reading)

tag_data <- cbind(tag_data, n_tags[1:95,2])

tag_data$claps <- round(tag_data$claps)
tag_data$reading <- round(tag_data$reading, digits = 2)

tag_data$tag <- gsub("Tag_", "", tag_data$tag)

names(tag_data)[4] <- "Articles"


## ----warning=FALSE, message=FALSE----------------------------------------
library(plotly)

p <- plot_ly(
  tag_data, x = ~reading, y = ~claps,
  text = ~paste("Tag: ", tag,
                "<br>Articles: ", Articles),
  size = ~Articles, color = ~Articles
) %>%
  layout(title = 'Engagement for Medium Articles',
         yaxis = list(title = 'Average number of claps'),
         xaxis = list(title = 'Average reading time'))


## ------------------------------------------------------------------------
api_create(p, filename = "tidytuesday-medium")

