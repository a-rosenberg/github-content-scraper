## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "figure/",
  fig.height = 1
)


## ---- load-package-------------------------------------------------------
library(ggplot2)
library(emoGG)


## ----find-tulip----------------------------------------------------------
emoji_search("tulip")


## ----iris_ex, fig.height=4, fig.width=4----------------------------------
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
  geom_emoji(emoji="1f337")


## ----mtcars, fig.height=4, fig.width=4-----------------------------------
ggplot(mtcars, aes(wt, mpg))+ geom_emoji(emoji="1f697")


## ---- catplotlib, fig.height=4, fig.width=4------------------------------
posx <- runif(50, 0, 10)
posy <- runif(50, 0, 10)
ggplot(data.frame(x = posx, y = posy), aes(x, y)) + geom_emoji(emoji="1f63b")


## ----big-emoji, fig.height=4, fig.width=4--------------------------------
qplot(x=Sepal.Length, y=Sepal.Width, data=iris, geom="point") + add_emoji(emoji="1f337")

