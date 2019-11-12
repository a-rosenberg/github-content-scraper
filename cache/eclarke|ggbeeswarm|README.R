## ----setup, echo=FALSE, message=FALSE------------------------------------
knitr::opts_chunk$set(
  fig.retina=2,
  fig.width=6,
  fig.height=4
)


## ---- eval=FALSE---------------------------------------------------------
## install.packages('ggbeeswarm')


## ---- eval=FALSE---------------------------------------------------------
## devtools::install_github("eclarke/ggbeeswarm")


## ----ggplot2-compare-----------------------------------------------------
set.seed(12345)
library(ggplot2)
library(ggbeeswarm)
#compare to jitter
ggplot(iris,aes(Species, Sepal.Length)) + geom_jitter()
ggplot(iris,aes(Species, Sepal.Length)) + geom_quasirandom()


## ----ggplot2-examples----------------------------------------------------

#default geom_quasirandom
ggplot(mpg,aes(class, hwy)) + geom_quasirandom()

# With categorical y-axis
ggplot(mpg,aes(hwy, class)) + geom_quasirandom(groupOnX=FALSE)

# Some groups may have only a few points. Use `varwidth=TRUE` to adjust width dynamically.
ggplot(mpg,aes(class, hwy)) + geom_quasirandom(varwidth = TRUE)

# Automatic dodging
sub_mpg <- mpg[mpg$class %in% c("midsize", "pickup", "suv"),]
ggplot(sub_mpg, aes(class, displ, color=factor(cyl))) + geom_quasirandom(dodge.width=1)


## ----ggplot2-methods,tidy=TRUE-------------------------------------------
ggplot(iris,aes(Species, Sepal.Length)) + geom_quasirandom(method='tukey') + ggtitle('Tukey texture')
ggplot(iris,aes(Species, Sepal.Length)) + geom_quasirandom(method='tukeyDense') + ggtitle('Tukey + density')
ggplot(iris,aes(Species, Sepal.Length)) + geom_quasirandom(method='frowney') + ggtitle('Banded frowns')
ggplot(iris,aes(Species, Sepal.Length)) + geom_quasirandom(method='smiley') + ggtitle('Banded smiles')
ggplot(iris,aes(Species, Sepal.Length)) + geom_quasirandom(method='pseudorandom') + ggtitle('Jittered density')
ggplot(iris,aes(Species, Sepal.Length)) + geom_beeswarm() + ggtitle('Beeswarm')


## ----ggplot2-beeswarm----------------------------------------------------

ggplot(iris,aes(Species, Sepal.Length)) + geom_beeswarm()
ggplot(iris,aes(Species, Sepal.Length)) + geom_beeswarm(beeswarmArgs=list(side=1))
ggplot(mpg,aes(class, hwy)) + geom_beeswarm(size=.5)
# With categorical y-axis
ggplot(mpg,aes(hwy, class)) + geom_beeswarm(size=.5,groupOnX=FALSE)
# Also watch out for points escaping from the plot with geom_beeswarm
ggplot(mpg,aes(hwy, class)) + geom_beeswarm(size=.5,groupOnX=FALSE) + scale_y_discrete(expand=expand_scale(add=c(0.5,1)))

ggplot(mpg,aes(class, hwy)) + geom_beeswarm(size=1.1)


# With automatic dodging
ggplot(sub_mpg, aes(class, displ, color=factor(cyl))) + geom_beeswarm(dodge.width=0.5)

#With different beeswarm point distribution priority
dat<-data.frame(x=rep(1:3,c(20,40,80)))
dat$y<-rnorm(nrow(dat),dat$x)
ggplot(dat,aes(x,y)) + geom_beeswarm(size=2) + ggtitle('Default (ascending)') + scale_x_continuous(expand=expand_scale(add=c(0.5,.5)))
ggplot(dat,aes(x,y)) + geom_beeswarm(size=2,priority='descending') + ggtitle('Descending') + scale_x_continuous(expand=expand_scale(add=c(0.5,.5)))
ggplot(dat,aes(x,y)) + geom_beeswarm(size=2,priority='density') + ggtitle('Density') + scale_x_continuous(expand=expand_scale(add=c(0.5,.5)))
ggplot(dat,aes(x,y)) + geom_beeswarm(size=2,priority='random') + ggtitle('Random') + scale_x_continuous(expand=expand_scale(add=c(0.5,.5)))

