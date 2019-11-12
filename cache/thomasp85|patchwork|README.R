## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/README-",
  message = FALSE
)


## ----gh-installation, eval = FALSE---------------------------------------
## # install.packages("devtools")
## devtools::install_github("thomasp85/patchwork")


## ----example-------------------------------------------------------------
library(ggplot2)
library(patchwork)

p1 <- ggplot(mtcars) + geom_point(aes(mpg, disp))
p2 <- ggplot(mtcars) + geom_boxplot(aes(gear, disp, group = gear))

p1 + p2


## ------------------------------------------------------------------------
ggplot(mtcars) +
  geom_point(aes(mpg, disp)) +
  ggplot(mtcars) + 
  geom_boxplot(aes(gear, disp, group = gear))


## ------------------------------------------------------------------------
p1 + p2 + plot_layout(ncol = 1, heights = c(3, 1))


## ------------------------------------------------------------------------
p1 + plot_spacer() + p2


## ------------------------------------------------------------------------
p3 <- ggplot(mtcars) + geom_smooth(aes(disp, qsec))
p4 <- ggplot(mtcars) + geom_bar(aes(carb))

p4 + {
  p1 + {
    p2 +
      p3 +
      plot_layout(ncol = 1)
  }
} +
  plot_layout(ncol = 1)


## ------------------------------------------------------------------------
p1 + p2 + plot_annotation(title = "A great plot!", tag_levels = "A")


## ------------------------------------------------------------------------
p1 + p2 + p3 + plot_layout(ncol = 1)


## ------------------------------------------------------------------------
p1 + p2 - p3 + plot_layout(ncol = 1)


## ------------------------------------------------------------------------
(p1 | p2 | p3) /
      p4


## ------------------------------------------------------------------------
(p1 + (p2 + p3) + p4 + plot_layout(ncol = 1)) * theme_bw()


## ------------------------------------------------------------------------
p1 + (p2 + p3) + p4 + plot_layout(ncol = 1) & theme_bw()

