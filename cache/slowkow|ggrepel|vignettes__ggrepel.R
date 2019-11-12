## ----setup, echo=FALSE, results='hide', warning=FALSE, error=FALSE, message=FALSE, cache=FALSE----
# output:
#   prettydoc::html_pretty:
#     theme: hpstr
#     highlight: github
#     toc: true
#     mathjax: null
#     self_contained: true

# output:
#   html_document:
#     css: style.css
#     highlight: pygments
#     mathjax: null
#     self_contained: true
#     toc: true
#     toc_float:
#       collapsed: false
#       smooth_scroll: false
library(knitr)
opts_chunk$set(
  cache       = FALSE,
  autodep     = TRUE,
  echo        = FALSE,
  warning     = FALSE,
  error       = FALSE,
  message     = FALSE,
  out.width   = 700,
  fig.width   = 12,
  fig.height  = 8,
  dpi         = 300,
  cache.path  = "cache/ggrepel/",
  fig.path    = "figures/ggrepel/",
  pngquant    = "--speed=1 --quality=0-10",
  concordance = TRUE
)
knit_hooks$set(
  pngquant = hook_pngquant
)
library(gridExtra)
library(ggplot2)
theme_set(theme_classic(base_size = 18) %+replace% theme(
  # axis.line.y = element_line(colour = "black", size = 0.2),
  # axis.line.x = element_line(colour = "black", size = 0.2),
  axis.ticks   = element_line(colour = "black", size = 0.3),
  panel.border = element_rect(size = 0.3, fill = NA),
  axis.line    = element_blank(),
  plot.title   = element_text(size = 18, vjust = 2, hjust = 0.5),
  strip.text   = element_text(size = 18),
  strip.background = element_blank()
))


## ----comparison, echo=TRUE, fig.width=9, fig.height=4--------------------
library(ggrepel)
set.seed(42)

dat <- subset(mtcars, wt > 2.75 & wt < 3.45)
dat$car <- rownames(dat)

p <- ggplot(dat, aes(wt, mpg, label = car)) +
  geom_point(color = "red")

p1 <- p + geom_text() + labs(title = "geom_text()")

p2 <- p + geom_text_repel() + labs(title = "geom_text_repel()")

gridExtra::grid.arrange(p1, p2, ncol = 2)


## ----install-cran, echo=TRUE, eval=FALSE---------------------------------
## install.packages("ggrepel")


## ----install-github, echo=TRUE, eval=FALSE-------------------------------
## # Use the devtools package
## # install.packages("devtools")
## devtools::install_github("slowkow/ggrepel")


## ----empty_string, echo=TRUE, fig.width=5.5, fig.height=4----------------
set.seed(42)

dat2 <- subset(mtcars, wt > 3 & wt < 4)
# Hide all of the text labels.
dat2$car <- ""
# Let's just label these items.
ix_label <- c(2, 3, 14)
dat2$car[ix_label] <- rownames(dat2)[ix_label]

ggplot(dat2, aes(wt, mpg, label = car)) +
  geom_text_repel() +
  geom_point(color = ifelse(dat2$car == "", "grey50", "red"))


## ----empty_string_big, echo=TRUE, fig.width=6, fig.height=3--------------
set.seed(42)

dat3 <- rbind(
  data.frame(
    wt  = rnorm(n = 10000, mean = 3),
    mpg = rnorm(n = 10000, mean = 19),
    car = ""
  ),
  dat2[,c("wt", "mpg", "car")]
)

ggplot(dat3, aes(wt, mpg, label = car)) +
  geom_point(data = dat3[dat3$car == "",], color = "grey50") +
  geom_text_repel(box.padding = 0.5) +
  geom_point(data = dat3[dat3$car != "",], color = "red")


## ----point_size_na, echo=TRUE, fig.width=5, fig.height=4-----------------
set.seed(42)
ggplot(dat, aes(wt, mpg, label = car)) +
  geom_point(color = "red") +
  geom_text_repel(point.size = NA)


## ----line_curve, echo=TRUE, fig.width=5, fig.height=4--------------------
set.seed(42)

ggplot(dat, aes(wt, mpg, label = car)) +
  geom_point(color = "red") +
  geom_text_repel(
    nudge_x = .15,
    box.padding = 0.5,
    nudge_y = 1,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20
  )


## ----line_curve_annotate, echo=TRUE, fig.width=5, fig.height=4-----------
set.seed(42)
cars <- c("Volvo 142E", "Merc 230")

ggplot(dat) +
  aes(wt, mpg, label = ifelse(car %in% cars, car, "")) +
  geom_point(color = "red") +
  geom_text_repel(
    point.padding = 0.2, 
    nudge_x = .15,
    nudge_y = .5,
    segment.curvature = -1e-20,
    arrow = arrow(length = unit(0.015, "npc"))
  ) +
  theme(legend.position = "none")


## ----all_linetypes, echo=FALSE, fig.width=5, fig.height=1----------------
dat_linetype <- data.frame(
  x = 1, xend = 2,
  y = 1, yend = 2,
  linetype = factor(1:6)
)
ggplot(dat_linetype) +
  aes(x = x, xend = xend, y = y, yend = yend, linetype = linetype) +
  geom_segment() +
  facet_grid(~ linetype) +
  theme_void() +
  theme(strip.text = element_text(size = 14), plot.title = element_text(size = 14, hjust = 0.5), legend.position = "none")


## ----all_arrows, echo=FALSE, fig.width=5, fig.height=1-------------------
dat_arrow <- data.frame(
  x = 1, xend = 2,
  y = 1, yend = 2,
  angle = seq(30, 90, length.out = 6),
  linetype = c(2, 1, 4, 1, 3, 1),
  length = 5,
  ends = rep(c("last", "first", "both"), length.out = 6),
  type = rep(c("open", "closed"), length.out = 6)
)
plots <- lapply(1:nrow(dat_arrow), function(i) {
  ggplot(dat_arrow[i,]) +
  aes(x = x, xend = xend, y = y, yend = yend) +
  geom_segment(linetype = dat_arrow$linetype[i],
    arrow = with(dat_arrow[i,], arrow(angle, unit(length, "mm"), ends, type))
  ) +
  scale_x_continuous(expand = expansion(add = 0.5)) +
  scale_y_continuous(expand = expansion(add = 0.5)) +
  theme_void() +
  theme(strip.text = element_text(size = 14), plot.title = element_text(size = 14, hjust = 0.5), legend.position = "none")
})
gridExtra::grid.arrange(
  plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]],
  nrow = 1, ncol = nrow(dat_arrow)
)


## ----line_curve_annotate_linetype, echo=TRUE, fig.width=5, fig.height=4----
set.seed(42)
cars <- c("Volvo 142E", "Merc 230")

ggplot(dat, aes(wt, mpg, label = ifelse(car %in% cars, car, ""))) +
  geom_point(color = "red") +
  geom_text_repel(
    point.padding = 0.2, 
    nudge_x = .15,
    nudge_y = .5,
    segment.linetype = 6,
    segment.curvature = -1e-20,
    arrow = arrow(length = unit(0.015, "npc"))
  )


## ----point_size_cars, echo=TRUE, fig.width=8, fig.height=4---------------
my_pal <- function(range = c(1, 6)) {
  force(range)
  function(x) scales::rescale(x, to = range, from = c(0, 1))
}

ggplot(dat, aes(wt, mpg, label = car)) +
  geom_point(aes(size = cyl), alpha = 0.6) + # data point size
  continuous_scale(
    aesthetics = c("size", "point.size"), scale_name = "size",
    palette = my_pal(c(2, 15)),
    guide = guide_legend(override.aes = list(label = "")) # hide "a" in legend
  ) +
  geom_text_repel(
    aes(point.size = cyl), # data point size
    size = 5, # font size in the text labels
    point.padding = 0, # additional padding around each point
    min.segment.length = 0, # draw all line segments
    max.time = 1, max.iter = 1e5, # stop after 1 second, or after 100,000 iterations
    box.padding = 0.3 # additional padding around each text label
  ) +
  theme(legend.position = "right")


## ----point_size_cars_label, echo=TRUE, fig.width=8, fig.height=4---------
my_pal <- function(range = c(1, 6)) {
  force(range)
  function(x) scales::rescale(x, to = range, from = c(0, 1))
}

ggplot(dat, aes(wt, mpg, label = car)) +
  geom_label_repel(
    aes(point.size = cyl), # data point size
    size = 5, # font size in the text labels
    point.padding = 0, # additional padding around each point
    min.segment.length = 0, # draw all line segments
    max.time = 1, max.iter = 1e5, # stop after 1 second, or after 100,000 iterations
    box.padding = 0.3 # additional padding around each text label
  ) +
  # Put geom_point() after geom_label_repel, so the
  # legend for geom_point() appears on the top layer.
  geom_point(aes(size = cyl), alpha = 0.6) + # data point size
  continuous_scale(
    aesthetics = c("size", "point.size"),
    scale_name = "size",
    palette = my_pal(c(2, 15)),
    guide = guide_legend(override.aes = list(label = "")) # hide "a" in legend
  ) +
  theme(legend.position = "right")


## ----xlim, echo=TRUE, fig.width=6, fig.height=4--------------------------
set.seed(42)

# All labels should be to the right of 3.
x_limits <- c(3, NA)

ggplot(dat) +
  aes(
    x = wt, y = mpg, label = car,
    color = factor(cyl), segment.color = factor(cyl)
  ) +
  geom_vline(xintercept = x_limits, linetype = 3) +
  geom_point() +
  geom_label_repel(
    arrow = arrow(
      length = unit(0.03, "npc"), type = "closed", ends = "first"
    ),
    xlim  = x_limits,
    point.padding = NA,
    box.padding = 0.1
  ) +
  scale_color_discrete(
    name = "cyl",
    aesthetics = c("color", "segment.color")
  )


## ----direction_x, echo=TRUE, fig.width=9, fig.height=3-------------------
set.seed(42)

ggplot(mtcars, aes(x = wt, y = 1, label = rownames(mtcars))) +
  geom_point(color = "red") +
  geom_text_repel(
    force        = 0.5,
    nudge_y      = 0.05,
    direction    = "x",
    angle        = 90,
    vjust        = 0,
    segment.size = 0.2
  ) +
  xlim(1, 6) +
  ylim(1, 0.8) +
  theme(
    axis.line.y  = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.title.y = element_blank()
  )


## ----neat-offset-x, echo=TRUE, fig.width=7, fig.height=4-----------------
set.seed(42)

dat <- mtcars
dat$car <- rownames(dat)

ggplot(dat, aes(qsec, mpg, label = car)) +
  geom_text_repel(
    data          = subset(dat, mpg > 30),
    nudge_y       = 36 - subset(dat, mpg > 30)$mpg,
    segment.size  = 0.2,
    segment.color = "grey50",
    direction     = "x"
  ) +
  geom_point(color = ifelse(dat$mpg > 30, "red", "black")) +
  scale_x_continuous(expand = c(0.05, 0.05)) +
  scale_y_continuous(limits = c(NA, 36))


## ----direction_y, echo=TRUE, fig.width=10, fig.height=8------------------
set.seed(42)

p <- ggplot(mtcars, aes(y = wt, x = 1, label = rownames(mtcars))) +
  geom_point(color = "red") +
  ylim(1, 5.5) +
  theme(
    axis.line.x  = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.title.x = element_blank()
  )

p1 <- p +
  xlim(1, 1.375) +
  geom_text_repel(
    force        = 0.5,
    nudge_x      = 0.15,
    direction    = "y",
    hjust        = 0,
    segment.size = 0.2
  ) +
  ggtitle("hjust = 0")

p2 <- p + 
  xlim(1, 1.375) +
  geom_text_repel(
    force        = 0.5,
    nudge_x      = 0.2,
    direction    = "y",
    hjust        = 0.5,
    segment.size = 0.2
  ) +
  ggtitle("hjust = 0.5 (default)")

p3 <- p +
  xlim(0.25, 1) +
  scale_y_continuous(position = "right") +
  geom_text_repel(
    force        = 0.5,
    nudge_x      = -0.25,
    direction    = "y",
    hjust        = 1,
    segment.size = 0.2
  ) +
  ggtitle("hjust = 1")

gridExtra::grid.arrange(p1, p2, p3, ncol = 3)


## ----neat-offset-y, echo=TRUE, fig.width=7, fig.height=3-----------------
set.seed(42)

dat <- subset(mtcars, wt > 2.75 & wt < 3.45)
dat$car <- rownames(dat)

ggplot(dat, aes(wt, mpg, label = car)) +
  geom_text_repel(
    data          = subset(dat, wt > 3),
    nudge_x       = 3.5 - subset(dat, wt > 3)$wt,
    segment.size  = 0.2,
    segment.color = "grey50",
    direction     = "y",
    hjust         = 0
  ) +
  geom_text_repel(
    data          = subset(dat, wt < 3),
    nudge_x       = 2.7 - subset(dat, wt < 3)$wt,
    segment.size  = 0.2,
    segment.color = "grey50",
    direction     = "y",
    hjust         = 1
  ) +
  scale_x_continuous(
    breaks = c(2.5, 2.75, 3, 3.25, 3.5),
    limits = c(2.4, 3.8)
  ) +
  geom_point(color = "red")


## ----ggplot2, echo=TRUE, eval=FALSE--------------------------------------
## # install.packages("devtools")
## devtools::install_github("tidyverse/ggplot2")


## ----jitter, echo=TRUE, fig.width=6, fig.height=5------------------------
mtcars$label <- rownames(mtcars)
mtcars$label[mtcars$cyl != 6] <- ""

# New! (not available in ggplot2 version 2.2.1 or earlier)
pos <- position_jitter(width = 0.3, seed = 2)

ggplot(mtcars, aes(factor(cyl), mpg, color = label != "", label = label)) +
  geom_point(position = pos) +
  geom_text_repel(position = pos) +
  theme(legend.position = "none") +
  labs(title = "position_jitter()")


## ----quasirandom, echo=TRUE, fig.width=6, fig.height=5-------------------
mtcars$label <- rownames(mtcars)
mtcars$label[mtcars$cyl != 6] <- ""

library(ggbeeswarm)
pos <- position_quasirandom()

ggplot(mtcars, aes(factor(cyl), mpg, color = label != "", label = label)) +
  geom_point(position = pos) +
  geom_text_repel(position = pos) +
  theme(legend.position = "none") +
  labs(title = "position_quasirandom()")


## ----shadowtext, echo=TRUE, fig.width=5, fig.height=4--------------------
set.seed(42)
ggplot(dat, aes(wt, mpg, label = car)) +
  geom_point(color = "red") +
  geom_text_repel(color = "white", bg.color = "black", bg.r = 0.15)


## ----wordcloud, echo=TRUE, fig.width=7, fig.height=2---------------------
set.seed(42)
ggplot(mtcars) +
  geom_text_repel(
    aes(
      label  = rownames(mtcars),
      size   = mpg > 15,
      colour = factor(cyl),
      x      = 0,
      y      = 0
    ),
    force_pull    = 0, # do not pull text toward the point at (0,0)
    max.time      = 0.5,
    max.iter      = 1e5,
    segment.color = NA,
    point.padding = NA
  ) +
  theme_void() +
  theme(strip.text = element_text(size = 16)) +
  facet_wrap(~ factor(cyl)) +
  scale_color_discrete(name = "Cylinders") +
  scale_size_manual(values = c(2, 3)) +
  theme(
    strip.text   = element_blank(),
    panel.border = element_rect(size = 0.2, fill = NA)
  )


## ----polar, echo=TRUE, fig.width=5, fig.height=4-------------------------
set.seed(42)

mtcars$label <- rownames(mtcars)
mtcars$label[mtcars$mpg < 25] <- ""

ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl), label = label)) +
  coord_polar(theta = "x") +
  geom_point(size = 2) +
  scale_color_discrete(name = "cyl") +
  geom_text_repel(show.legend = FALSE) + # Don't display "a" in the legend.
  theme_bw(base_size = 18)


## ----japanese, echo=TRUE, fig.width=6, fig.height=5----------------------
library(ggrepel)

set.seed(42)
dat <- data.frame(
  x = runif(32),
  y = runif(32),
  label = strsplit(
    x = "原文篭毛與美篭母乳布久思毛與美夫君志持此岳尓菜採須兒家吉閑名思毛",
    split = ""
  )[[1]]
)

# Make sure to choose a font that is installed on your system.
my_font <- "HiraKakuProN-W3"

ggplot(dat, aes(x, y, label = label)) +
  geom_point(size = 2, color = "red") +
  geom_text_repel(size = 8, family = my_font) +
  ggtitle("テスト") +
  theme_bw(base_size = 18, base_family = my_font)


## ----math, echo=TRUE, fig.width=5, fig.height=4--------------------------
d <- data.frame(
  x    = c(1, 2, 2, 1.75, 1.25),
  y    = c(1, 3, 1, 2.65, 1.25),
  math = c(
    NA,
    "integral(f(x) * dx, a, b)",
    NA,
    "lim(f(x), x %->% 0)",
    NA
  )
)

ggplot(d, aes(x, y, label = math)) +
  geom_point() +
  geom_label_repel(
    parse       = TRUE, # Parse mathematical expressions.
    size        = 6,
    box.padding = 2
  )


## ----animated, echo=TRUE, eval=FALSE-------------------------------------
## # This chunk of code will take a minute or two to run.
## library(ggrepel)
## library(animation)
## 
## plot_frame <- function(n) {
##   set.seed(42)
##   p <- ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars))) +
##     geom_text_repel(
##       size = 5, force = 1, max.iter = n
##     ) +
##     geom_point(color = "red") +
##     # theme_minimal(base_size = 16) +
##     labs(title = n)
##   print(p)
## }
## 
## xs <- ceiling(1.18^(1:52))
## # xs <- ceiling(1.4^(1:26))
## xs <- c(xs, rep(xs[length(xs)], 15))
## # plot(xs)
## 
## saveGIF(
##   lapply(xs, function(i) {
##     plot_frame(i)
##   }),
##   interval   = 0.15,
##   ani.width  = 800,
##   ani.heigth = 600,
##   movie.name = "animated.gif"
## )


## ----session_info, echo=TRUE---------------------------------------------
sessionInfo()

