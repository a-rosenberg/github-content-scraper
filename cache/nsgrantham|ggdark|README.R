## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "README-"
)


## ----cran-installation, eval = FALSE-------------------------------------
## install.packages("ggdark")


## ----gh-installation, eval = FALSE---------------------------------------
## # install.packages("devtools")
## devtools::install_github("nsgrantham/ggdark")


## ----gray, fig.path="man/figures/"---------------------------------------
library(ggplot2)

p <- ggplot(diamonds) + 
  geom_point(aes(carat, price, color = cut)) + 
  scale_y_continuous(label = scales::dollar) +
  guides(color = guide_legend(reverse = TRUE)) +
  labs(title = "Prices of 50,000 round cut diamonds by carat and cut",
       x = "Weight (carats)",
       y = "Price in US dollars",
       color = "Quality of the cut")

p + theme_gray()  # ggplot default


## ----dark-gray, fig.path="man/figures/"----------------------------------
library(ggdark)

p + dark_theme_gray()  # the dark version


## ----add-element, fig.path="man/figures/"--------------------------------
# modify the theme to your liking, as you would in ggplot2
p + dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14) + 
  theme(plot.title = element_text(family = "Fira Sans Condensed"),
        plot.background = element_rect(fill = "grey10"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.815, 0.27))


## ----mtcars, fig.path="man/figures/"-------------------------------------
mtcars2 <- within(mtcars, {
  vs <- factor(vs, labels = c("V-shaped", "Straight"))
  am <- factor(am, labels = c("Automatic", "Manual"))
  cyl  <- factor(cyl)
  gear <- factor(gear)
})

p <- ggplot(mtcars2) +
  geom_point(aes(wt, mpg, color = gear)) +
  facet_grid(vs ~ am) +
  labs(title = "Fuel economy declines as weight increases",
       subtitle = "(1973-74)",
       caption = "Data from the 1974 Motor Trend US magazine.",
       x = "Weight (1000 lbs)",
       y = "Fuel economy (mpg)",
       color = "Gears")


## ----all-themes, fig.path="man/figures/"---------------------------------
p + dark_theme_gray()
p + dark_theme_bw()
p + dark_theme_linedraw()
p + dark_theme_light()  # quite dark
p + dark_theme_dark()  # quite light
p + dark_theme_minimal()
p + dark_theme_classic()
p + dark_theme_void()


## ----gapminder, fig.path="man/figures/"----------------------------------
invert_geom_defaults()  # change geom defaults back to black

library(gapminder)

p <- ggplot(subset(gapminder, continent != "Oceania")) +
  geom_line(aes(year, lifeExp, group = country, color = country), lwd = 1, show.legend = FALSE) + 
  facet_wrap(~ continent) +
  scale_color_manual(values = country_colors) +
  labs(title = "Life expectancy has increased worldwide")


## ----fivethirtyeight, fig.path="man/figures/"----------------------------
# install.packages("ggthemes")
library(ggthemes)

p + theme_fivethirtyeight()


## ----dark-fivethirtyeight, fig.path="man/figures/"-----------------------
p + dark_mode(theme_fivethirtyeight())


## ----reset-defaults------------------------------------------------------
invert_geom_defaults()  # leave the geom defaults how you found them!

