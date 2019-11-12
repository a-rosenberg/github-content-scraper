## ---- message = FALSE----------------------------------------------------
library(tidyverse)


## ---- message = FALSE----------------------------------------------------
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(title = "Fuel efficiency generally decreases with engine size")


## ---- message = FALSE----------------------------------------------------
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Fuel efficiency generally decreases with engine size",
    subtitle = "Two seaters (sports cars) are an exception because of their light weight",
    caption = "Data from fueleconomy.gov"
  )


## ---- message = FALSE----------------------------------------------------
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  geom_smooth(se = FALSE) +
  labs(
    x = "Engine displacement (L)",
    y = "Highway fuel economy (mpg)",
    colour = "Car type"
  )


## ---- fig.asp = 1, out.width = "50%", fig.width = 3----------------------
df <- tibble(
  x = runif(10),
  y = runif(10)
)
ggplot(df, aes(x, y)) +
  geom_point() +
  labs(
    x = quote(sum(x[i] ^ 2, i == 1, n)),
    y = quote(alpha + beta + frac(delta, theta))
  )


## ------------------------------------------------------------------------
best_in_class <- mpg %>%
  group_by(class) %>%
  filter(row_number(desc(hwy)) == 1)

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  geom_text(aes(label = model), data = best_in_class)


## ------------------------------------------------------------------------
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  geom_label(aes(label = model), data = best_in_class, nudge_y = 2, alpha = 0.5)


## ------------------------------------------------------------------------
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  geom_point(size = 3, shape = 1, data = best_in_class) +
  ggrepel::geom_label_repel(aes(label = model), data = best_in_class)


## ------------------------------------------------------------------------
class_avg <- mpg %>%
  group_by(class) %>%
  summarise(
    displ = median(displ),
    hwy = median(hwy)
  )

ggplot(mpg, aes(displ, hwy, colour = class)) +
  ggrepel::geom_label_repel(aes(label = class),
    data = class_avg,
    size = 6,
    label.size = 0,
    segment.color = NA
  ) +
  geom_point() +
  theme(legend.position = "none")


## ------------------------------------------------------------------------
label <- mpg %>%
  summarise(
    displ = max(displ),
    hwy = max(hwy),
    label = "Increasing engine size is \nrelated to decreasing fuel economy."
  )

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_text(aes(label = label), data = label, vjust = "top", hjust = "right")


## ------------------------------------------------------------------------
label <- tibble(
  displ = Inf,
  hwy = Inf,
  label = "Increasing engine size is \nrelated to decreasing fuel economy."
)

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_text(aes(label = label), data = label, vjust = "top", hjust = "right")


## ------------------------------------------------------------------------
"Increasing engine size is related to decreasing fuel economy." %>%
  stringr::str_wrap(width = 40) %>%
  writeLines()


## ----just, echo = FALSE, fig.cap = "All nine combinations of `hjust` and `vjust`.", fig.asp = 0.5, fig.width = 4.5, out.width = "60%"----
vjust <- c(bottom = 0, center = 0.5, top = 1)
hjust <- c(left = 0, center = 0.5, right = 1)

df <- tidyr::crossing(hj = names(hjust), vj = names(vjust)) %>%
  mutate(
    y = vjust[vj],
    x = hjust[hj],
    label = paste0("hjust = '", hj, "'\n", "vjust = '", vj, "'")
  )

ggplot(df, aes(x, y)) +
  geom_point(colour = "grey70", size = 5) +
  geom_point(size = 0.5, colour = "red") +
  geom_text(aes(label = label, hjust = hj, vjust = vj), size = 4) +
  labs(x = NULL, y = NULL) 


## ----default-scales, fig.show = "hide"-----------------------------------
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class))


## ---- fig.show = "hide"--------------------------------------------------
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  scale_x_continuous() +
  scale_y_continuous() +
  scale_colour_discrete()


## ------------------------------------------------------------------------
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  scale_y_continuous(breaks = seq(15, 40, by = 5))


## ------------------------------------------------------------------------
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  scale_x_continuous(labels = NULL) +
  scale_y_continuous(labels = NULL)


## ------------------------------------------------------------------------
presidential %>%
  mutate(id = 33 + row_number()) %>%
  ggplot(aes(start, id)) +
    geom_point() +
    geom_segment(aes(xend = end, yend = id)) +
    scale_x_date(NULL, breaks = presidential$start, date_labels = "'%y")


## ----fig.asp = 1, fig.align = "default", out.width = "50%", fig.width = 4----
base <- ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class))

base + theme(legend.position = "left")
base + theme(legend.position = "top")
base + theme(legend.position = "bottom")
base + theme(legend.position = "right") # the default


## ------------------------------------------------------------------------
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  geom_smooth(se = FALSE) +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(nrow = 1, override.aes = list(size = 4)))


## ---- fig.align = "default", out.width = "50%"---------------------------
ggplot(diamonds, aes(carat, price)) +
  geom_bin2d()

ggplot(diamonds, aes(log10(carat), log10(price))) +
  geom_bin2d()


## ------------------------------------------------------------------------
ggplot(diamonds, aes(carat, price)) +
  geom_bin2d() + 
  scale_x_log10() + 
  scale_y_log10()


## ---- fig.align = "default", out.width = "50%"---------------------------
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv))

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv)) +
  scale_colour_brewer(palette = "Set1")


## ------------------------------------------------------------------------
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv, shape = drv)) +
  scale_colour_brewer(palette = "Set1")


## ----brewer, fig.asp = 2.5, echo = FALSE, fig.cap = "All ColourBrewer scales."----
par(mar = c(0, 3, 0, 0))
RColorBrewer::display.brewer.all()


## ------------------------------------------------------------------------
presidential %>%
  mutate(id = 33 + row_number()) %>%
  ggplot(aes(start, id, colour = party)) +
    geom_point() +
    geom_segment(aes(xend = end, yend = id)) +
    scale_colour_manual(values = c(Republican = "red", Democratic = "blue"))


## ---- fig.align = "default", fig.asp = 1, out.width = "50%", fig.width = 4----
df <- tibble(
  x = rnorm(10000),
  y = rnorm(10000)
)
ggplot(df, aes(x, y)) +
  geom_hex() +
  coord_fixed()

ggplot(df, aes(x, y)) +
  geom_hex() +
  viridis::scale_fill_viridis() +
  coord_fixed()


## ----fig.show = "hide"---------------------------------------------------
ggplot(df, aes(x, y)) +
  geom_hex() +
  scale_colour_gradient(low = "white", high = "red") +
  coord_fixed()


## ---- dev = "png", out.width = "50%"-------------------------------------
ggplot(diamonds, aes(carat, price)) +
  geom_point(aes(colour = cut), alpha = 1/20)


## ----out.width = "50%", fig.align = "default", message = FALSE-----------
ggplot(mpg, mapping = aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth() +
  coord_cartesian(xlim = c(5, 7), ylim = c(10, 30))

mpg %>%
  filter(displ >= 5, displ <= 7, hwy >= 10, hwy <= 30) %>%
  ggplot(aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth()


## ----out.width = "50%", fig.align = "default", fig.width = 4-------------
suv <- mpg %>% filter(class == "suv")
compact <- mpg %>% filter(class == "compact")

ggplot(suv, aes(displ, hwy, colour = drv)) +
  geom_point()

ggplot(compact, aes(displ, hwy, colour = drv)) +
  geom_point()


## ----out.width = "50%", fig.align = "default", fig.width = 4-------------
x_scale <- scale_x_continuous(limits = range(mpg$displ))
y_scale <- scale_y_continuous(limits = range(mpg$hwy))
col_scale <- scale_colour_discrete(limits = unique(mpg$drv))

ggplot(suv, aes(displ, hwy, colour = drv)) +
  geom_point() +
  x_scale +
  y_scale +
  col_scale

ggplot(compact, aes(displ, hwy, colour = drv)) +
  geom_point() +
  x_scale +
  y_scale +
  col_scale


## ---- message = FALSE----------------------------------------------------
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  theme_bw()


## ----themes, echo = FALSE, fig.cap = "The eight themes built-in to ggplot2."----
knitr::include_graphics("images/visualization-themes.png")


## ---- fig.show = "none"--------------------------------------------------
ggplot(mpg, aes(displ, hwy)) + geom_point()
ggsave("my-plot.pdf")

## ---- include = FALSE----------------------------------------------------
file.remove("my-plot.pdf")


## ---- include = FALSE----------------------------------------------------
plot <- ggplot(mpg, aes(displ, hwy)) + geom_point()

## ---- fig.width = 4, echo = FALSE----------------------------------------
plot

## ---- fig.width = 6, echo = FALSE----------------------------------------
plot

## ---- fig.width = 8, echo = FALSE----------------------------------------
plot

