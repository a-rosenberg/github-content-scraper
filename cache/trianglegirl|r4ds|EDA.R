## ----setup, message = FALSE----------------------------------------------
library(tidyverse)


## ------------------------------------------------------------------------
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))


## ------------------------------------------------------------------------
diamonds %>% 
  count(cut)


## ------------------------------------------------------------------------
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)


## ------------------------------------------------------------------------
diamonds %>% 
  count(cut_width(carat, 0.5))


## ------------------------------------------------------------------------
smaller <- diamonds %>% 
  filter(carat < 3)
  
ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.1)


## ------------------------------------------------------------------------
ggplot(data = smaller, mapping = aes(x = carat, colour = cut)) +
  geom_freqpoly(binwidth = 0.1)


## ------------------------------------------------------------------------
ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.01)


## ------------------------------------------------------------------------
ggplot(data = faithful, mapping = aes(x = eruptions)) + 
  geom_histogram(binwidth = 0.25)


## ------------------------------------------------------------------------
ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5)


## ------------------------------------------------------------------------
ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))


## ---- include = FALSE----------------------------------------------------
old <- options(tibble.print_max = 10, tibble.print_min = 10)


## ------------------------------------------------------------------------
unusual <- diamonds %>% 
  filter(y < 3 | y > 20) %>% 
  select(price, x, y, z) %>%
  arrange(y)
unusual


## ---- include = FALSE----------------------------------------------------
options(old)


## ---- eval = FALSE-------------------------------------------------------
## diamonds2 <- diamonds %>%
##   filter(between(y, 3, 20))


## ------------------------------------------------------------------------
diamonds2 <- diamonds %>% 
  mutate(y = ifelse(y < 3 | y > 20, NA, y))


## ---- dev = "png"--------------------------------------------------------
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
  geom_point()


## ---- eval = FALSE-------------------------------------------------------
## ggplot(data = diamonds2, mapping = aes(x = x, y = y)) +
##   geom_point(na.rm = TRUE)


## ------------------------------------------------------------------------
nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(mapping = aes(sched_dep_time)) + 
    geom_freqpoly(mapping = aes(colour = cancelled), binwidth = 1/4)


## ------------------------------------------------------------------------
ggplot(data = diamonds, mapping = aes(x = price)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)


## ---- fig.width = "50%", fig.width = 4-----------------------------------
ggplot(diamonds) + 
  geom_bar(mapping = aes(x = cut))


## ------------------------------------------------------------------------
ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)


## ---- echo = FALSE, out.width = "100%"-----------------------------------
knitr::include_graphics("images/EDA-boxplot.png")


## ----fig.height = 3------------------------------------------------------
ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_boxplot()


## ------------------------------------------------------------------------
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()


## ----fig.height = 3------------------------------------------------------
ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy))


## ------------------------------------------------------------------------
ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) +
  coord_flip()


## ------------------------------------------------------------------------
ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = color))


## ------------------------------------------------------------------------
diamonds %>% 
  count(color, cut)


## ------------------------------------------------------------------------
diamonds %>% 
  count(color, cut) %>%  
  ggplot(mapping = aes(x = color, y = cut)) +
    geom_tile(mapping = aes(fill = n))


## ---- dev = "png"--------------------------------------------------------
ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price))


## ---- dev = "png"--------------------------------------------------------
ggplot(data = diamonds) + 
  geom_point(mapping = aes(x = carat, y = price), alpha = 1 / 100)


## ---- fig.asp = 1, out.width = "50%", fig.align = "default", message = FALSE----
ggplot(data = smaller) +
  geom_bin2d(mapping = aes(x = carat, y = price))

# install.packages("hexbin")
ggplot(data = smaller) +
  geom_hex(mapping = aes(x = carat, y = price))


## ------------------------------------------------------------------------
ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))


## ------------------------------------------------------------------------
ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_number(carat, 20)))


## ---- dev = "png"--------------------------------------------------------
ggplot(data = diamonds) +
  geom_point(mapping = aes(x = x, y = y)) +
  coord_cartesian(xlim = c(4, 11), ylim = c(4, 11))


## ----fig.height = 2------------------------------------------------------
ggplot(data = faithful) + 
  geom_point(mapping = aes(x = eruptions, y = waiting))


## ---- dev = "png"--------------------------------------------------------
library(modelr)

mod <- lm(log(price) ~ log(carat), data = diamonds)

diamonds2 <- diamonds %>% 
  add_residuals(mod) %>% 
  mutate(resid = exp(resid))

ggplot(data = diamonds2) + 
  geom_point(mapping = aes(x = carat, y = resid))


## ------------------------------------------------------------------------
ggplot(data = diamonds2) + 
  geom_boxplot(mapping = aes(x = cut, y = resid))


## ---- eval = FALSE-------------------------------------------------------
## ggplot(data = faithful, mapping = aes(x = eruptions)) +
##   geom_freqpoly(binwidth = 0.25)


## ---- eval = FALSE-------------------------------------------------------
## ggplot(faithful, aes(eruptions)) +
##   geom_freqpoly(binwidth = 0.25)


## ---- eval = FALSE-------------------------------------------------------
## diamonds %>%
##   count(cut, clarity) %>%
##   ggplot(aes(clarity, cut, fill = n)) +
##     geom_tile()

