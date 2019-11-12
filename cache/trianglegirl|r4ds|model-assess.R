## ----setup, message = FALSE----------------------------------------------
# Standard data manipulation and visulisation
library(dplyr)
library(ggplot2)

# Tools for working with models
library(broom)
library(modelr)
library(splines)

# Tools for working with lots of models
library(purrr)
library(tidyr)


## ------------------------------------------------------------------------
# Options that make your life easier
options(
  contrasts = c("contr.treatment", "contr.treatment"),
  na.option = na.exclude
)


## ------------------------------------------------------------------------
true_model <- function(x) {
  1 + 2 * x + rnorm(length(x), sd = 0.25)
}

df <- tibble(
  x = seq(0, 1, length = 20),
  y = true_model(x)
)

df %>% 
  ggplot(aes(x, y)) +
  geom_point()


## ---- message = FALSE----------------------------------------------------
library(splines)
my_model <- function(df) {
  lm(y ~ poly(x, 7), data = df)
}

mod <- my_model(df)
rmse(mod, df)

grid <- df %>% 
  expand(x = seq_range(x, 50))
preds <- grid %>% 
  add_predictions(mod, var = "y")

df %>% 
  ggplot(aes(x, y)) +
  geom_line(data = preds) + 
  geom_point()


## ------------------------------------------------------------------------
fs <- list(
  y ~ x,
  y ~ poly(x, 2),
  y ~ poly(x, 3),
  y ~ poly(x, 4),
  y ~ poly(x, 5),
  y ~ poly(x, 6),
  y ~ poly(x, 7)
)

models <- tibble(
  n = 1:7, 
  f = fs,
  mod = map(f, lm, data = df),
  rmse = map2_dbl(mod, list(df), rmse)
)

models %>% 
  ggplot(aes(n, rmse)) + 
  geom_line(colour = "grey70") + 
  geom_point(size = 3)


## ------------------------------------------------------------------------
boot <- bootstrap(df, 100) %>% 
  mutate(
    mod = map(strap, my_model),
    pred = map2(list(grid), mod, add_predictions)
  )

boot %>% 
  unnest(pred) %>% 
  ggplot(aes(x, pred, group = .id)) +
  geom_line(alpha = 1/3)


## ------------------------------------------------------------------------
last_plot() + 
  coord_cartesian(ylim = c(0, 5))


## ------------------------------------------------------------------------
cv <- crossv_mc(df, 100) %>% 
  mutate(
    mod = map(train, my_model),
    rmse = map2_dbl(mod, test, rmse)
  )
cv


## ------------------------------------------------------------------------
cv %>% 
  ggplot(aes(rmse)) +
  geom_ref_line(v = rmse(mod, df)) +
  geom_freqpoly(binwidth = 0.2) +
  geom_rug()


## ------------------------------------------------------------------------
filter(cv, rmse > 1.5) %>% 
  unnest(map(train, as.data.frame)) %>% 
  ggplot(aes(x, .id)) + 
    geom_point() + 
    xlim(0, 1)


## ------------------------------------------------------------------------
x <- resample_bootstrap(as_tibble(mtcars))
class(x)

x


## ------------------------------------------------------------------------
lm(mpg ~ wt, data = x)


## ------------------------------------------------------------------------
bootstrap(df, 3)


## ------------------------------------------------------------------------
crossv_mc(df, 3)


## ------------------------------------------------------------------------
heights <- tibble(readRDS("data/heights.RDS"))
h <- lm(income ~ height, data = heights)
h 

qae(h, heights)
range(heights$income)


## ------------------------------------------------------------------------
rsquare(h, heights)


## ------------------------------------------------------------------------
cor(heights$income, heights$height) ^ 2

