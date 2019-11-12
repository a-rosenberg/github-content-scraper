## ----setup, message = FALSE----------------------------------------------
library(tidyverse)


## ------------------------------------------------------------------------
df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)


## ------------------------------------------------------------------------
median(df$a)
median(df$b)
median(df$c)
median(df$d)


## ------------------------------------------------------------------------
output <- vector("double", ncol(df))  # 1. output
for (i in seq_along(df)) {            # 2. sequence
  output[[i]] <- median(df[[i]])      # 3. body
}
output


## ------------------------------------------------------------------------
y <- vector("double", 0)
seq_along(y)
1:length(y)


## ---- eval = FALSE-------------------------------------------------------
## out <- ""
## for (x in letters) {
##   out <- stringr::str_c(out, x)
## }
## 
## x <- sample(100)
## sd <- 0
## for (i in seq_along(x)) {
##   sd <- sd + (x[i] - mean(x)) ^ 2
## }
## sd <- sqrt(sd / (length(x) - 1))
## 
## x <- runif(100)
## out <- vector("numeric", length(x))
## out[1] <- x[1]
## for (i in 2:length(x)) {
##   out[i] <- out[i - 1] + x[i]
## }


## ---- eval = FALSE-------------------------------------------------------
## output <- vector("integer", 0)
## for (i in seq_along(x)) {
##   output <- c(output, lengths(x[[i]]))
## }
## output


## ------------------------------------------------------------------------
df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

df$a <- rescale01(df$a)
df$b <- rescale01(df$b)
df$c <- rescale01(df$c)
df$d <- rescale01(df$d)


## ------------------------------------------------------------------------
for (i in seq_along(df)) {
  df[[i]] <- rescale01(df[[i]])
}


## ---- eval = FALSE-------------------------------------------------------
## results <- vector("list", length(x))
## names(results) <- names(x)


## ---- eval = FALSE-------------------------------------------------------
## for (i in seq_along(x)) {
##   name <- names(x)[[i]]
##   value <- x[[i]]
## }


## ------------------------------------------------------------------------
means <- c(0, 1, 2)

output <- double()
for (i in seq_along(means)) {
  n <- sample(100, 1)
  output <- c(output, rnorm(n, means[[i]]))
}
str(output)


## ------------------------------------------------------------------------
out <- vector("list", length(means))
for (i in seq_along(means)) {
  n <- sample(100, 1)
  out[[i]] <- rnorm(n, means[[i]])
}
str(out)
str(unlist(out))


## ---- eval = FALSE-------------------------------------------------------
## while (condition) {
##   # body
## }


## ---- eval = FALSE-------------------------------------------------------
## for (i in seq_along(x)) {
##   # body
## }
## 
## # Equivalent to
## i <- 1
## while (i <= length(x)) {
##   # body
##   i <- i + 1
## }


## ------------------------------------------------------------------------
flip <- function() sample(c("T", "H"), 1)

flips <- 0
nheads <- 0

while (nheads < 3) {
  if (flip() == "H") {
    nheads <- nheads + 1
  } else {
    nheads <- 0
  }
  flips <- flips + 1
}
flips


## ---- eval = FALSE-------------------------------------------------------
## show_mean(iris)
## #> Sepal.Length: 5.84
## #> Sepal.Width:  3.06
## #> Petal.Length: 3.76
## #> Petal.Width:  1.20


## ---- eval = FALSE-------------------------------------------------------
## trans <- list(
##   disp = function(x) x * 0.0163871,
##   am = function(x) {
##     factor(x, labels = c("auto", "manual"))
##   }
## )
## for (var in names(trans)) {
##   mtcars[[var]] <- trans[[var]](mtcars[[var]])
## }


## ------------------------------------------------------------------------
df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)


## ------------------------------------------------------------------------
output <- vector("double", length(df))
for (i in seq_along(df)) {
  output[[i]] <- mean(df[[i]])
}
output


## ------------------------------------------------------------------------
col_mean <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[i] <- mean(df[[i]])
  }
  output
}


## ------------------------------------------------------------------------
col_median <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[i] <- median(df[[i]])
  }
  output
}
col_sd <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[i] <- sd(df[[i]])
  }
  output
}


## ------------------------------------------------------------------------
f1 <- function(x) abs(x - mean(x)) ^ 1
f2 <- function(x) abs(x - mean(x)) ^ 2
f3 <- function(x) abs(x - mean(x)) ^ 3


## ------------------------------------------------------------------------
f <- function(x, i) abs(x - mean(x)) ^ i


## ------------------------------------------------------------------------
col_summary <- function(df, fun) {
  out <- vector("double", length(df))
  for (i in seq_along(df)) {
    out[i] <- fun(df[[i]])
  }
  out
}
col_summary(df, median)
col_summary(df, mean)


## ------------------------------------------------------------------------
map_dbl(df, mean)
map_dbl(df, median)
map_dbl(df, sd)


## ------------------------------------------------------------------------
df %>% map_dbl(mean)
df %>% map_dbl(median)
df %>% map_dbl(sd)


## ------------------------------------------------------------------------
map_dbl(df, mean, trim = 0.5)


## ------------------------------------------------------------------------
z <- list(x = 1:3, y = 4:5)
map_int(z, length)


## ------------------------------------------------------------------------
models <- mtcars %>% 
  split(.$cyl) %>% 
  map(function(df) lm(mpg ~ wt, data = df))


## ------------------------------------------------------------------------
models <- mtcars %>% 
  split(.$cyl) %>% 
  map(~lm(mpg ~ wt, data = .))


## ------------------------------------------------------------------------
models %>% 
  map(summary) %>% 
  map_dbl(~.$r.squared)


## ------------------------------------------------------------------------
models %>% 
  map(summary) %>% 
  map_dbl("r.squared")


## ------------------------------------------------------------------------
x <- list(list(1, 2, 3), list(4, 5, 6), list(7, 8, 9))
x %>% map_dbl(2)


## ------------------------------------------------------------------------
x1 <- list(
  c(0.27, 0.37, 0.57, 0.91, 0.20),
  c(0.90, 0.94, 0.66, 0.63, 0.06), 
  c(0.21, 0.18, 0.69, 0.38, 0.77)
)
x2 <- list(
  c(0.50, 0.72, 0.99, 0.38, 0.78), 
  c(0.93, 0.21, 0.65, 0.13, 0.27), 
  c(0.39, 0.01, 0.38, 0.87, 0.34)
)

threshold <- function(x, cutoff = 0.8) x[x > cutoff]
x1 %>% sapply(threshold) %>% str()
x2 %>% sapply(threshold) %>% str()


## ------------------------------------------------------------------------
safe_log <- safely(log)
str(safe_log(10))
str(safe_log("a"))


## ------------------------------------------------------------------------
x <- list(1, 10, "a")
y <- x %>% map(safely(log))
str(y)


## ------------------------------------------------------------------------
y <- y %>% transpose()
str(y)


## ------------------------------------------------------------------------
is_ok <- y$error %>% map_lgl(is_null)
x[!is_ok]
y$result[is_ok] %>% flatten_dbl()


## ------------------------------------------------------------------------
x <- list(1, 10, "a")
x %>% map_dbl(possibly(log, NA_real_))


## ------------------------------------------------------------------------
x <- list(1, -1)
x %>% map(quietly(log)) %>% str()


## ------------------------------------------------------------------------
mu <- list(5, 10, -3)
mu %>% 
  map(rnorm, n = 5) %>% 
  str()


## ------------------------------------------------------------------------
sigma <- list(1, 5, 10)
seq_along(mu) %>% 
  map(~rnorm(5, mu[[.]], sigma[[.]])) %>% 
  str()


## ------------------------------------------------------------------------
map2(mu, sigma, rnorm, n = 5) %>% str()


## ---- echo = FALSE-------------------------------------------------------
knitr::include_graphics("diagrams/lists-map2.png")


## ------------------------------------------------------------------------
map2 <- function(x, y, f, ...) {
  out <- vector("list", length(x))
  for (i in seq_along(x)) {
    out[[i]] <- f(x[[i]], y[[i]], ...)
  }
  out
}


## ------------------------------------------------------------------------
n <- list(1, 3, 5)
args1 <- list(n, mu, sigma)
args1 %>%
  pmap(rnorm) %>% 
  str()


## ---- echo = FALSE-------------------------------------------------------
knitr::include_graphics("diagrams/lists-pmap-unnamed.png")


## ---- eval = FALSE-------------------------------------------------------
## args2 <- list(mean = mu, sd = sigma, n = n)
## args2 %>%
##   pmap(rnorm) %>%
##   str()


## ---- echo = FALSE-------------------------------------------------------
knitr::include_graphics("diagrams/lists-pmap-named.png")


## ------------------------------------------------------------------------
params <- tribble(
  ~mean, ~sd, ~n,
    5,     1,  1,
   10,     5,  3,
   -3,    10,  5
)
params %>% 
  pmap(rnorm)


## ------------------------------------------------------------------------
f <- c("runif", "rnorm", "rpois")
param <- list(
  list(min = -1, max = 1), 
  list(sd = 5), 
  list(lambda = 10)
)


## ------------------------------------------------------------------------
invoke_map(f, param, n = 5) %>% str()


## ---- echo = FALSE, out.width = NULL-------------------------------------
knitr::include_graphics("diagrams/lists-invoke.png")


## ---- eval = FALSE-------------------------------------------------------
## sim <- tribble(
##   ~f,      ~params,
##   "runif", list(min = -1, max = 1),
##   "rnorm", list(sd = 5),
##   "rpois", list(lambda = 10)
## )
## sim %>%
##   mutate(sim = invoke_map(f, params, n = 10))


## ------------------------------------------------------------------------
x <- list(1, "a", 3)

x %>% 
  walk(print)


## ---- eval = FALSE-------------------------------------------------------
## library(ggplot2)
## plots <- mtcars %>%
##   split(.$cyl) %>%
##   map(~ggplot(., aes(mpg, wt)) + geom_point())
## paths <- stringr::str_c(names(plots), ".pdf")
## 
## pwalk(list(paths, plots), ggsave, path = tempdir())


## ------------------------------------------------------------------------
iris %>% 
  keep(is.factor) %>% 
  str()

iris %>% 
  discard(is.factor) %>% 
  str()


## ------------------------------------------------------------------------
x <- list(1:5, letters, list(10))

x %>% 
  some(is_character)

x %>% 
  every(is_vector)


## ------------------------------------------------------------------------
x <- sample(10)
x

x %>% 
  detect(~ . > 5)

x %>% 
  detect_index(~ . > 5)


## ------------------------------------------------------------------------
x %>% 
  head_while(~ . > 5)

x %>% 
  tail_while(~ . > 5)


## ------------------------------------------------------------------------
dfs <- list(
  age = tibble(name = "John", age = 30),
  sex = tibble(name = c("John", "Mary"), sex = c("M", "F")),
  trt = tibble(name = "Mary", treatment = "A")
)

dfs %>% reduce(full_join)


## ------------------------------------------------------------------------
vs <- list(
  c(1, 3, 5, 6, 10),
  c(1, 2, 3, 7, 8, 10),
  c(1, 2, 3, 4, 8, 9, 10)
)

vs %>% reduce(intersect)


## ------------------------------------------------------------------------
x <- sample(10)
x
x %>% accumulate(`+`)


## ------------------------------------------------------------------------
col_sum3 <- function(df, f) {
  is_num <- sapply(df, is.numeric)
  df_num <- df[, is_num]

  sapply(df_num, f)
}


## ---- eval = FALSE-------------------------------------------------------
## df <- tibble(
##   x = 1:3,
##   y = 3:1,
##   z = c("a", "b", "c")
## )
## # OK
## col_sum3(df, mean)
## # Has problems: don't always return numeric vector
## col_sum3(df[1:2], mean)
## col_sum3(df[1], mean)
## col_sum3(df[0], mean)

