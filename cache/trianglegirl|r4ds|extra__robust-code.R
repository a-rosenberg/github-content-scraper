## ---- include = FALSE----------------------------------------------------
library(magrittr)


## ---- eval = FALSE-------------------------------------------------------
## baz <- foo(bar, qux)


## ---- eval = FALSE-------------------------------------------------------
## df2 <- arrange(df, qux)


## ------------------------------------------------------------------------
df <- data.frame(xy = c("x", "y"))
# Character vectors were hard to work with for a long time, so R
# helpfully converts to a factor for you:
class(df$xy)

# If you're only selecting a single column, R tries to be helpful
# and give you that column, rather than giving you a single column
# data frame
class(df[, "xy"])

# If you have long variable names, R is "helpful" and lets you select
# them with a unique prefix
df$x


## ---- error = TRUE-------------------------------------------------------
df <- tibble::tibble(xy = c("x", "y"))
class(df$xy)
class(df[, "xy"])
df$x


## ------------------------------------------------------------------------
last_row <- function(df) {
  df[nrow(df), ]
}


## ------------------------------------------------------------------------
df <- data.frame(x = 1:3)
last_row(df)


## ------------------------------------------------------------------------
last_row <- function(df) {
  df[nrow(df), , drop = FALSE]
}
last_row(df)


## ------------------------------------------------------------------------
df <- data.frame(
  a = 1L,
  b = 1.5,
  y = Sys.time(),
  z = ordered(1)
)


df[1:4] %>% sapply(class) %>% str()
df[1:2] %>% sapply(class) %>% str()
df[3:4] %>% sapply(class) %>% str()


## ---- eval = FALSE-------------------------------------------------------
## ggplot(mpg, aes(displ, cty)) + geom_point()
## filter(mpg, displ > 10)


## ------------------------------------------------------------------------
big_x <- function(df, threshold) {
  dplyr::filter(df, x > threshold)
}


## ---- error = TRUE-------------------------------------------------------
big_x(mtcars, 10)

x <- 1
big_x(mtcars, 10)


## ------------------------------------------------------------------------
df <- tibble::tibble(x = 1:10, threshold = 100)
big_x(df, 5)


## ------------------------------------------------------------------------
big_x <- function(df, threshold) {
  if (!"x" %in% names(df)) 
    stop("`df` must contain variable called `x`.", call. = FALSE)
  
  if ("threshold" %in% names(df))
    stop("`df` must not contain variable called `threshold`.", call. = FALSE)
  
  dplyr::filter(df, x > threshold)
}


## ------------------------------------------------------------------------
big_x <- function(df, threshold) {
  dplyr::filter(df, local(x) > parent(threshold))
}


## ------------------------------------------------------------------------
big_x <- function(df, threshold) {
  rows <- df$x > threshold
  df[!is.na(rows) & rows, , drop = FALSE]
}

