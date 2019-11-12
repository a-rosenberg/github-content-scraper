## ------------------------------------------------------------------------
df <- tibble::tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

df$a <- (df$a - min(df$a, na.rm = TRUE)) / 
  (max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df$b <- (df$b - min(df$b, na.rm = TRUE)) / 
  (max(df$b, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df$c <- (df$c - min(df$c, na.rm = TRUE)) / 
  (max(df$c, na.rm = TRUE) - min(df$c, na.rm = TRUE))
df$d <- (df$d - min(df$d, na.rm = TRUE)) / 
  (max(df$d, na.rm = TRUE) - min(df$d, na.rm = TRUE))


## ---- eval = FALSE-------------------------------------------------------
## (df$a - min(df$a, na.rm = TRUE)) /
##   (max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))


## ------------------------------------------------------------------------
x <- df$a
(x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))


## ------------------------------------------------------------------------
rng <- range(x, na.rm = TRUE)
(x - rng[1]) / (rng[2] - rng[1])


## ------------------------------------------------------------------------
rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale01(c(0, 5, 10))


## ------------------------------------------------------------------------
rescale01(c(-10, 0, 10))
rescale01(c(1, 2, 3, NA, 5))


## ------------------------------------------------------------------------
df$a <- rescale01(df$a)
df$b <- rescale01(df$b)
df$c <- rescale01(df$c)
df$d <- rescale01(df$d)


## ------------------------------------------------------------------------
x <- c(1:10, Inf)
rescale01(x)


## ------------------------------------------------------------------------
rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale01(x)


## ---- eval = FALSE-------------------------------------------------------
## mean(is.na(x))
## 
## x / sum(x, na.rm = TRUE)
## 
## sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)


## ------------------------------------------------------------------------
is_directory <- function(x) file.info(x)$isdir
is_readable <- function(x) file.access(x, 4) == 0


## ---- eval = FALSE-------------------------------------------------------
## # Too short
## f()
## 
## # Not a verb, or descriptive
## my_awesome_function()
## 
## # Long, but clear
## impute_missing()
## collapse_years()


## ---- eval = FALSE-------------------------------------------------------
## # Never do this!
## col_mins <- function(x, y) {}
## rowMaxes <- function(y, x) {}


## ---- eval = FALSE-------------------------------------------------------
## # Good
## input_select()
## input_checkbox()
## input_text()
## 
## # Not so good
## select_input()
## checkbox_input()
## text_input()


## ---- eval = FALSE-------------------------------------------------------
## # Don't do this!
## T <- FALSE
## c <- 10
## mean <- function(x) sum(x)


## ---- eval = FALSE-------------------------------------------------------
## # Load data --------------------------------------
## 
## # Plot data --------------------------------------


## ---- echo = FALSE, out.width = NULL-------------------------------------
knitr::include_graphics("screenshots/rstudio-nav.png")


## ------------------------------------------------------------------------
f1 <- function(string, prefix) {
  substr(string, 1, nchar(prefix)) == prefix
}
f2 <- function(x) {
  if (length(x) <= 1) return(NULL)
  x[-length(x)]
}
f3 <- function(x, y) {
  rep(y, length.out = length(x))
}


## ---- eval = FALSE-------------------------------------------------------
## if (condition) {
##   # code executed when condition is TRUE
## } else {
##   # code executed when condition is FALSE
## }


## ------------------------------------------------------------------------
has_name <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    rep(FALSE, length(x))
  } else {
    !is.na(nms) & nms != ""
  }
}


## ---- error = TRUE-------------------------------------------------------
if (c(TRUE, FALSE)) {}

if (NA) {}


## ------------------------------------------------------------------------
identical(0L, 0)


## ------------------------------------------------------------------------
x <- sqrt(2) ^ 2
x
x == 2
x - 2


## ---- eval = FALSE-------------------------------------------------------
## if (this) {
##   # do that
## } else if (that) {
##   # do something else
## } else {
##   #
## }


## ---- echo = FALSE-------------------------------------------------------
function(x, y, op) {
  switch(op,
    plus = x + y,
    minus = x - y,
    times = x * y,
    divide = x / y,
    stop("Unknown op!")
  )
}


## ---- eval = FALSE-------------------------------------------------------
## # Good
## if (y < 0 && debug) {
##   message("Y is negative")
## }
## 
## if (y == 0) {
##   log(x)
## } else {
##   y ^ x
## }
## 
## # Bad
## if (y < 0 && debug)
## message("Y is negative")
## 
## if (y == 0) {
##   log(x)
## }
## else {
##   y ^ x
## }


## ------------------------------------------------------------------------
y <- 10
x <- if (y < 20) "Too low" else "Too high"


## ------------------------------------------------------------------------
if (y < 20) {
  x <- "Too low" 
} else {
  x <- "Too high"
}


## ---- eval = FALSE-------------------------------------------------------
## if (temp <= 0) {
##   "freezing"
## } else if (temp <= 10) {
##   "cold"
## } else if (temp <= 20) {
##   "cool"
## } else if (temp <= 30) {
##   "warm"
## } else {
##   "hot"
## }


## ---- eval = FALSE-------------------------------------------------------
## switch(x,
##   a = ,
##   b = "ab",
##   c = ,
##   d = "cd"
## )


## ------------------------------------------------------------------------
# Compute confidence interval around mean using normal approximation
mean_ci <- function(x, conf = 0.95) {
  se <- sd(x) / sqrt(length(x))
  alpha <- 1 - conf
  mean(x) + se * qnorm(c(alpha / 2, 1 - alpha / 2))
}

x <- runif(100)
mean_ci(x)
mean_ci(x, conf = 0.99)


## ---- eval = FALSE-------------------------------------------------------
## # Good
## mean(1:10, na.rm = TRUE)
## 
## # Bad
## mean(x = 1:10, , FALSE)
## mean(, TRUE, x = c(1:10, NA))


## ---- eval = FALSE-------------------------------------------------------
## # Good
## average <- mean(feet / 12 + inches, na.rm = TRUE)
## 
## # Bad
## average<-mean(feet/12+inches,na.rm=TRUE)


## ------------------------------------------------------------------------
wt_mean <- function(x, w) {
  sum(x * w) / sum(w)
}
wt_var <- function(x, w) {
  mu <- wt_mean(x, w)
  sum(w * (x - mu) ^ 2) / sum(w)
}
wt_sd <- function(x, w) {
  sqrt(wt_var(x, w))
}


## ------------------------------------------------------------------------
wt_mean(1:6, 1:3)


## ------------------------------------------------------------------------
wt_mean <- function(x, w) {
  if (length(x) != length(w)) {
    stop("`x` and `w` must be the same length", call. = FALSE)
  }
  sum(w * x) / sum(w)
}


## ------------------------------------------------------------------------
wt_mean <- function(x, w, na.rm = FALSE) {
  if (!is.logical(na.rm)) {
    stop("`na.rm` must be logical")
  }
  if (length(na.rm) != 1) {
    stop("`na.rm` must be length 1")
  }
  if (length(x) != length(w)) {
    stop("`x` and `w` must be the same length", call. = FALSE)
  }
  
  if (na.rm) {
    miss <- is.na(x) | is.na(w)
    x <- x[!miss]
    w <- w[!miss]
  }
  sum(w * x) / sum(w)
}


## ---- error = TRUE-------------------------------------------------------
wt_mean <- function(x, w, na.rm = FALSE) {
  stopifnot(is.logical(na.rm), length(na.rm) == 1)
  stopifnot(length(x) == length(w))
  
  if (na.rm) {
    miss <- is.na(x) | is.na(w)
    x <- x[!miss]
    w <- w[!miss]
  }
  sum(w * x) / sum(w)
}
wt_mean(1:6, 6:1, na.rm = "foo")


## ------------------------------------------------------------------------
sum(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
stringr::str_c("a", "b", "c", "d", "e", "f")


## ------------------------------------------------------------------------
commas <- function(...) stringr::str_c(..., collapse = ", ")
commas(letters[1:10])

rule <- function(..., pad = "-") {
  title <- paste0(...)
  width <- getOption("width") - nchar(title) - 5
  cat(title, " ", stringr::str_dup(pad, width), "\n", sep = "")
}
rule("Important output")


## ------------------------------------------------------------------------
x <- c(1, 2)
sum(x, na.mr = TRUE)


## ------------------------------------------------------------------------
complicated_function <- function(x, y, z) {
  if (length(x) == 0 || length(y) == 0) {
    return(0)
  }
    
  # Complicated code here
}



## ---- eval = FALSE-------------------------------------------------------
## f <- function() {
##   if (x) {
##     # Do
##     # something
##     # that
##     # takes
##     # many
##     # lines
##     # to
##     # express
##   } else {
##     # return something short
##   }
## }


## ---- eval = FALSE-------------------------------------------------------
## 
## f <- function() {
##   if (!x) {
##     return(something_short)
##   }
## 
##   # Do
##   # something
##   # that
##   # takes
##   # many
##   # lines
##   # to
##   # express
## }


## ------------------------------------------------------------------------
show_missings <- function(df) {
  n <- sum(is.na(df))
  cat("Missing values: ", n, "\n", sep = "")
  
  invisible(df)
}


## ------------------------------------------------------------------------
show_missings(mtcars)


## ------------------------------------------------------------------------
x <- show_missings(mtcars) 
class(x)
dim(x)


## ---- include = FALSE----------------------------------------------------
library(dplyr)

## ------------------------------------------------------------------------
mtcars %>% 
  show_missings() %>% 
  mutate(mpg = ifelse(mpg < 20, NA, mpg)) %>% 
  show_missings() 


## ------------------------------------------------------------------------
f <- function(x) {
  x + y
} 


## ------------------------------------------------------------------------
y <- 100
f(10)

y <- 1000
f(10)


## ------------------------------------------------------------------------
`+` <- function(x, y) {
  if (runif(1) < 0.1) {
    sum(x, y)
  } else {
    sum(x, y) * 1.1
  }
}
table(replicate(1000, 1 + 2))
rm(`+`)

