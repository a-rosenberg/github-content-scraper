## ----setup---------------------------------------------------------------
library(purrr)


## ------------------------------------------------------------------------
# From https://api.github.com/repos/hadley/r4ds/issues
issues <- jsonlite::fromJSON("issues.json", simplifyVector = FALSE)


## ------------------------------------------------------------------------
length(issues)
str(issues[[1]])


## ------------------------------------------------------------------------
tibble::tibble(
  i = seq_along(issues),
  names = issues %>% map(names) 
) %>% 
  tidyr::unnest(names) %>% 
  table() %>% 
  t()


## ------------------------------------------------------------------------

issues %>% map_int("id")
issues %>% map_lgl("locked")
issues %>% map_chr("state")


## ------------------------------------------------------------------------
users <- issues %>% map("user")
users %>% map_chr("login")
users %>% map_int("id")


## ------------------------------------------------------------------------
issues %>% map_chr(c("user", "login"))
issues %>% map_int(c("user", "id"))


## ---- error = TRUE-------------------------------------------------------
issues %>% map_chr(c("pull_request", "html_url"))


## ------------------------------------------------------------------------
issues %>% map(c("pull_request", "html_url"))


## ------------------------------------------------------------------------
issues %>% map_chr(c("pull_request", "html_url"), .null = NA)


## ------------------------------------------------------------------------
issues %>% map_chr(list("pull_request", 1), .null = NA)


## ------------------------------------------------------------------------
x <- list(list(a = 1, b = 2), list(c = 3, d = 4))
str(x)

y <- flatten(x) 
str(y)
flatten_dbl(y)


## ---- echo = FALSE-------------------------------------------------------
knitr::include_graphics("diagrams/lists-flatten.png")


## ------------------------------------------------------------------------
x <- list(
  x = list(a = 1, b = 3, c = 5),
  y = list(a = 2, b = 4, c = 6)
)
x %>% str()
x %>% transpose() %>% str()


## ---- echo = FALSE-------------------------------------------------------
knitr::include_graphics("diagrams/lists-transpose.png")


## ------------------------------------------------------------------------
df <- tibble::tibble(x = 1:3, y = c("a", "b", "c"))
df %>% transpose() %>% str()


## ---- eval = FALSE-------------------------------------------------------
## files <- dir("data", pattern = "\\.csv$")
## files %>%
##   set_names(., basename(.)) %>%
##   map_df(safely(readr::read_csv), .id = "filename") %>%

