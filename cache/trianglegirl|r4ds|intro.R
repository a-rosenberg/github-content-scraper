## ----echo = FALSE, out.width = "75%"-------------------------------------
knitr::include_graphics("diagrams/data-science.png")


## ----echo = FALSE, out.width = "75%"-------------------------------------
knitr::include_graphics("diagrams/rstudio-console.png")


## ---- eval = FALSE-------------------------------------------------------
## install.packages("tidyverse")


## ------------------------------------------------------------------------
library(tidyverse)


## ---- eval = FALSE-------------------------------------------------------
## install.packages(c("nycflights13", "gapminder", "Lahman"))


## ---- eval = TRUE--------------------------------------------------------
1 + 2
#> [1] 3


## ---- results = "asis", echo = FALSE, message = FALSE--------------------
library(dplyr)
# git --no-pager shortlog -ns > contribs.txt
contribs <- readr::read_tsv("contribs.txt", col_names = c("n", "name"))

contribs <- contribs %>% 
  filter(!name %in% c("hadley", "Garrett", "Hadley Wickham",
                      "Garrett Grolemund")) %>% 
  arrange(name) %>% 
  mutate(uname = ifelse(!grepl(" ", name), paste0("@", name), name))

cat("Thanks go to all contributers in alphabetical order: ")
cat(paste0(contribs$uname, collapse = ", "))
cat(".\n")


## ------------------------------------------------------------------------
devtools::session_info(c("tidyverse"))

