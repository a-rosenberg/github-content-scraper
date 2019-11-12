## ---- results = "asis", echo = FALSE, message = FALSE--------------------
library(dplyr)
contributors <- readr::read_csv("contributors.csv", col_types = list())
contributors <- contributors %>% 
  mutate(
    link = glue::glue("[\\@{login}](https://github.com/{login})"),
    desc = ifelse(is.na(name), link, glue::glue("{name} ({link})"))
  )

cat("A big thanks goes to everyone who has contributed!\n")
cat(paste0(contributors$desc, collapse = ", "))

