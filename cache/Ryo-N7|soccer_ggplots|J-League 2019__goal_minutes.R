## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---- message=FALSE, warning=FALSE---------------------------------------
pacman::p_load(tidyverse, polite, scales, ggimage, ggforce,
               rvest, glue, extrafont, ggrepel, magick, ggtext)
loadfonts()


## ------------------------------------------------------------------------
url <- "https://data.j-league.or.jp/SFMS02/?match_card_id=21513"

session <- bow(url)

url %>% 
  read_html() %>% 
  html_nodes(".score-board-pk-b > table:nth-child(1)") %>% 
  html_table(fill = TRUE) %>% 
  purrr::flatten_df() %>% 
  View()


url %>% 
  read_html() %>% 
  html_nodes("td.right-area > table:nth-child(1)") %>% #html_text() %>% View()
  html_table(fill = TRUE) %>% 
  purrr::flatten_df() %>% 
  View()

thingy22 <- url %>% 
  read_html() %>% 
  html_nodes("td.left-area > table:nth-child(1)") 


thingy <- url %>% 
  read_html() %>% 
  html_nodes("td.right-area > table:nth-child(1)")   

if (length(xml_contents(thingy)) == 0) {
  thingy <- tibble(name = "",
                   minute = "")
}

if (length(xml_contents(thingy22)) == 0) {
  thingy22 <- tibble(name = "",
                   minute = "")
}

thingy22 %>% 
  html_table(fill = TRUE) %>% 
  purrr::flatten_df() %>% # %>% View()
  rename(name = X1, minute = X2)



".score-board-pk-b > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(1)"
"td.right-area > table:nth-child(1)"
"td.right-area"


## ------------------------------------------------------------------------
url %>% 
  read_html() %>% 
  html_nodes(".score-board-b > table:nth-child(1)") %>% 
  html_text() %>% View()
  .[[1]] %>% 
  html_table(fill = TRUE)

