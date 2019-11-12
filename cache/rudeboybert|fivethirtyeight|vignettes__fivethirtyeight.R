## ---- message=FALSE, warning=FALSE, echo=FALSE---------------------------
library(fivethirtyeight)
library(ggplot2)
library(dplyr)
library(readr)
library(knitr)

# Import master Google Sheet of all 538 data available here:
# https://docs.google.com/spreadsheets/d/1IMWAHNPIDzplafWW6AGnGyHmB1BMjohEw_V5HmT70Gs/edit#gid=840984416
datasets <- 
  "https://goo.gl/OT8iHa" %>% 
  read_csv() %>% 
  filter(!is.na(DATAFRAME_NAME)) %>% 
  transmute(
    `Data Frame Name` = DATAFRAME_NAME,
    `Article Title` = ARTICLE_TITLE,
    `Filed Under` = ARTICLE_FILED_UNDER,
    `Author 1` = ARTICLE_AUTHOR_1,
    `Author 2` = ARTICLE_AUTHOR_2,
    `Author 3` = ARTICLE_AUTHOR_3,
    Preview = `ONLY PREVIEW OF FIRST 10 ROWS INCLUDED IN PACKAGE?`,
    Date = ARTICLE_DATE,
    URL = ARTICLE_URL
  ) %>% 
  mutate(
    `Filed Under` = ifelse(is.na(`Filed Under`), "", `Filed Under`),
    `Author 1` = ifelse(is.na(`Author 1`), "", `Author 1`),
    `Author 2` = ifelse(is.na(`Author 2`), "", `Author 2`),
    `Author 3` = ifelse(is.na(`Author 3`), "", `Author 3`)
  ) %>% 
  arrange(`Data Frame Name`)

preview_only_datasets <- datasets %>% 
  filter(Preview == "Y") %>% 
  pull(`Data Frame Name`)


## ---- message=FALSE, warning=FALSE, echo=FALSE---------------------------
preview_only_datasets


## ---- message=FALSE, warning=FALSE, echo=FALSE---------------------------
datasets %>% 
  mutate(`Data Frame Name` = paste("`", `Data Frame Name`, "`", sep="")) %>% 
  select(-Preview) %>% 
  kable()

