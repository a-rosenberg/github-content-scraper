## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(ggthemes)
library(kableExtra)

seattle_pets <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-26/seattle_pets.csv")

seattle_pets$license_issue_date <- as.Date(seattle_pets$license_issue_date,
                                           "%B %d %Y")

seattle_pets$animals_name <- str_replace(seattle_pets$animals_name, " \\(.*\\)", "")
seattle_pets$animals_name <- str_replace(seattle_pets$animals_name, " \\\".*\\\"", "")
seattle_pets$animals_name <- str_replace(seattle_pets$animals_name, " \\\'.*\\\'", "")

seattle_pets$name_length <- nchar(seattle_pets$animals_name)

pets_2018 <- seattle_pets %>% filter(year(license_issue_date) == 2018)


## ------------------------------------------------------------------------
top_names <- pets_2018 %>% drop_na(animals_name) %>%
  count(animals_name, sort = TRUE) %>%
  head(20)


## ------------------------------------------------------------------------
top_names %>% 
  mutate(animals_name = fct_reorder(animals_name, n)) %>%
  ggplot() +
  geom_col(aes(x = animals_name, y = n)) +
  coord_flip() +
  labs(title = "Most popular pet names in Seattle in 2018",
       x = "", y = "") +
  theme_light()


## ------------------------------------------------------------------------
dogs_2018 <- pets_2018 %>% drop_na(animals_name) %>%
  filter(species == "Dog")

top_dogs <- dogs_2018 %>%
  count(animals_name, sort = TRUE)


## ------------------------------------------------------------------------
cats_2018 <- pets_2018 %>% drop_na(animals_name) %>%
  filter(species == "Cat")

top_cats <- cats_2018 %>%
  count(animals_name, sort = TRUE)


## ------------------------------------------------------------------------
top_dogs %>% 
  mutate(animals_name = fct_reorder(animals_name, n)) %>%
  head(20) %>%
  ggplot() +
  geom_col(aes(x = animals_name, y = n)) +
  coord_flip() +
  labs(title = "Most popular dog names in Seattle in 2018",
       x = "", y = "") +
  theme_light()


## ------------------------------------------------------------------------
dogs_y_ie <- dogs_2018[(str_ends(dogs_2018$animals_name, "y") |
          str_ends(dogs_2018$animals_name, "ie")), ]


## ------------------------------------------------------------------------
cats_y_ie <- cats_2018[(str_ends(cats_2018$animals_name, "y") |
          str_ends(cats_2018$animals_name, "ie")), ]


## ------------------------------------------------------------------------
dogs_y_ie %>% count(animals_name, sort = TRUE) %>% View()
cats_y_ie %>% count(animals_name, sort = TRUE) %>% View()


## ------------------------------------------------------------------------
pets_2018 %>% filter(animals_name == "Fenway" | 
                       animals_name == "Wrigley") %>%
  ggplot(aes(animals_name)) +
  geom_bar(aes(fill = animals_name)) +
  scale_fill_manual(values = c("#BD3039","#0E3386"), guide = FALSE) +
  coord_flip() +
  labs(x = "", y = "",
       title = "Pets in Seattle named after ballparks",
       caption = "@trevin_flick") +
  theme_fivethirtyeight()
  
ggsave("mlb-pets.png")


## ------------------------------------------------------------------------
dogs_2018 %>% arrange(desc(name_length)) %>%
  head(10) %>%
  kable() %>% 
  kable_styling()


## ------------------------------------------------------------------------
cats_2018 %>% arrange(desc(name_length)) %>%
  head(10) %>%
  kable() %>% 
  kable_styling()


## ------------------------------------------------------------------------
top_cats <- pets_2018 %>% filter(species == "Cat") %>% 
  drop_na(animals_name)


## ------------------------------------------------------------------------
pets_2018 %>% 
  group_by(license_number) %>% 
  filter(n() > 1) %>%
  arrange(license_number) %>%
  View()


## ------------------------------------------------------------------------
pets_2018 %>% drop_na(species) %>%
  count(species, sort = TRUE)

