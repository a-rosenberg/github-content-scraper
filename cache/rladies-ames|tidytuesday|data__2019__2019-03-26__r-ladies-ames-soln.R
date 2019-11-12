## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align="center")


## ----getdat--------------------------------------------------------------
library(tidyverse)
pets <- read_csv("seattle_pets.csv")
head(pets)


## ----dates---------------------------------------------------------------
library(lubridate)
pets %>% 
  mutate(date = parse_date(license_issue_date, format = "%B %d %Y"), 
         zip = parse_integer(zip_code)) -> pets
# check for missings 
pets %>% filter(is.na(date))
pets %>% filter(is.na(zip))
pets %>% filter(is.na(zip), !is.na(zip_code))

# if zip_code is not NA, only take the first 5 digits 
pets <- pets %>% 
  mutate(zip = ifelse((!is.na(zip_code) & is.na(zip)), parse_integer(str_sub(zip_code, 1, 5)), zip))
head(pets)


## ----lets----------------------------------------------------------------
pets %>% 
  mutate(first_letter = toupper(str_sub(animals_name, 1,1))) -> pets 
pets %>% 
  ggplot() + 
  geom_bar(aes(x = first_letter, fill = species)) 


## ----weird---------------------------------------------------------------
pets %>% filter(!(first_letter %in% LETTERS) , !(is.na(animals_name)))
# only 12 that are non-alpha


## ----lets2---------------------------------------------------------------
pets %>% filter(first_letter %in% LETTERS, species %in% c("Cat", "Dog")) %>% 
  ggplot() + 
  geom_bar(aes(x = first_letter, fill = species), position = "dodge")
pets %>% filter(first_letter %in% LETTERS, species %in% c("Goat", "Pig")) %>% 
  ggplot() + 
  geom_bar(aes(x = first_letter, fill = species), position = "dodge")


## ----goats---------------------------------------------------------------

filter(pets, species == "Goat") %>% 
  select(animals_name) %>% count(animals_name) %>% arrange(desc(n))

filter(pets, species == "Pig") %>% 
  select(animals_name) %>% count(animals_name) %>% arrange(desc(n))



## ----chisq---------------------------------------------------------------
pets %>% filter(species %in% c("Cat", "Dog"), first_letter %in% LETTERS) %>% 
  group_by(species, first_letter) %>% count() -> test

# ?chisq.test

cats <- (test %>% filter(species == "Cat"))$n
dogs <- (test %>% filter(species == "Dog"))$n

chisq.test(cats, dogs, correct = FALSE)

