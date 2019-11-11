library(tidyverse)

tgiv_raw <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-11-20/thanksgiving_meals.csv")

#DataExplorer::create_report(tgiv_raw)

#let's see if there's any correlation between number of sides and number of desserts

tgiv_processed <- tgiv_raw %>% 
  filter(celebrate == "Yes") %>%
  #Select only side and desserts, drop "Other" columns
  select(id, side1:side13, side15, dessert1:dessert10, dessert12) %>%
  gather(side1:side15, key = "side_number", value = "side_name") %>% 
  gather(dessert1:dessert12, key = "dessert_number", value = "dessert_name") 

respond_id <- tgiv_raw %>% select(id) 

dessert_count <- 
  tgiv_processed %>% 
  select(id, dessert_name)%>% 
  filter(!is.na(dessert_name)) %>%
  group_by(id) %>%
  filter(!duplicated(dessert_name)) %>%
  summarise(dessert_num = n())

side_count <- 
  tgiv_processed %>% 
  select(id, side_name)%>% 
  filter(!is.na(side_name)) %>% 
  group_by(id) %>% 
  filter(!duplicated(side_name)) %>% 
  summarise(side_num = n()) 
  

respond_id <- respond_id %>% 
  left_join(dessert_count, by = "id") %>% 
  left_join(side_count, by = "id")

respond_id[is.na(respond_id)] <- 0

respond_id$id <- as.character(respond_id$id)

respond_id$dessert_num <- as.factor(respond_id$dessert_num)
respond_id$side_num <- as.factor(respond_id$side_num)

ggplot(respond_id, aes(side_num, dessert_num)) + 
  geom_point(colour = "orange", alpha = 0.7) + 
  geom_jitter(colour = "orange", alpha = 0.7) +
  labs(x = "# Sides", 
       y = "# Desserts",
       title = "Does a balanced Thanksgiving meal",
        subtitle = "mean more sides than desserts?",
        caption="*0's could be N/A or 0 \n Plot by @oscar_b123 \n Data: fivethirtyeight ") +
    theme_minimal() 

