## ------------------------------------------------------------------------
# load packages and parse data
library(tidyverse)
library(scales)
library(RColorBrewer)
library(forcats)
library(lubridate)
library(ebbr)

nyc_restaurants_raw <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-12-11/nyc_restaurants.csv")

nyc_restaurants <- nyc_restaurants_raw %>%
  filter(inspection_date != '01/01/1900') #%>% # filter out establishments which have not been inspected yet
  #filter(grepl("A|B|C", grade)) %>% # filter those without grades
  #filter(!grepl("Missing", boro))


## ------------------------------------------------------------------------
nyc_restaurants %>%
  select(score, grade) %>%
  drop_na(score) %>%
  group_by(grade) %>%
  summarize(min = min(score),
            mean = mean(score),
            median = median(score),
            max = max(score))
   

nyc_restaurants %>%
  select(score, grade) %>%
  drop_na(score) %>%
  ggplot() + 
  geom_density(aes(score, fill = grade)) +
  theme_bw()



## ------------------------------------------------------------------------
scores <- nyc_restaurants %>%
  select(cuisine_description, score) %>%
  group_by(cuisine_description) %>%
  na.omit() %>%
  summarize(mean = mean(score),
            total = n(),
            low = qbeta(0.025, mean + 0.5, total - mean + 0.5),
            high = qbeta(0.975, mean + 0.5, total - mean + 0.5),
            segment = ifelse(mean >= 21.336, "above", 
                             ifelse(mean <= 17.665, "below", "average"))) %>%
  na.omit()

summary(scores$mean)
#1stQ: 17.665
#3rdQ: 21.336

scores %>%
  filter(total > 100) %>%
  arrange(desc(total)) %>%
  select(cuisine_description, low, mean, high, total, segment) %>% 
  ggplot() +
  geom_point(aes(reorder(cuisine_description, -mean), mean, color = segment),
             size = 2) + 
  geom_errorbar(aes(x=cuisine_description, ymin=mean-low, ymax=mean+high, color = segment),
                size = 0.5) +
  coord_flip() +
  theme_bw(base_size = 10) +
  labs(x = "",
       y = "",
       title = "Average inspection score for NY restaurants by cuisine",
       subtitle = "Best score of \"5.733\"; minimum 100 inspections per cruisine type",
       caption = "Source: NYC Open Data") + 
  scale_color_brewer(palette = 'Accent', direction = -1) +
  theme(legend.position = 'none')



## ------------------------------------------------------------------------
cuisine_grades <- nyc_restaurants %>%
  select(cuisine_description, grade) %>%
  na.omit() %>%
  group_by(cuisine_description) %>%
  count(grade) %>%
  mutate(total = sum(n),
         pct_total = n/total) %>%
  ungroup()

ebb_cuisine_grades <- cuisine_grades %>%
  add_ebb_estimate(n, total) %>%
  filter(grade == "A") %>%
  arrange(desc(.fitted)) %>%
  filter(n >= 100) %>%
  head(30) 

ebb_cuisine_grades %>%
  select(cuisine_description, "Empirical Bayes Rate"=.fitted, "Measured Rate"=.raw, .low, .high) %>%
  gather(key, value, -cuisine_description, -.low, -.high) %>%
  ggplot() + 
  geom_point(aes(reorder(cuisine_description, value), value, color = key), size = 3) +
  geom_errorbar(aes(ymin = .low, ymax = .high, x=cuisine_description), color = "gray50") +
  scale_y_continuous(labels = percent_format(round(1))) +
  coord_flip() +
  theme_minimal(base_size = 15) +
  labs(x = "",
       y = "",
       title = "Rate of NYC restaurant inspections with a final grade of \'A\' by cuisine type",
       subtitle = "95% credible intervals with a minimum of 100 inspections",
       caption = "Source: NYC Open Data") +
  scale_color_brewer(palette = 'Set1', direction = -1) +
  theme(legend.title=element_blank())



## ------------------------------------------------------------------------
top_cuisines <- nyc_restaurants %>%
  select(cuisine_description, score) %>%
  count(cuisine_description) %>%
  arrange(desc(n)) %>%
  top_n(20)

# density by score
nyc_restaurants %>%
  select(cuisine_description, score) %>% 
  left_join(top_cuisines) %>% 
  drop_na(n) %>%
  ggplot() + 
  geom_density(aes(score, fill = cuisine_description, color = cuisine_description), 
               alpha = 0.1) +
  scale_x_log10() +
  theme_bw()



## ------------------------------------------------------------------------
nyc_restaurants %>%
  select(cuisine_description, boro, score) %>%
  group_by(cuisine_description, boro) %>%
  na.omit() %>%
  summarize(mean = mean(score),
            total = n(),
            low = qbeta(0.025, mean + 0.5, total - mean + 0.5),
            high = qbeta(0.975, mean + 0.5, total - mean + 0.5)) %>%
  na.omit() %>% 
  top_n(50) %>% 
  select(cuisine_description, boro, low, mean, high, total) %>% 
  ggplot() +
  geom_point(aes(reorder(cuisine_description, mean), mean)) + 
  geom_errorbar(aes(x=cuisine_description, ymin=mean-low, ymax=mean+high)) +
  coord_flip() +
  theme_bw() +
  facet_wrap(~boro)


