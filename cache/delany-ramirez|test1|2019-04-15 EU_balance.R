library(tidyverse)


eu_balance <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/eu_balance.csv")


## Just factor declarations for country and account_type

eu_balance <- eu_balance %>% 
  mutate(country = as_factor(country),
         account_type = as_factor(account_type))

## Did a percentual balance growth, but at the end didn't use it

eu_balance %>% 
  filter(year %in% c(2009,2015), account_type == "current") %>% 
  spread(key = "year", value = "value") %>% 
  mutate(balance_growth = (`2015` - `2009`) / `2015`) -> eu_balance_growth 

min(eu_balance_growth$`2009`)
min(eu_balance_growth$`2015`)

## Graph 2009 to 2015 Growth relationship of Current Values by Country

eu_balance_growth %>% 
  mutate(`2009` = `2009` + 56192,
         `2015` = `2015` + 18091) %>% ## Had to add values for the log transformation
  ggplot(aes(x = `2009`, y = `2015`, col = country, label = country)) +
  geom_jitter(alpha = 0.5) +
  geom_text(vjust = -0.5) +
  scale_x_log10() + 
  scale_y_log10() + 
  labs(title = "2009 to 2015 Growth relationship of Current Values by Country",
       subtitle = "Source: The Economist",
       x = "2009 \n log10 value",
       y = "2015 \n log10 value") +
  theme_light() +
  theme(legend.position = "none")

## 2009 to 2015 Growth of Current Values by Country

eu_balance_growth %>% 
  select(country, `2009`,`2015`) %>% 
  mutate(country = fct_reorder(country, `2015`)) %>% 
  gather(key = year, value = value, `2009`:`2015`, -country) %>% 
  ggplot(aes(x = country, y = value, group = country, col = year)) + 
  geom_point(size = 2) +
  geom_path(arrow = arrow(length = unit(1.5, "mm"), type = "closed"), col = "DarkBlue") + 
  coord_flip() +
  labs(title = "2009 to 2015 Growth of Current Values by Country",
       subtitle = "Source: The Economist",
       x = "Country",
       y = "2009 to 2015 value") +
  theme_light() 
