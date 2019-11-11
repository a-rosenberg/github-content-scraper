library(tidyverse)
library(USAboundaries)
library(sf)

hpi <- read_csv("https://github.com/rfordatascience/tidytuesday/raw/master/data/2019/2019-02-05/state_hpi.csv")

now <- hpi %>% 
  filter(year == 2001,
         month == 11) %>% 
  select(state,
         year_now = year,
         price_index_now = price_index)

joined <- hpi %>% 
  filter(year >= 2007) %>% 
  group_by(state) %>% 
  filter(price_index == min(price_index)) %>% 
  select(state,
         year_low = year,
         month_low = month,
         price_index_low = price_index) %>% 
  left_join(now, by = "state") %>%
  mutate(price_index_diff = price_index_now / price_index_low - 1,
         price_index_pct  = scales::percent(price_index_diff))

state_boundaries <- USAboundaries::us_states() %>% 
  left_join(joined, by = c("stusps" = "state")) %>% 
  filter(!(stusps %in% c("AK", "DC", "HI", "PR")))

ggplot(state_boundaries, aes(fill = price_index_diff)) +
  geom_sf(col = "white") +
  geom_sf_text(aes(label = price_index_pct),
               check_overlap = T) +
  coord_sf(crs = 5070) +
  scale_fill_gradient(low  = "#87FA87", 
                      high = "#006400") +
  labs(title = "Western States had greater increases in HPI since lows",
       subtitle = "Should have invested in real estate in Nevada",
       fill  = "Pct. Increase") +
  theme_void() +
  theme(panel.grid = element_line(color = "white"))

