library(tidyverse)
library(lubridate)
library(ggridges)
library(viridis)
library(ggdark)
library(LaCroixColoR)
# clean dataset from lizawood's github
url <- "https://raw.githubusercontent.com/lizawood/apps-and-games/master/PC_Games/PCgames_2004_2018_raw.csv"

# read in raw data
raw_df <- url %>% 
  read_csv() %>% 
  janitor::clean_names() 

# clean up some of the factors and playtime data
clean_df <- raw_df %>% 
  mutate(price = as.numeric(price),
         score_rank = word(score_rank_userscore_metascore, 1),
         average_playtime = word(playtime_median, 1),
         median_playtime = word(playtime_median, 2),
         median_playtime = str_remove(median_playtime, "\\("),
         median_playtime = str_remove(median_playtime, "\\)"),
         average_playtime = 60 * as.numeric(str_sub(average_playtime, 1, 2)) +
           as.numeric(str_sub(average_playtime, 4, 5)),
         median_playtime = 60 * as.numeric(str_sub(median_playtime, 1, 2)) +
           as.numeric(str_sub(median_playtime, 4, 5)),
         metascore = as.double(str_sub(score_rank_userscore_metascore, start = -4, end = -3))) %>% 
  select(-score_rank_userscore_metascore, -score_rank, -playtime_median) %>% 
  rename(publisher = publisher_s, developer = developer_s)



pub <- clean_df %>% 
  filter(publisher == "Paradox Interactive") %>%
  mutate( release_date = mdy(release_date)) %>%
  mutate( release_date = year(release_date)) %>%
  arrange(release_date) %>%
  filter(price != "NA")
  
  

ggplot(pub, aes(x = price, y = release_date, group = release_date, fill = ..x..)) + 
  geom_density_ridges_gradient() +
  scale_fill_viridis(option= 'B', name = "Game Price [$]") +
  dark_theme_grey() +
  labs(title = 'Are Paradox Interactive video game prices increasing?') +
  theme(legend.position = "bottom",
        axis.text.x=element_text(size=11, face = "bold", hjust = 1, color = "deeppink"),
        axis.text.y = element_text(size = 10, color = "deeppink", face = "bold"),
        axis.title.x = element_text(color = "deeppink", face = "bold"),
        legend.title = element_text(color = "deeppink", size = 12, face = "bold"),
        legend.text = element_text(color = "deeppink", face = "bold"),
        plot.title = element_text(color = "deeppink", face = "bold"))

ggsave("2019-07-30/paradox.png", width = 7, height = 7)

