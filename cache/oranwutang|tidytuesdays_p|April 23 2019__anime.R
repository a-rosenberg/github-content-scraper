library(tidyverse)
library(magrittr)
library(lubridate)
library(lisa)

raw_anime<-readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/raw_anime.csv")
clean_raw<-raw_anime %>%   # Aired
  mutate(aired = str_remove(aired, "\\{"),
         aired = str_remove(aired, "\\}"),
         aired = str_remove(aired, "'from': "),
         aired = str_remove(aired, "'to': "),
         aired = word(aired, start = 1, 2, sep = ",")) %>% 
  separate(aired, into = c("start_date", "end_date"), sep = ",") %>% 
  mutate(start_date = str_remove_all(start_date, "'"),
         start_date = str_sub(start_date, 1, 10),
         end_date = str_remove_all(start_date, "'"),
         end_date = str_sub(end_date, 1, 10)) %>%
  mutate(start_date = lubridate::ymd(start_date),
         end_date = lubridate::ymd(end_date)) %>% 
  # Drop unranked or unpopular series
  filter(rank != 0,
         popularity != 0)


clean_raw$start_date<-ymd(clean_raw$start_date)
clean_raw$end_date<-ymd(clean_raw$end_date)

# weight(x, weights, digits = 0)
clean_raw %>% filter(start_date>"1970-1-1" & rating!="None") %>%
  ggplot(aes(x=start_date, y=score, color=rating)) + 
  geom_point(alpha=0.04) +
  geom_smooth(se = FALSE, size=1.4) + 
  scale_color_manual(values = rev(lisa$ReneMagritte)) + 
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background=element_rect(fill="transparent",colour=NA),
        plot.background=element_rect(fill="transparent",colour=NA),
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        legend.position = c(0.3, 0.15),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12), 
        plot.title = element_text(size = 18))+
  labs(x="Start Date", y= "Score", color="Rating",
       title = "Score of anime\ncategories over time",
       subtitle = "Violence and profanity are on the rise")

