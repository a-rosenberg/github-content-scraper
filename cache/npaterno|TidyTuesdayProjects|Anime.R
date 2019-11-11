library(tidyverse)

tidy_anime <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/tidy_anime.csv")

toei_tv <- tidy_anime %>% 
  filter(studio=="Toei Animation" & type == "TV") %>% 
  group_by(name,episodes, rank) %>% 
  summarize(score=mean(score)) %>% 
  mutate(highlight = case_when(name=="Dragon Ball Z"~"highlight",
                               TRUE ~ "normal"))

my_colors <- c("highlight"="dark red","normal"="blue")


ggplot(toei_tv)+
  geom_point(mapping=aes(episodes, score,size=rank, color = highlight) ,alpha=0.3)+
  scale_color_manual("Status", values = my_colors,guide=FALSE) +
  geom_text(data = subset(toei_tv,name=="Dragon Ball Z"),
                           aes(x = episodes, y = score, label = name),hjust=1,vjust=-0.5) +
  theme_bw()+
  labs(title="Average Rating for Toei Studios",
      subtitle = "TV Shows",
      size= "Rank",
       x="Number of Episodes",
       y="Average Rating",
       caption = "Source: myanimelist.net | Graph: @Mathl3t3")

ggplot()+
  geom_point(toei_tv,mapping=aes(rank, score, color=highlight), alpha=0.3)+
  scale_color_manual("Status", values = my_colors,guide=FALSE) +
  geom_text(data = subset(toei_tv,name=="Dragon Ball Z"),
            aes(x = episodes, y = score, label = name),hjust=-0.2) +
  theme_bw()+  labs(title="Average Rating for Toei Studios",
       subtitle = "TV Shows",
       x="Rank",
       y="Average Rating",
       caption = "Source: myanimelist.net | Graph: @Mathl3t3")
