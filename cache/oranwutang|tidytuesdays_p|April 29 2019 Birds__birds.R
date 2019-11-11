library(tidyverse)
library(magrittr)

joined<-left_join(bird_collisions %>% filter(locality=="MP"), mp_light , by = "date")

joined %>%
  filter(!is.na(light_score)&family!="Laniidae"&family!="Icteridae") %>% 
  group_by(family) %>% 
  mutate(MedianLight=median(light_score)) %>% 
  ggplot(aes(x=reorder(family, -light_score, FUN=median), y=light_score))+
  geom_boxplot(aes(fill=MedianLight))+
  scale_fill_gradient(low="NA", high="steelblue")+
  ggthemes::theme_solarized()+
  theme(panel.grid = element_blank(),
        legend.position = "NA")+
  coord_flip()+
  ylab("Light Score")+
  xlab("Family")+
  labs(title="Birds' collisions vary according\nto Light Score",
          caption="Source:\nhttps://doi.org/10.1098/rspb.2019.0364\nhttps://doi.org/10.5061/dryad.8rr0498")
