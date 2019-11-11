library(tidyverse)
library(magrittr)
# install.packages("remotes")
# remotes::install_github("dgrtwo/drlib") # drlibr includes the functions used for correctly sorting data within facets

nobel_winner_all_pubs <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winner_all_pubs.csv")

nobel_winner_all_pubs$journal<-gsub("proceedings of the national academy of sciences of the united states of america", "PNAS", nobel_winner_all_pubs$journal)

nobel_winner_all_pubs$journal<-gsub("journal of the chemical society chemical communications", "J Chem Soc, Chem Commun", nobel_winner_all_pubs$journal)

nobel_winner_all_pubs$journal<-gsub("journal of the american chemical society", "J. Am. Chem. Soc.", nobel_winner_all_pubs$journal)

nobel_winner_all_pubs$journal <- str_to_title(nobel_winner_all_pubs$journal)

nobel_winner_all_pubs$category <- str_to_title(nobel_winner_all_pubs$category)

nobel_winner_all_pubs %>% 
  filter(is_prize_winning_paper=="YES", !is.na(journal)) %>% 
  select(journal, category) %>% 
  group_by(category, journal) %>% 
  count() %>% 
  group_by(category) %>% 
  arrange(desc(n)) %>% 
  slice(1:15) %>% 
  ggplot(aes(drlib::reorder_within(x=journal, by=n, within=category), y=n))+
  geom_col(aes(fill=n))+ 
  drlib::scale_x_reordered() + 
  facet_wrap(~category, scales = "free")+ 
  labs(x="", y="", title = "Top 15 Journals Publishing Laureate Articles")+
  theme_minimal()+
  theme(plot.title = element_text(size=26))+
  scale_fill_viridis_c(option = "inferno")+
  coord_flip()
