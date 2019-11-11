# Kathleen Cachel

library(tidyverse)
library(reshape2)
library(scales)
library(ggdark)
library(ggthemes)
library(ggpomological)

women_research_raw <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/Economist_women-research.csv")

research_titles <- c("country",
                     "Health sciences",
                     "Physical sciences",
                     "Engineering",
                     "Computer science, maths",
                     "Women inventors")

# remove rows with NA values
# update Column Names
women_research_clean <- women_research_raw %>% 
  na.omit() %>% 
  set_names(nm = research_titles) %>% 
  filter(country != "Country" & country != "Brazil" & country != "Denmark" & country !="Britain" &
           country != "France" & country != "United Kingdom") %>% 
  gather(field, percent, `Health sciences`:`Women inventors`)

#make men version
men_research_clean <- women_research_clean %>%
  mutate(percent = 1 - as.numeric(percent))

#create new gender column
women_research_clean$gender <- "female"
men_research_clean$gender <- " amale"

#update type in women data frame
women_research_clean$percent <- as.numeric(women_research_clean$percent)

#union rows to make one big tidy data set
research_clean <- union(women_research_clean, men_research_clean)




#plotting
united_research <-  unite(research_clean, field_gender, field, gender, sep = ".", remove = FALSE)


ggplot(data=united_research, aes(x=field, y=percent, fill=field_gender)) + 
  geom_bar(stat="identity") + 
  facet_grid(country~., switch = "y")+
  scale_fill_manual(breaks = c("Women inventors.female", "Physical sciences.female","Health sciences.female", "Engineering.female", "Computer science, maths.female"),
                    values = c("#efedf5", "#756bb1","#e5f5e0" , "#31a354","#deebf7", "#3182bd",
                               "#fde0dd", "#c51b8a","#fff7bc", "#d95f0e"), 
                    labels =c("Women inventors", "Physical sciences","Health sciences", "Engineering", "Computer science & Math"),
                    name = "Field:")+
  coord_flip()+
  theme_economist_white( base_size = 14)+
  theme(axis.text.y = element_blank(),
        legend.position = "bottom",
        strip.text.y = element_text(angle = 180),
        legend.text = element_text(size =10, face = "bold")
  )+
  labs(x = "", y = "Percent of total field that are Women",
       title = "Still a man's world",
       subtitle = "Women among researchers with papers published 2011-2015 *",
       caption = "Sources: 'Gender in the Global Research Landscape' by Elsevier *Indexed in Scopus")+
  scale_y_continuous(labels = percent)+
  geom_hline(yintercept=.50, linetype="dashed", 
             color = "red", size=2)

ggsave("my_women_researcher_plot.png", width = 10.5, height = 8)


