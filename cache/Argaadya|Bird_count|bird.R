library(tidyverse)
library(ggridges)
setwd(dir = "D:/R/tidytuesday/bird_count/")
bird <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-18/bird_counts.csv")
data <- na.omit(bird)

#What Bird is the Top Species? ------------------------------------------
top_species <- data %>% group_by(species) %>% 
  summarise(total= n(),counted = mean(how_many_counted), hour = mean(total_hours))
top_species <- top_species[order(top_species$counted,decreasing = T),]
top_species <- top_species[1:20,]
top_species <- top_species[order(top_species$counted),]
top_species$species <- factor(top_species$species,top_species$species)

png("bird1.png", width = 3840, height = 2160, units = 'px', res = 300)
ggplot(top_species, aes(species,counted))+
  geom_col(aes(fill=species))+
  coord_flip()+
  scale_fill_discrete(guide=F)+
  labs(title = "What Bird is The Most Observed Overall?",
       subtitle = "Mean of Number of Bird Observed Annually",
       x = "Species", y ="Number of Birds",
       caption = "Data: Bird Studies Canada")+
  geom_text(aes(label=round(counted,2)),nudge_y = 1000)+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = "lightgray"))
dev.off()

#How is the trend of Bird sighting in years? -----------------------------
top_species <- data %>% group_by(species) %>% 
  summarise(total= n(),counted = mean(how_many_counted), hour = mean(total_hours))
top_species <- top_species[order(top_species$counted),]
top_species <- top_species[(nrow(top_species)-4):(nrow(top_species)),]
data_year <- data[data$species %in% top_species$species,]
data_year$species <- factor(data_year$species,top_species$species)

png("bird2.png", width = 3840, height = 2160, units = 'px', res = 300)
ggplot(data_year, aes(year,how_many_counted,color=species))+
  geom_point()+
  geom_line()+
  theme_light()+
  labs(title = "Trend of Number of Top 5 Bird Species Observed Annually",
       subtitle = "Many of bird observed only start to trend just before 2000, while the European Starling sighting was booming after 1960 before starting to decline in 1990",
       x = "Year", y ="Number of Birds",
       caption = "Data: Bird Studies Canada")+
  scale_x_continuous(breaks = c(1920,1940,1960,1980,2000,2020))
dev.off()  

#Distribution of Bird Counted ------------------------------------------
top_species <- data %>% group_by(species) %>% 
  summarise(total= n(),counted = mean(how_many_counted), hour = mean(total_hours))
top_species <- top_species[order(top_species$counted,decreasing = T),]
top_species <- top_species[1:10,]
top_species <- top_species[order(top_species$counted),]
top_species$species <- factor(top_species$species,top_species$species)
data_year <- data[data$species %in% top_species$species,]
data_year$species <- factor(data_year$species,top_species$species)

png("bird3.png", width = 3840, height = 2160, units = 'px', res = 300)
ggplot(data_year, aes(species,how_many_counted, fill = species))+
  geom_boxplot(alpha=1/2)+
  scale_fill_discrete(guide=F)+
  theme_ridges()+
  coord_flip()+
  labs(title = "Number of Bird Observed Annually",
       subtitle = "European Starling has the widest range in number of birds observed",
       x = "Species", y ="Number of Birds",
       caption = "Data: Bird Studies Canada")
dev.off()

#How many hours people spend on bird sighting annually? -------------------
sight <- data %>% group_by(year) %>% 
  summarise(total = n(), hour = mean(total_hours))

png("bird4.png", width = 3840, height = 2160, units = 'px', res = 300)
ggplot(sight, aes(year,hour))+
  geom_point()+ geom_smooth(alpha=1/8)+
  geom_line()+
  theme_light()+
  labs(title = "How many hours people spend on bird sighting annually?",
       subtitle = "There is an increase in number of hours people spend to observe birds",
       x = "Year", y ="Number of Hours",
       caption = "Data: Bird Studies Canada")+
  scale_x_continuous(breaks = c(1920,1940,1960,1980,2000,2020))
dev.off()  

#How many species of bird spotted every year? -------------------------------
birda <- data %>% filter(how_many_counted > 0)
birda <- birda %>% group_by(year) %>% 
  summarise(hour = mean(total_hours), bird = sum(how_many_counted), 
            species = n())
birda <- birda %>% filter(year>1930)

png("bird5.png", width = 3840, height = 2160, units = 'px', res = 300)
ggplot(birda, aes(year,species))+
  geom_line(color="navy",alpha=1/2)+
  geom_point(aes(size=bird,color=bird),alpha=1/2)+
  theme_light()+
  theme(panel.grid = element_blank())+
  labs(title = "How many species of bird spotted every year?",
       subtitle = "There is positive trend on number of species observed every year",
       x = "Year", y ="Number of Species",
       caption = "Data: Bird Studies Canada",
       color = "Bird Observed")+
  scale_x_continuous(breaks = c(1920,1940,1960,1980,2000,2020))+
  scale_size(guide=F)
dev.off()  

#Correlation between total hour and number of species ----------------------
sight <- data %>% filter(how_many_counted > 0 & year > 1930) %>% 
  group_by(year) %>% 
  summarise(species = n(), hour = mean(total_hours), bird = sum(how_many_counted))

png("bird6.png", width = 3840, height = 2160, units = 'px', res = 300)
ggplot(sight, aes(hour,species,color=bird,size=bird))+
  geom_point(alpha=1/2)+
  theme_ridges()+
  labs(title = "Correlation Between Total Hour and Number of Species",
       subtitle = "The more hour you spend, the more species of bird you can found",
       x = "Total Hour", y ="Number of Species",
       caption = "Data: Bird Studies Canada",
       color = "Bird Observed")+
  scale_size(guide=F)+
  scale_color_gradient(low = "red",high = "yellow")
dev.off()       
