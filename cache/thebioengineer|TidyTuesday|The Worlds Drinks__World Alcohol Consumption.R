## ----import and load-----------------------------------------------------
library(tidyverse)
library(httr)  
library(rgdal)
library(ggthemes)

alcohol<-read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/week13_alcohol_global.csv")
set.seed(9917)


## ----normalization-------------------------------------------------------
NormAlcohol<-alcohol%>%
  mutate(TotalServings=beer_servings+spirit_servings+wine_servings,
         beer=beer_servings/TotalServings,
         spirits=spirit_servings/TotalServings,
         wine=wine_servings/TotalServings
         )%>%
  select(country,beer,spirits,wine)%>%
  replace_na(list(beer=0,spirits=0,wine=0))%>%
  data.frame%>%
  `rownames<-`(.,.$country)

alcoholPRCOMP<-NormAlcohol%>%
  select(-country)%>%
  prcomp()

cluster<-alcoholPRCOMP$x%>%
  data.frame%>%
  mutate(Cluster=kmeans(.,centers=7)%>%'$'(cluster),
         country=rownames(.))


ggplot(cluster)+
  geom_point(aes(x = PC1, y=PC2, color=factor(Cluster)))



## ----Drinking_clusters---------------------------------------------------
summary_groups<-NormAlcohol%>%
  mutate(country=rownames(.))%>%
  merge(cluster[,c("Cluster","country")])%>%
  group_by(Cluster)%>%
  summarise(
    totalDrinks=sum(beer)+sum(wine)+sum(spirits),
    beer=sum(beer),
    wine=sum(wine),
    spirits=sum(spirits),
    countries=paste(country,collapse=", "))%>%
  mutate(beer=beer/totalDrinks,wine=wine/totalDrinks,spirits=spirits/totalDrinks)%>%
  select(-totalDrinks)

knitr::kable(summary_groups)



## ----choropleth----------------------------------------------------------

# this ensures you only download the shapefile once and hides
# errors and warnings. remove `try` and `invisible` to see messages
try(invisible(GET("http://www.pewglobal.org/wp-content/lib/js/world-geo.json",write_disk("world-geo.json"))), silent=TRUE)

world <- readOGR("world-geo.json")
world_wt <- spTransform(world, CRS("+proj=robin"))
world_map <- fortify(world_wt)%>%
  left_join(data_frame(id=rownames(world@data), name=world@data$name)) %>%
  select(-id)%>%
  rename(id=name) %>%
  mutate(id=recode(id,
                        "Republic of the Congo"="Congo",
                        "Ivory Coast"="Cote d'Ivoire",
                        "Democratic Republic of the Congo"="DR Congo",
                        "Burma (Myanmar)"="Myanmar",
                        "Cape Verde"="Cabo Verde",
                        "United States"="USA",
                        "Antigua and Barbuda"="Antigua & Barbuda",
                        "Guinea Bissau"="Guinea-Bissau" ,
                        "Russia"="Russian Federation",
                        "St. Kitts and Nevis"="St. Kitts & Nevis",
                        "St. Vincent and the Grenadines"="St. Vincent & the Grenadines",
                        "Trinidad and Tobago"="Trinidad & Tobago",
                        "Bosnia and Herzegovina"="Bosnia-Herzegovina",
                        "Kyrgyz Republic"="Kyrgyzstan",
                        "Republic of Macedonia"="Macedonia",
                        "Sao Tome and Principe"="Sao Tome & Principe"
  ))

GroupedCountries<-summary_groups%>%
  mutate(DrinkerType=c("Beer","Mainly Beer","Most Beer and Spirits","Some Beer, Mostly Spirits","Non-Drinkers","Spirits","Wine and Beer"))%>%
  separate_rows(countries,sep = ", ")%>%
  rename(country=countries)%>%
  merge(alcohol,by="country")%>%
  mutate(hover=paste("Country:",country,"<br>Group:",DrinkerType,
                     "<br>Beer:",beer_servings,"<br>Sprits:",spirit_servings,"<br>Wine:",wine_servings))%>%
  merge(world_map,
        
        by.x="country",by.y="id")

gg<-ggplot() +
  geom_map(data=world_map, map=world_map,
                    aes(x=long, y=lat, map_id=id),
                    color="#7f7f7f", fill="white", size=0.15)+
  geom_map(data=GroupedCountries,
           map=world_map,
           aes(map_id = country, x=long, y=lat, fill=DrinkerType,text=hover))+
  theme_map()+
  theme(legend.position="bottom")+
  guides(fill=guide_legend(title="Drinking Habit"))+
  labs(title="World Drinking Habits\n")

ggsave(gg,filename = "World_Drinking_habits.PNG",height = 7,width = 10)
 
gg


