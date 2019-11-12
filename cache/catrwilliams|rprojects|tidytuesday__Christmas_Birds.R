## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
setwd("~/../Google Drive/Data Analysis/Tidy Tuesday/06-18-19 - Christmas Bird Counts/")


## ----library, message=FALSE, results=FALSE-------------------------------
#function to check if packages are installed, if not then install them, and load all packages
libraries <- function(packages){
  for(package in packages){
    #checks if package is installed
    if(!require(package, character.only = TRUE)){
      #If package does not exist, then it will install
      install.packages(package, dependencies = TRUE)
      #Loads package
      library(package, character.only = TRUE)
    }
  }
}

packages <- c("data.table","tidyverse","visdat","zoo","extrafont","stringr")

libraries(packages)

theme_set(theme_classic())


## ----import, message=FALSE-----------------------------------------------
df <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-18/bird_counts.csv", stringsAsFactors = TRUE)


## ----view----------------------------------------------------------------
glimpse(df)
head(df)
summary(df)
sapply(df, function(x) n_distinct(x)) %>% sort()


## ----missing-------------------------------------------------------------
#Visualize missing values
vis_miss(df, sort_miss=TRUE)

#see count of missing values
na_values <- function(df){
  na <- colSums(is.na(df)) %>% sort(decreasing=TRUE)
  na[na>0]
}

na_values(df)


## ----eda, warning=FALSE, out.width="75%"---------------------------------
df %>% ggplot(aes(year,(how_many_counted/total_hours)))+
  geom_bar(stat="identity")+
  labs(title="Christmas Bird Counts Per Hour Over Time", x="Year", y="Count per Hour")

df %>% ggplot(aes(total_hours, how_many_counted))+
  geom_jitter(alpha=0.3, size=1)+
  labs(title="Total Christmas Bird Counts",x="Hours", y="Count")


## ----wrangle-------------------------------------------------------------
#remove NA hours if how_many_counted is 0
df <- df[!(df$how_many_counted==0 & is.na(df$total_hours)),]

#view missing data again
na_values(df)

# use interpolated values to replace NA values, grouped by species
df <- df %>% group_by(species) %>% mutate(total_hours = na.approx(total_hours)) %>% ungroup()

#view missing data again
na_values(df)

#drop how_many_counted_by_hour. this can be recalculated later
df <- df %>% select(1:5)


## ----viz, out.width="100%"-----------------------------------------------
# how many owls were seen
df_owl <- df %>% filter(str_detect(species, "Owl"))
df_owl <- df_owl %>% group_by(species) %>% summarize(counted = sum(how_many_counted))
df_owl <- df_owl %>% mutate(species = str_sub(species, end=-4))
df_owl$species <- fct_reorder(df_owl$species, df_owl$counted)

df_owl %>% ggplot(aes(species,counted))+
  geom_col(fill="steelblue")+
  geom_text(aes(label=counted), hjust=-0.1, vjust=0.5, family="Bodoni MT", fontface="bold")+
  coord_flip()+
  labs(title="Christmas Owl Spottings", subtitle="From 1921 - 2017", y="Number Spotted")+
  theme(legend.position="none",
        text=element_text(family="Bodoni MT", size=14),
        axis.title.y=element_blank(),
        plot.title=element_text(face="bold", size=24, hjust=0.175),
        plot.subtitle=element_text(hjust=0.325),
        plot.background=element_rect(fill="#fff7e6"),
        panel.background=element_rect(fill="#fff7e6"))

ggsave("christmas_owls.png", limitsize=FALSE)

