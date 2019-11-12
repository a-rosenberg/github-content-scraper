## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


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

packages <- c("tidyverse","naniar","rvest","textstem","modeest","countrycode","ggridges","viridis")

libraries(packages)

theme_set(theme_classic()) #applies classic theme to all charts


## ----import, message=FALSE-----------------------------------------------
df <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-04/ramen_ratings.csv")


## ----view----------------------------------------------------------------
glimpse(df)
head(df)
summary(df)
sapply(df, function(x) n_distinct(x)) %>% sort()


## ----missing-------------------------------------------------------------
#Visualize missing values
gg_miss_var(df) + labs(title="Missing Values")

#see count of NA values
df %>% is.na() %>% colSums() %>% sort(decreasing=TRUE)

#view only rows with NAs
df <- df %>% rownames_to_column()  #add row number to make it easier to locate observations
df %>% filter_all(any_vars(.=="NA"|is.na(.)))

#add in missing value for review_number
df[189:191,]
df[190,2] <- 2991

#find missing values for style and stars
df <- df %>% group_by(country, brand)%>% mutate(style = replace_na(style, mfv1(style, na.rm=T)),
                                                stars = replace_na(stars, mean(stars, na.rm=T))) %>% ungroup()

#see count of NA values again
df %>% is.na() %>% colSums() %>% sort(decreasing=TRUE)

#there are still some missing values, so we'll try again with less grouping
#find missing values for stars
df <- df %>% group_by(country)%>% mutate(stars = replace_na(stars, mean(stars, na.rm=T))) %>% ungroup()


## ----type----------------------------------------------------------------
df <- df %>% mutate_if(is.character,as.factor)


## ----country, warning=FALSE, message=FALSE-------------------------------
#import data frame with list of countries
country <- read_csv("https://docs.google.com/spreadsheets/d/1NjSI2LaS3SqbgYc0HdD8oIb7lofGtiHgoKKATCpwVdY/export?format=csv&gid=1088874596", skip=1)
head(country)

#there appear to be 2 columns with slightly different names. gathering these into one column
country <- country %>% gather(column, name, c(`#country +name +i_en +alt +v_unterm`, `#country +name +preferred`)) %>% select(name) %>% distinct()

#sort out country values that are NOT in the country name to correct them
country_fix <- df[!df$country %in% country$name,] %>% select(country) %>% distinct()
country_fix

#change values to appropriate names
df <- df %>% mutate(country = as.character(country),
                    country = (case_when(country %in% "USA" ~ "United States",
                                        country %in% "Dubai" ~ "United Arab Emirates",
                                        country %in% "Holland" ~ "Netherlands",
                                        country %in% "Sarawak" ~ "Malaysia",
                                        country %in% "UK" ~ "United Kingdom",
                                        country %in% "Phlippines" ~ "Philippines",
                                        TRUE ~ country)),
                    country = as.factor(country)) 


## ----brand---------------------------------------------------------------
df <- df %>% mutate(brand = str_replace_all(brand, "[:punct:]+", " "),
                    brand = str_replace(brand, " s", "'s"),
                    brand = str_to_title(brand),
                    brand = str_squish(brand),
                    brand = stem_words(brand) %>% as.factor())


## ----variety-------------------------------------------------------------
df <- df %>% mutate(variety = str_replace_all(variety, "[:punct:]+", " "),
                    variety = str_replace(variety, " s", "'s"),
                    variety = str_to_title(variety),
                    variety = str_squish(variety),
                    variety = stem_words(variety) %>% as.factor())


## ----style---------------------------------------------------------------
df %>% count(style, sort=T)
df <- df %>% group_by(style) %>% filter(n()>4) %>% ungroup()


## ----feature-------------------------------------------------------------
# change countries into regions
df$region <- df$country %>% countrycode(origin="country.name",destination="region") %>% as.factor()
df <- df %>% group_by(region) %>% filter(n()>10) %>% ungroup()

count(df, region, sort=TRUE)

# change countries into continents
df$continent <- df$country %>% countrycode(origin="country.name",destination="continent") %>% as.factor()

count(df, continent, sort=TRUE)


## ----country mean, out.width="75%"---------------------------------------
df_country <- df %>% group_by(country) %>% summarize(stars=mean(stars)) %>% ungroup()

df_country %>% mutate(country = fct_reorder(country, stars)) %>% 
  ggplot(aes(country, stars))+
  geom_bar(stat="identity")+
  coord_flip()


## ----region mean, out.width="75%"----------------------------------------
df_region <- df %>% group_by(region) %>% summarize(stars=mean(stars)) %>% ungroup()

df_region %>% mutate(region = fct_reorder(region, stars)) %>%
  ggplot(aes(region, stars))+
  geom_bar(stat="identity")+
  coord_flip()


## ----continent mean, out.width="75%"-------------------------------------
df_continent <- df %>% group_by(continent) %>% summarize(stars=mean(stars)) %>% ungroup()

df_continent %>% mutate(continent = fct_reorder(continent, stars)) %>%
  ggplot(aes(continent, stars))+
  geom_bar(stat="identity")+
  coord_flip()


## ----style mean, out.width="75%"-----------------------------------------
df_style <- df %>% group_by(style) %>% summarize(stars=mean(stars)) %>% ungroup()

df_style %>% mutate(style = fct_reorder(style, stars)) %>%
  ggplot(aes(style, stars))+
  geom_bar(stat="identity")+
  coord_flip()


## ----final, message=FALSE------------------------------------------------
df %>% mutate(region = fct_reorder(region, stars)) %>% 
  ggplot(aes(stars, region, fill = region)) +
  geom_density_ridges() +
  scale_fill_viridis(option = "D", discrete = TRUE) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks=0:5) +
  labs(title="Ramen Ratings by Region", x="Rating", y="Region")

