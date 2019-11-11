# Load necessary libraries
library(tidyverse)
library(ggthemes)
library(nlstools)

# Load raw data
park_visits <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/national_parks.csv")
state_pop <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/state_pop.csv")
gas_price <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/gas_price.csv")

# Recreate first graph from fivethirtyeight article https://fivethirtyeight.com/features/the-national-parks-have-never-been-more-popular/
total_park_visits<-park_visits%>%
  filter(year!="Total" & unit_type=="National Park")%>%
  mutate(Year=as.double(year))%>%
  group_by(Year)%>%
  summarize(total=sum(visitors)/1000000)

ggplot(total_park_visits, aes(x = Year, y = total))+
  geom_path(size = 1.25, color="Dark Green", alpha = 0.8)+
  geom_area(fill="Dark Green", alpha = 0.4)+
  scale_x_continuous( limits = c(1900, 2020),
                   labels = c("", 1910, " '20" , " '30" , " '40" , " '50" , " '60", " '70", " '80", " '90", "2000", " '10"),
                   breaks = c(1900, 1910, 1920, 1930, 1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010))+
  scale_y_continuous(limits = c(0,90),
                     labels = c(0, 20, 40, 60, "80 m"),
                     breaks = c(0, 20, 40, 60, 80))+
  labs(title = "U.S. national parks have never been so popular",
       subtitle = "Annual recreational visits to national parks since 1904",
       x= "", y="", 
       caption = "Source: data.world | Graph: @Mathl3t3")+
  theme_fivethirtyeight()

# Exploratory plots 

# Create data set to compare visits to gas price and population
visits_merge<-park_visits%>%
  filter(year>=1929 & year!= "Total")%>%
  mutate(year = as.double(year))%>%
  group_by(year)%>%
  summarize(total_visits=sum(visitors)/10^6)

state_pop_merge<-state_pop%>%
  filter(year>=1929)%>%
  group_by(year)%>%
  summarize(population = sum(pop))%>%
  rbind(c(2016,NA))

gas_price_merge<-gas_price%>%
  rbind(c(2016,NA,NA))

full_data<-visits_merge%>%
  full_join(state_pop_merge, by = "year")%>%
  full_join(gas_price_merge, by = "year")

# Plot park visits (in millions) as a function of population (in millions)
# Fit with a negative quadratic model
ggplot(full_data, aes(population/10^6, total_visits))+
  geom_point(aes(size=gas_current), alpha=0.6,na.rm=TRUE, show.legend=FALSE)+
  geom_smooth(method="nls", se=FALSE, formula=y ~ -a*(x-b)^2+k,
              method.args=list(start=c(a=1, b=1, k=1)))+
  theme_fivethirtyeight()+
  labs(title = "Total National Park Visits",
       subtitle = "As a funtion of national population")

# Plot gas price as a function of population
#
ggplot(full_data, aes(population/10^6, gas_current))+
  geom_point(aes(color=total_visits))+
  geom_smooth(method = "nls", se=FALSE, formula = y ~ a*exp(b*x),
              method.args=list(start=c(a=1, b=0)))+
  scale_color_gradient(name="National Park Visits\n(Millions)",low="yellow",high="red")+
  theme(panel.background = element_rect(fill = "lightblue",
                                                colour = "lightblue",
                                                size = 0.5, linetype = "solid"))+
  labs(title = "Gas Price as a Function of Population",
       x = "Population (millions)",
       y = "Gas Price ($)", 
       caption = "Source: data.world | Graph: @Mathl3t3")
