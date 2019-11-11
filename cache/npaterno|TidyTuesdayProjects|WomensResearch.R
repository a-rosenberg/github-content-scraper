library(tidyverse)

#load data
women_research <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/women_research.csv")

#plot percent women v country 
ggplot(women_research,aes(x=country,y=percent_women,fill=field))+
  geom_col(position="dodge")+
  theme_dark() %+replace%
  theme(axis.text = element_text(angle=90))+
  labs(title = "Published Women Researchers: 2011-2015", 
       subtitle="as a percent of their Field",
       x="Field",
       y="Percent Women",
       caption = "Source: Economist | Graphic: @Mathl3t3")
#The above plot is messy. It does show which fields have more female researchers being published
#but there is too much going on to gain any real insight.

#mutate date to give percent of field from each country
field_by_country <- women_research %>% 
  mutate(total_country = 100*percent_women) %>% 
  group_by(field) %>% 
  mutate(field_percent_by_country = total_country/sum(total_country))

#This plot shows what percent of published researchers come from each country within each field.
ggplot(field_by_country,aes(x=country,y=field_percent_by_country,fill=field))+
  geom_col()+
  facet_wrap(~field)+
  theme_bw() %+replace%
  theme(axis.text = element_text(angle=90),
        legend.position="none")+
  labs(title="Published Women Researchers: 2011-2015",
       subtitle="Percent of field by country",
       x="Country",
       y="Percent",
       caption="Source: Economist | Graphic: @Mathl3t3")

#Calculace what percent each field is within each country
country_by_field <- women_research %>% 
  mutate(total_country = 100*percent_women) %>% 
  group_by(country) %>% 
  mutate(country_percent_by_field = total_country/sum(total_country))

#This plot shows percent each field represents within each country. Again, its a little
#messy since there are 12 countries.
ggplot(country_by_field,aes(x=field,y=country_percent_by_field,fill=country))+
  geom_col()+
  facet_wrap(~country)+
  theme_bw() %+replace%
  theme(axis.text = element_text(angle=90),
        legend.position="none")+
  labs(title="Published Women Researchers: 2011-2015",
       subtitle="Percent of country by field",
       x="Field",
       y="Percent",
       caption="Source: Economist | Graphic: @Mathl3t3")
