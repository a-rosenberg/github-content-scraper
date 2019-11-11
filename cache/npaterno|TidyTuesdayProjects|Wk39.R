# Load libraries
library(tidyverse)
library(ggalt)
library(ggthemes)

# Load data
school_diversity <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-24/school_diversity.csv")


# Focus on Washington state, add non-white category 
wa_schools<-school_diversity%>%
  filter(ST=="WA")%>%
  mutate(Multi = replace_na(Multi, 0),
    non_white = AIAN + Asian + Black + Hispanic + Multi)%>%
  rename(Native = AIAN)

# Calculate mean proportions for each local
wa_schools_locale<-wa_schools%>%
  filter(!is.na(d_Locale_Txt))%>%
  separate(d_Locale_Txt, into = c("locale_general", "local_specific"), sep = "-")%>%
  group_by(locale_general, SCHOOL_YEAR)%>%
  select(locale_general, SCHOOL_YEAR, Native, Asian, Black, Hispanic, White, Multi)%>%
  summarize_all(mean)%>%
  pivot_longer(-c(locale_general, SCHOOL_YEAR), names_to = "Ethnicity" , values_to = "count")%>%
  pivot_wider(names_from = SCHOOL_YEAR, values_from = count)

# Capitalize first letter of locale_general variable
wa_schools_locale$locale_general<-str_to_title(wa_schools_locale$locale_general)

# Plot diversity by school district location
ggplot(wa_schools_locale)+
  geom_dumbbell(aes(x=`1994-1995`, xend=`2016-2017`, y = Ethnicity), 
                size = 3,
                color = "grey60",
                colour_x = "#66CC99", 
                colour_xend = "#cc6666")+
  facet_wrap(~locale_general)+
  geom_text(data = filter(wa_schools_locale,locale_general== "City" & Ethnicity == "White"),
            aes(x = `1994-1995`, y = Ethnicity),
            label = "1994-1995", fontface = "bold",
            color = "#66cc99",
            vjust = 2)+ 
  geom_text(data = filter(wa_schools_locale,locale_general== "City" & Ethnicity == "White"),
            aes(x = `2016-2017`, y = Ethnicity),
            label = "2016-2017", fontface = "bold",
            color = "#cc6666",
            vjust = 2)+ 
  theme_economist()+
  labs(title = "Ethnic Diversity in Washington Schools:",
       subtitle = "by school district location", 
       x = "Proportion",
       y = "Ethnicity",
       caption = "Source: National Center for Education Statistics | Graph: @Mathl3t3")




