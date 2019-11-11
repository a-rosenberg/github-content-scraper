# Load Libraries
library(tidyverse)
library(ggthemes)

# Load data
ipf_lifts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-08/ipf_lifts.csv")

# Filter Men's Data
men<-ipf_lifts%>%
  filter(sex == "M")

#Tidy Round 1
mavg<-men%>%
  select(-c("event","federation","age_class","division","equipment", "meet_name"))%>%
  separate("date", 
           into = c("year","month","day"), 
           sep = "-", 
           convert = TRUE)%>%
  mutate(decade = floor(year/10)*10)


# Tidy Round 2
mavg2<-mavg%>%
  group_by(name)%>%
  summarize(Squat = mean(best3squat_kg),
            Bench = mean(best3bench_kg),
            Deadlift = mean(best3deadlift_kg))%>%
  pivot_longer(cols = c("Squat", "Bench", "Deadlift"), 
               names_to = "lift_type", 
               values_to = "lift_weight")

# Isolate top 10 bench per year
mbench<-mavg%>%
  arrange(desc(best3bench_kg))%>%
  group_by(year)%>%
  top_n(10, best3bench_kg)

# Graph top 10 bench per year
ggplot(mbench, aes(year, best3bench_kg, color = age))+
  geom_point(na.rm=TRUE, 
             size = 3)+
  coord_flip()+
  scale_color_continuous_tableau(palette = "Green")+
  theme_economist()+
  labs(title = "Top 10 Male Bench Press",
       subtitle = "by year",
       x = "Year",
       y = "Weight (kg)", 
       color = "Age",
       caption = "Source: openpowerlifting.org | Graph: @Mathl3t3")

# Histogram for average lifts by lifter
ggplot(mavg2%>%group_by(lift_type), aes(lift_weight))+
  geom_histogram(binwidth = 20, 
                 color = "white", 
                 fill = "springgreen", 
                 na.rm = TRUE)+
  facet_wrap(~lift_type)+
  scale_x_continuous(limits = c(0,450))+
  theme_economist()+
  labs(title = "Distribution of Average Lift Weight",
       subtitle = "Per Male Competitor",
       x = "Weight", 
       y = "Number of Competitors",
       caption = "Source: openpowerlifting.org | Graph: @Mathl3t3")

# Repeat Analysis for Women

# Filter Women's data
women<-ipf_lifts%>%
  filter(sex == "F")

#Tidy Round 1
wavg<-women%>%
  select(-c("event","federation","age_class","division","equipment", "meet_name"))%>%
  separate("date", 
           into = c("year","month","day"), 
           sep = "-", 
           convert = TRUE)%>%
  mutate(decade = floor(year/10)*10)


# Tidy Round 2
wavg2<-wavg%>%
  group_by(name)%>%
  summarize(Squat = mean(best3squat_kg),
            Bench = mean(best3bench_kg),
            Deadlift = mean(best3deadlift_kg))%>%
  pivot_longer(cols = c("Squat", "Bench", "Deadlift"), 
               names_to = "lift_type", 
               values_to = "lift_weight")

# Isolate top 10 women's bench per year
wbench<-wavg%>%
  arrange(desc(best3bench_kg))%>%
  group_by(year)%>%
  top_n(10, best3bench_kg)

# Graph top 10 women's bench per year
ggplot(wbench, aes(year, best3bench_kg, color = age))+
  geom_point(na.rm=TRUE, 
             size = 3)+
  coord_flip()+
  scale_color_continuous_tableau(palette = "Red")+
  theme_economist()+
  labs(title = "Top 10 Bench Press",
       subtitle = "by year",
       x = "Year",
       y = "Weight (kg)", 
       color = "Age",
       caption = "Source: openpowerlifting.org | Graph: @Mathl3t3")

# Histogram for average lifts by lifter
ggplot(wavg2%>%group_by(lift_type), aes(lift_weight))+
  geom_histogram(binwidth = 20, 
                 color = "grey60", 
                 fill = "cyan", 
                 na.rm = TRUE, 
                 alpha = 0.6)+
  facet_wrap(~lift_type)+
  scale_x_continuous(limits = c(0,270))+
  theme_economist()+
  theme(panel.background = element_rect(fill = "grey60",
                                        colour = "lightblue",
                                        size = 0.5, 
                                        linetype = "solid"))+
  labs(title = "Distribution of Average Lift Weight",
       subtitle = "Per Female Competitor",
       x = "Weight", 
       y = "Number of Competitors",
       caption = "Source: openpowerlifting.org | Graph: @Mathl3t3")
