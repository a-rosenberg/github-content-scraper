library(tidyverse)
library(magrittr)
library(lubridate)
library(ggpubr)


student_ratio <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-07/student_teacher_ratio.csv")

student_ratio$country <- str_replace(student_ratio$country, pattern = "United Kingdom of Great Britain", replacement = "UK")

# First Chart, grouping by year:

a <- student_ratio %>% 
  group_by(country, indicator, year) %>%
  summarise(Ratio=mean(student_ratio, na.rm = TRUE)) %>%  
  arrange(-Ratio) %>% 
  filter(!is.na(Ratio)) %>% 
  ungroup() %>% 
  top_n(25) %>% 
  ggplot(aes(x=reorder(country, Ratio), y=Ratio, color=indicator))+
    geom_segment(aes(x=reorder(country, Ratio), y= 0, xend=country, yend=Ratio), size=1)+
  geom_point(aes(size=as.factor(year)), alpha=0.3)+
  theme_minimal()+coord_flip()+xlab("")+
  ggpubr::theme_pubr()+
  theme(legend.title = element_blank())


a<-set_palette(a, "nejm")

b <- student_ratio %>% 
  group_by(country, indicator, year) %>%
  summarise(Ratio=mean(student_ratio, na.rm = TRUE)) %>%  
  arrange(-Ratio) %>% 
  filter(!is.na(Ratio)) %>% 
  ungroup() %>% 
  top_n(-25) %>% 
  ggplot(aes(x=reorder(country, Ratio), y=Ratio, color=indicator))+
  geom_segment(aes(x=reorder(country, Ratio), y= 0, xend=country, yend=Ratio), size=1)+
  geom_point(aes(size=as.factor(year)), alpha=0.3)+
  theme_minimal()+coord_flip()+xlab("")+
  ggpubr::theme_pubr()+
  theme(legend.title = element_blank())

b <- set_palette(b, "nejm")


arranged<-ggarrange(a, b, common.legend = TRUE, legend = "bottom")

chartByYear<-annotate_figure(arranged,
                top = text_grob("The 25 highest and lowest Student/Teacher Ratios\n by countries and year", face = "bold", size = 14))

chartByYear
#Second Chart, not grouping by year

a <- student_ratio %>% 
  group_by(country, indicator) %>%
  summarise(Ratio=mean(student_ratio, na.rm = TRUE)) %>%  
  arrange(-Ratio) %>% 
  filter(!is.na(Ratio)) %>% 
  ungroup() %>% 
  top_n(25) %>% 
  ggplot(aes(x=reorder(country, Ratio), y=Ratio, color=indicator))+
  geom_segment(aes(x=reorder(country, Ratio), y= 0, xend=country, yend=Ratio), size=1)+
  geom_point(alpha=0.3, size=3)+
  theme_minimal()+coord_flip()+xlab("")+
  ggpubr::theme_pubr()+
  theme(legend.title = element_blank())


a<-set_palette(a, "nejm")

b <- student_ratio %>% 
  group_by(country, indicator) %>%
  summarise(Ratio=mean(student_ratio, na.rm = TRUE)) %>%  
  arrange(-Ratio) %>% 
  filter(!is.na(Ratio)) %>% 
  ungroup() %>% 
  top_n(-25) %>% 
  ggplot(aes(x=reorder(country, Ratio), y=Ratio, color=indicator))+
  geom_segment(aes(x=reorder(country, Ratio), y= 0, xend=country, yend=Ratio), size=1)+
  geom_point( alpha=0.3, size=3)+
  theme_minimal()+coord_flip()+xlab("")+
  ggpubr::theme_pubr()+
  theme(legend.title = element_blank())

b <- set_palette(b, "nejm")


arranged<-ggarrange(a, b, common.legend = TRUE, legend = "bottom")

chartWOYear<-annotate_figure(arranged,
                             top = text_grob("The 25 highest and lowest Student/Teacher Ratios\n by countries", face = "bold", size = 14))

chartWOYear
