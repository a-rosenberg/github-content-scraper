# libraries ---------------------------------------------------------------

library(tidyverse)
library(janitor)
library(showtext)

# import ------------------------------------------------------------------

url    <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-19/phd_by_field.csv"

df_or  <- read_csv(url) %>% clean_names()


# pre-plot ----------------------------------------------------------------

df_or %>% summary()

df_or$major_field %>% table()

df_or$broad_field %>% table()

df <- df_or %>% 
  mutate(year = as.factor(year)) %>% 
  mutate(broad_field = case_when(
    str_detect(broad_field,"Mathema") == T ~ "Math/C.S",
    str_detect(broad_field,"Psycholog") == T ~ "Social",
    str_detect(broad_field,"Humanities") == T ~ "Humanities",
    T ~ broad_field
  )) %>% 
  group_by(broad_field, year) %>% 
  summarise(n = sum(n_phds,na.rm = T)) %>% 
  group_by(year) %>% 
  mutate(N = sum(n,na.rm = T),
         p = round(100*n/N,2) ) %>% 
  mutate(broad_field = fct_reorder(broad_field, p) %>% fct_rev()) %>%  
  mutate(broad_field = fct_relevel(broad_field, "Other", after = Inf))  
  

df %>% summary()

# plot --------------------------------------------------------------------

df %>% 
  ggplot(aes( x= year, p ))+
  geom_col(aes(fill = broad_field))+
  #coord_flip()+
  scale_y_continuous(expand = c(0,0))+
  scale_x_discrete(expand = c(0,0))+
  theme_bw(18)+
  labs(y = "%",
       x = "Year",
       fill = "Field:",
       title = "% of PhD awarded by field | 2008-2017",
       subtitle = "TidyTuesday - Week 8: US PhD's Awarded",
       caption = "graphic: H0Vinicius | source: NSF.")+
  scale_fill_brewer(palette = "Set1")


df %>% 
  ggplot(aes(year, broad_field ))+
  geom_tile(aes(fill = p), col = "black")+
  scale_y_discrete(expand = c(0,0))+
  scale_x_discrete(expand = c(0,0))+
  theme_bw(18)+
  theme(legend.key.height = unit(1.25,"cm"))+
  labs(fill = "%",
       x = "Year",
       y = "Field",
       title = "% of PhD awarded by field | 2008-2017",
       subtitle = "TidyTuesday - Week 8: US PhD's Awarded",
       caption = "graphic: H0Vinicius | source: NSF.")+
  scale_fill_gradient2(low = "yellow",
                       mid = "darkorange1",
                       high = "firebrick1",
                       midpoint = 21,
                       limits  = c(0,42))+
  geom_text(aes(label = p), fontface = "bold")
  
