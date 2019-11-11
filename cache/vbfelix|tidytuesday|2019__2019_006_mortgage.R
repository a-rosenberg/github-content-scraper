# libraries ---------------------------------------------------------------

library(tidyverse)
library(janitor)
library(fiftystater)
library(gganimate)
library(magrittr)
library(grid)
library(gridExtra)

# import ------------------------------------------------------------------

url    <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-05/state_hpi.csv"

df_or  <- read_csv(url) %>% filter(year > 2013)

abbr   <- read.table(col.names = c("name","state"),
                     text = "Alabama - AL
                     Alaska - AK
                     Arizona - AZ
                     Arkansas - AR
                     California - CA
                     Colorado - CO
                     Connecticut - CT
                     Delaware - DE
                     Florida - FL
                     Georgia - GA
                     Hawaii - HI
                     Idaho - ID
                     Illinois - IL
                     Indiana - IN
                     Iowa - IA
                     Kansas - KS
                     Kentucky - KY
                     Louisiana - LA
                     Maine - ME
                     Maryland - MD
                     Massachusetts - MA
                     Michigan - MI
                     Minnesota - MN
                     Mississippi - MS
                     Missouri - MO
                     Montana - MT
                     Nebraska - NE
                     Nevada - NV
                     New Hampshire - NH
                     New Jersey - NJ
                     New Mexico - NM
                     New York - NY
                     North Carolina - NC
                     North Dakota - ND
                     Ohio - OH
                     Oklahoma - OK
                     Oregon - OR
                     Pennsylvania - PA
                     Rhode Island - RI
                     South Carolina - SC
                     South Dakota - SD
                     Tennessee - TN
                     Texas - TX
                     Utah - UT
                     Vermont - VT
                     Virginia - VA
                     Washington - WA
                     West Virginia - WV
                     Wisconsin - WI
                     Wyoming - WY",sep = "-",
                     stringsAsFactors = F) %>% 
  mutate_all(str_trim)

df_or %>% 
  group_by(state,year) %>% 
  summarise(price_index = mean(price_index)) %>% 
  left_join(abbr) %>% 
  right_join(fifty_states %>%  mutate(name = str_to_title(id))) -> df_map

# plot --------------------------------------------------------------------
df_map %>% 
  filter(is.na(year)==F) %$%
  summary(price_index)

df_map %>% 
  filter(is.na(year)==F) %>% 
  ggplot(aes(long,lat,fill = ))+
  geom_polygon(aes(group = group, fill= price_index),col = "black")+
  coord_map()+
  theme_void(18)+
  theme(legend.key.height = unit(1.5,"cm"),
        plot.margin = unit(c(.5,.5,.5,.5),"cm"),
        plot.caption = element_text(hjust = 0))+
  labs(fill = "Price\nIndex")+
  scale_fill_viridis_c(option = "B",limits = c(95,280),
                       breaks = seq(95,280, by = 30))+
  labs(title = "TidyTuesday - Week 6 : House and Mortgage",
       subtitle = "",
       caption = "graphic: H0Vinicius | source: Freddie Mac.")+
  facet_wrap(.~year, ncol = 3) -> p1

df_or %>% 
  group_by(year) %>% 
  summarise(us_avg = mean(us_avg)) %>% 
  ggplot(aes(year, us_avg)) +
  theme_bw(12) +
  geom_col(aes(fill = us_avg), show.legend = F,col = "black")+
  geom_text(aes(label = us_avg %>% round(2), y = us_avg*.85),fontface = "bold",size = 5)+
  scale_x_continuous(breaks = 2014:2018)+
  scale_fill_viridis_c(option = "B",limits = c(95,280),
                       breaks = seq(95,280, by = 30))+
  scale_y_continuous(expand = c(0,0), breaks = seq(0,200,by = 50), limits =c(0,200))+
  labs(x = "",y = "",
       title = "Price Index - Averaged at national level") ->p2

x11()

grid1 <- viewport(width = 1, height = 1, x = 0.5, y = 0.5) 
grid2 <- viewport(width = 0.25, 
                  height = .33,
                  x = 0.77, y = .3) 

grid.newpage()
print(p1, vp = grid1)
print(p2, vp = grid2)
