
# Packages ----------------------------------------------------------------

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggmap)
library(fiftystater)
library(grid)

# Import ------------------------------------------------------------------

df_or<-read_xlsx("us_avg_tuition.xlsx")

df_or %>% 
  gather(Year,Value,-State)->df

minus<-function(x){
  c(NA,diff(x))
}
divide<-function(x,y){
  n<-length(x)
  c(NA,x[2:n]/y[1:(n-1)])
}

df %>% 
  group_by(State) %>%
  mutate(Dif = minus(Value)) %>% 
  mutate(Division = 100*divide(Dif,Value))->df

fifty_states %>% 
    left_join(df %>% 
                mutate(id = tolower(State))) %>% 
  filter(is.na(Year)==F) ->df_map


# Plot --------------------------------------------------------------------

df_map %>%
  ggplot(aes(fill = Division,x=long,y=lat))+
  facet_wrap(~Year, ncol= 4)+
  geom_polygon(aes(group = group),col="black")+
  viridis::scale_fill_viridis(breaks=seq(-9,24,by=3),
                              labels=seq(-9,24,by=3),
                              limits=c(-9,24))+
  theme_inset(base_size = 16)+
  theme(legend.position = "bottom",
        legend.key.width = unit(2.5,"cm"),
        plot.margin = unit(c(2,2,2,2), "cm"))+
  labs(fill = "Difference in tuition (%) relative to the previous year")+
  coord_map()->p_all


df_map %>%
  filter(Year == "2004-05") %>% 
  ggplot(aes(fill = Value,x=long,y=lat))+
  facet_wrap(~Year, ncol= 1)+
  geom_polygon(aes(group = group),col="black")+
  viridis::scale_fill_viridis(breaks=seq(3500,13000,by=1500),
                              labels=seq(3500,13000,by=1500),
                              limits=c(3500,12500),option = "A",
                              direction = -1,alpha = .7)+
  theme_inset(base_size = 16)+
  theme(legend.position = "top",
        legend.key.width  = unit(2.5,"cm"))+
  labs(fill = "Average\ntuition ($)")+
  coord_map()->p_2004


# Union -------------------------------------------------------------------

grid1 <- viewport(width = 1, height = 1, x = 0.5, y = 0.5) 
grid2 <- viewport(width = 0.25, 
                 height = .327,
                 x = 0.178, y = .83) 

x11()
grid.newpage()
print(p_all , vp = grid1)
print(p_2004, vp = grid2)


