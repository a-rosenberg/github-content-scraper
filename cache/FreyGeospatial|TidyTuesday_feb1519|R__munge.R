# Author: Jordan Frey
# Created: 2/15/2019
# Code free to distribute and use as a reference
# Please Visit www.FreyGeospatial.com

######################################
#LOAD PACKAGE LIBRARIES

library(tidyverse)
library(gridExtra)


##############################3
#READ IN DATA

fed_rd <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/fed_r_d_spending.csv")
#energy_spend <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/energy_spending.csv")
#climate_spend <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/climate_spending.csv")

########################################

#color blind friendly pallette
cbPalette <- c("#000000", 
                "#E69F00", 
                "#56B4E9", 
                "#009E73", 
                "#F0E442", 
                "#0072B2", 
                "#D55E00", 
                "#CC79A7",
                "#A9A9A9",
                "#ADD8E6")

#EPA RD Budget
epa_rd_budget<- fed_rd %>% 
  filter(department == "EPA") %>% 
  ggplot(aes(x=year))+
  geom_line(aes(y=rd_budget/1000000000), size=1)+
  ggtitle("EPA Inflation Adjusted R&D Budget")+
  scale_y_continuous(labels = scales::dollar_format())+
  labs(y="R&D Budget (Billions)",
       x="Year")+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

epa_rd_budget


#RD ALL DEPTS
rd_gdp_all<- fed_rd %>% 
  ggplot(aes(x=year, y=rd_budget/gdp, color=department))+
  geom_line(size=0.5)+
  labs(x = "Year",
       y = "Percent R&D to GDP",
       subtitle = "As Percent of GDP",
       color = "Department")+
  ggtitle("US Government R&D Spending for All Departments")+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))


#RD SELECT DEPTS

#distinct department names
fed_rd %>% distinct(department)


department_subset <- fed_rd %>% 
  filter(department == "EPA" | department == "DOC" |
           department == "DOE" | department == "NSF" |
           department == "USDA" | department == "NIH" | 
           department == "Interior" | department == "DOT" | 
          department == "NASA" | department == "VA") %>% 
  ggplot(aes(x=year, y=rd_budget/gdp, color=department))+
  geom_line(size=1)+
  labs(x = "Year",
       y = "Percent R&D to GDP",
       subtitle = "As Percent of GDP")+
  ggtitle("US Government R&D Spending of Select Departments")+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
  scale_colour_manual(values=cbPalette, name = "Department")

grid.arrange(rd_gdp_all, department_subset)




