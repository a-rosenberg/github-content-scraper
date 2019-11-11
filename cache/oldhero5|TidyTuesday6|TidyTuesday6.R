#Tidy Tuesday
#05FEB18
library(tidyverse)
library(ggthemes)

#readdata
state_hpi <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-05/state_hpi.csv")
mortgage_rates <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-05/mortgage.csv")
recession_dates <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-05/recessions.csv")

data<- state_hpi %>% filter(year >= 2002 & year < 2008)
data1 <- state_hpi %>% filter(year >= 2008)

data$date <- paste0(data$year,"-",data$month,"-","01")
data1$date <- paste0(data1$year,"-",data1$month,"-","01")
data$date <- as.Date(data$date)
data1$date <- as.Date(data1$date)



PI_plot <- ggplot(data = data, aes(date)) +
    geom_line(aes(y = price_index,color = "State Price Index")) +
    geom_line(aes(y = us_avg,color = "US AVG Price Index"))+ 
  labs(title = "Pre-Recession Housing Price Index  by State",
    x = "Price Index",
    y = "Date",
    color = "Legend") +
  theme_fivethirtyeight() +
  facet_wrap(vars(state))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

PI_plot

PI1_plot <- ggplot(data = data1, aes(date)) +
    geom_rect(aes(xmin = as.Date('2008-01-01'), ymin = -Inf, 
                  xmax = as.Date('2009-06-01'), ymax = Inf),
                  fill = "#c6dbef")+
    geom_line(aes(y = price_index,color = "State Price Index")) +
    geom_line(aes(y = us_avg,color = "US AVG Price  Index"))+
    labs(title = "Post-Recession Housing Price Index by State",
         subtitle = "Shaded Area Represents Recession Period",
         x = "Price Index",
         y = "Date",
         color = "Legend") +
    theme_fivethirtyeight() +
    facet_wrap(vars(state))+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

PI1_plot
#DC is out of control
data_DC<- data %>% filter(state == "DC")
data_DC1 <- data1 %>% filter(state == "DC")

DC_plot <- ggplot(data = data_DC, aes(date)) +
    geom_line(aes(y = price_index,color = "State Price Index")) +
    geom_line(aes(y = us_avg,color = "US AVG Price Index"))+  
    labs(title = "Pre-Recession Housing Price Index in DC",
         x = "Price Index",
         y = "Date",
         color = "Legend") +
    theme_fivethirtyeight()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

DC_plot

DC1_plot <- ggplot(data = data_DC1, aes(date)) +
    geom_rect(aes(xmin = as.Date('2008-01-01'), ymin = -Inf, 
                  xmax = as.Date('2009-06-01'), ymax = Inf),
              fill = "#c6dbef")+
    geom_line(aes(y = price_index,color = "State Price Index")) +
    geom_line(aes(y = us_avg,color = "US AVG Price Index"))+
    labs(title = "Post-Recession Housing Price Index in DC",
         subtitle = "Shaded Area Represents Recession Period",
         x = "Price Index",
         y = "Date",
         color = "Legend") +
    theme_fivethirtyeight()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

DC1_plot