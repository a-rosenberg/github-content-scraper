## ----load_libraries------------------------------------------------------
# devtools::install_github("thebioengineer/tidytuesdayR")
library(tidytuesdayR)
library(tidyverse)
library(plotly)
library(htmlwidgets)

tt_data<-tt_load(2019,week=10)
tt_data


## ----transform-----------------------------------------------------------


#cluster groupings over the years

lp<-tt_data$employed_gender%>%
  plot_ly(  x = ~year, y = ~full_time_female, name = 'Full Time Female', type = 'scatter', mode = 'lines+marker', line=list(color ="orange")) %>%
  add_trace(x = ~year, y = ~part_time_female, name = 'Part Time Female', mode = 'lines+marker', line=list(color ="orange",  dash = 'dash')) %>%
  add_trace(x = ~year, y = ~full_time_male,   name = 'Full Time Male',   mode = 'lines+marker', line=list(color ="green")) %>%
  add_trace(x = ~year, y = ~part_time_male,   name = 'Part Time Male',   mode = 'lines+marker', line=list(color ="green",  dash = 'dash')) %>% 
  add_trace(x = ~year, y = ~total_full_time,  name = 'Total Full Time',  mode = 'lines+marker', line=list(color ="black")) %>%
  add_trace(x = ~year, y = ~total_part_time,  name = 'Total Part Time',  mode = 'lines+marker', line=list(color ="black",  dash = 'dash')) %>% 
  layout(title = 'Gender Employment Over time',
         xaxis =list(title = 'Year') ,
         yaxis = list(title = 'Percent Employed as Full or Part Time' ))%>%
  layout(showlegend = FALSE)

lp$sizingPolicy$padding <- "0"

saveWidget(lp,
           "Percent_Employed_by_Gender.html",     
           selfcontained = FALSE,
           libdir = "lib",
           title = "Employment Types by Gender")



