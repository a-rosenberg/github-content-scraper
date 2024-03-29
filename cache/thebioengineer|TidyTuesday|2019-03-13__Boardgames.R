## ----load_libraries------------------------------------------------------
# devtools::install_github("thebioengineer/tidytuesdayR")
library(tidytuesdayR)
library(tidyverse)
# devtools::install_github("ropensci/plotly") #the dev 
library(plotly)
library(htmlwidgets)

tt_data<-tt_load(2019,week=11)

# tt_data


## ----transform-----------------------------------------------------------


#cluster groupings over the years

three_d_boardgames<-tt_data$board_games %>%
  filter()
  mutate(labs=paste0("<div><p>",name,"</p><img href=\"https:",image,"\"/a></div>")) %>% 
  plot_ly(
    x = ~year_published,
    y = ~min_age,
    z = ~average_rating,
    color = ~ expansion, colors = c('#BF382A', '#0C4B8E'),
    hovertext = labs) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Publish Year'),
                     yaxis = list(title = 'Minimum Recommended Age'),
                     zaxis = list(title = 'Average Rating')),
         title = 'Board Games for All')

three_d_boardgames$sizingPolicy$padding <- "0"

saveWidget(three_d_boardgames,
           "BoardGame_Ratings.html",     
           selfcontained = FALSE,
           libdir = "lib",
           title = "BoardGame Ratings cloud - TidyTuesday March 13, 2019")



