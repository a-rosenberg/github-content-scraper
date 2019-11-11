#loading the tidyverse package
library(tidyverse) 
#Importing the dataset
media_franchises <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-02/media_franchises.csv")

#Variation of book revenues by Year 
ggplot(media_franchises, aes(x = year_created, y = revenue)) +
  geom_line(stat = "identity", color = "red") +
geom_point() + 
  labs(title = "How revenues have varied over the years", y = "Revenue($)", x = "Year", caption = "Source: Wikipedia") 


#How costs have varied for each of the revenue categories
my_plot <- ggplot(data = media_franchises) +
  geom_smooth(mapping = aes(x = year_created, y = revenue, color = revenue_category)) +
  labs(title = "How revenues have varied over the years by category", y = "Revenue($)", x = "Year", caption = "Source: Wikipedia") 
  
# Out putting the graph with limits on the x and y axes
my_plot + coord_cartesian(xlim =c(1924, 2013), ylim = c(0, 85))

#+
 # labs(title = "How revenues have varied over the years", y = "Revenue ($)", x = "Year", caption = "Source: Drowning Project Data") 
  









