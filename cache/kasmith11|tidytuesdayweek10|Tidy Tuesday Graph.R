#loads in data from github
earnings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/earnings_female.csv") 

#loads in tidyverse which contains ggplot2
library(tidyverse)

#creates a line plot
ggplot(earnings, mapping = aes(x = Year, y = percent, color = group)) +
  geom_line() +
  ggtitle("Women Salary Percent of Men Salary")
