#tidytuesday 08012019
library(tidyverse)

importfile<-"tidytues2019/tt_jan19/tt_08012019/IMDb_Economist_tv_ratings.csv"
exportfile<-"tidytues2019/tt_jan19/tt_08012019/08012019.pdf"
  
tvdata <- read_csv(importfile)
tvdata

ggplot(data = tvdata) +
  geom_point(mapping = aes(x=date,y=av_rating))
    
ggsave(exportfile)
