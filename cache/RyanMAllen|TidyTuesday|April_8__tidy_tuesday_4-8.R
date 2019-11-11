library(dplyr)

library(ggplot2)

library(data.table)



player_dob <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/player_dob.csv")



grand_slams <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slams.csv")



grand_slam_timeline <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slam_timeline.csv")





g <- grand_slams %>% 
    
    select(name,gender,rolling_win_count) %>%
    
    group_by(name, gender) %>% 
    
    summarise(rolling_win_count = sum(rolling_win_count))



males <- as.data.table(filter(g, gender == "Male") %>% arrange(desc(rolling_win_count)) %>% top_n(n=9))

top_10m <- slice(males, 1:10)





females <- as.data.table(filter(g, gender == "Female") %>% arrange(desc(rolling_win_count)))

top_10f <- slice(females, 1:10)





top_10m$name <- factor(top_10m$name, levels = top_10m$name[order(-top_10m$rolling_win_count)])



chart <- ggplot(top_10m, aes(x=name, y= rolling_win_count))

chart + geom_bar(stat='identity') + theme(panel.grid.major = element_blank(), axis.ticks = element_blank(), panel.grid.minor = element_blank(),
                                          
                                          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    
    geom_text(aes(label=rolling_win_count), vjust=-0.5) + ggtitle("Top 10 Male Tennis Players")



top_10f$name <- factor(top_10f$name, levels = top_10f$name[order(-top_10f$rolling_win_count)])



chart1 <- ggplot(top_10f, aes(x=name, y= rolling_win_count))

chart1 + geom_bar(stat='identity') + theme(panel.grid.major = element_blank(), axis.ticks = element_blank(), panel.grid.minor = element_blank(),
                                           
                                           panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    
    geom_text(aes(label=rolling_win_count), vjust=-0.5) + ggtitle("Top 10 Female Tennis Players")







https://stackoverflow.com/questions/21349329/drawing-a-barchart-to-compare-two-sets-of-data-using-ggplot2-package
