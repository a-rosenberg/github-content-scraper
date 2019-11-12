## ----echo=FALSE,message=FALSE,warning=FALSE------------------------------
library(dplyr)
# library(Rcpp) #; install.packages("Rcpp")
library(skimr) #; install.packages("skimr")
# library(circlepackeR) #; devtools::install_github("jeromefroe/circlepackeR")
# library(data.tree)  #; install.packages("data.tree")

library(igraph)
library(visNetwork)
library(ggplot2)



## ----echo=FALSE,message=FALSE,warning=FALSE------------------------------
small_trains <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-26/small_trains.csv") 

color_palette <- c("blue","green","red")
uniques_graph <- small_trains %>%    
        rename(from=departure_station) %>%
        rename(to=arrival_station) %>% 
        group_by(from,to) %>% 
        summarize(total_delay=sum(avg_delay_all_departing+avg_delay_all_arriving)) %>% 
        # mutate(is_tgv=str_detect(from,"TGV")|str_detect(to,"TGV")) %>% 
        # mutate(color=if_else(is_tgv,'red','blue')) %>%
        mutate(intervals=if_else(total_delay<=2000,"0-2k",
                                 if_else(total_delay>4000,">4k","2k1-4k"))) %>% 
        mutate(color=case_when (
            total_delay<=2000 ~ color_palette[1],
            total_delay>4000 ~ color_palette[3],
            total_delay>2000 & total_delay<=4000 ~ color_palette[2]))
        


hist(uniques_graph$total_delay)
boxplot(uniques_graph$total_delay)



## ----echo=FALSE,message=FALSE,warning=FALSE------------------------------
glimpse(small_trains) 


small_trains %>% 
    count(service)



## ----echo=FALSE,message=FALSE,warning=FALSE------------------------------
skimr::skim(phd_raw) 




## ----echo=FALSE,message=FALSE,warning=FALSE------------------------------
# library(circlepackeR) #; devtools::install_github("jeromefroe/circlepackeR")
# 
# phd_processed <- phd_raw %>% filter(year==2008) %>% select(-year) %>%  
#         mutate(path_string = paste("phds", broad_field, major_field, field, sep = "/"))
# # sample: http://shiny.rstudio.com/gallery/submitbutton-demo.html
# # UI
# 
# # library(data.tree)
# # phd_processed_circler <- data.tree::as.Node(phd_processed)
# 
# 
# phd_processed %>% count(field) %>%  filter(n>1)
# 
# phd_processed %>% filter(field=="Environmental toxicologyc")
# 
# circlepackeR::circlepackeR(phd_raw)
# 
# 
# 
# phd_processed2 <- phd_raw %>% filter(year==2008) %>% select(-year) %>% 
#     mutate(from=) %>% mutate( to="")
# 
# 
# # Libraries
# library(ggraph)
# library(igraph)
# library(tidyverse)
# library(viridis)
#  
# # We need a data frame giving a hierarchical structure. Let's consider the flare dataset:
# edges=flare$edges
# vertices = flare$vertices
# mygraph <- graph_from_data_frame( edges, vertices=vertices )
#  
# # Control the size of each circle: (use the size column of the vertices data frame)
# # png("~/Dropbox/R_GG/R_GRAPH/#314_custom_circle_packing1.png", height = 480, width=480)
# ggraph(mygraph, layout = 'circlepack', weight="size") + 
#   geom_node_circle() +
#   theme_void()




## ----echo=FALSE,message=FALSE,warning=FALSE------------------------------
# library(circlepackeR)
# library(data.tree)
# library(treemap)#;install.packages("treemap"); install.packages("httpuv"); install.packages("mime")
# 
# data(GNI2014)
# head(GNI2014)
# 
# GNI2014$pathString <- paste("world", 
#                             GNI2014$continent, 
#                             GNI2014$country, 
#                             sep = "/")
# population <- as.Node(GNI2014)
# 
# circlepackeR(population, size = "population", color_min = "hsl(56,80%,80%)", 
#              color_max = "hsl(341,30%,40%)")



