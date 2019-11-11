#libraries
library(tidyverse)
library(igraph)
library(ggplot2)

#load data
nobel_winner_all_pubs <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winner_all_pubs.csv")

#physics
df1<- nobel_winner_all_pubs %>%
  filter(category=="physics") %>%
  select(paper_id, laureate_id)

#make into matrix
pubs1<- as.data.frame.matrix(table(df1))

#graph incidence
pubs_inc1<- graph.incidence(pubs1)

#make bipartite graph
pubs_pr1<- bipartite.projection(pubs_inc1)

#and save just collaborations (authors)
collab_phy<- pubs_pr1$proj2


#chemistry
df2<- nobel_winner_all_pubs %>%
  filter(category=="chemistry") %>%
  select(paper_id, laureate_id)

#make into matrix
pubs2<- as.data.frame.matrix(table(df2))

#graph incidence
pubs_inc2<- graph.incidence(pubs2)

#make bipartite graph
pubs_pr2<- bipartite.projection(pubs_inc2)

#and save just collaborations (authors)
collab_chem<- pubs_pr2$proj2


#medicine
df3<- nobel_winner_all_pubs %>%
  filter(category=="medicine") %>%
  select(paper_id, laureate_id)

#make into matrix
pubs3<- as.data.frame.matrix(table(df3))

#graph incidence
pubs_inc3<- graph.incidence(pubs3)

#make bipartite graph
pubs_pr3<- bipartite.projection(pubs_inc3)

#and save just collaborations (authors)
collab_med<- pubs_pr3$proj2


#plot all three and include density for each
op<- par(mfrow=c(1,3))
plot(collab_phy, vertex.label=NA, vertex.size=degree(collab_phy)/1.5, vertex.color="darkred", main="Physics", sub=paste0("Density= ", round(graph.density(collab_phy), 4)))
plot(collab_chem, vertex.label=NA, vertex.size=degree(collab_chem)/1.5, vertex.color="lightblue",main="Chemistry", sub=paste0("Density= ", round(graph.density(collab_chem), 4)))
plot(collab_med, vertex.label=NA, vertex.size=degree(collab_med)/1.5, vertex.color="salmon", main="Medicine", sub=paste0("Density= ", round(graph.density(collab_med), 4)))
par(op)

#plot separately for clearer images

plot(collab_phy, vertex.label=NA, vertex.size=degree(collab_phy)/1.5, vertex.color="darkred", main="Physics", sub=paste0("Density= ", round(graph.density(collab_phy), 4)))

plot(collab_chem, vertex.label=NA, vertex.size=degree(collab_chem)/1.5, vertex.color="lightblue",main="Chemistry", sub=paste0("Density= ", round(graph.density(collab_chem), 4)))

plot(collab_med, vertex.label=NA, vertex.size=degree(collab_med)/1.5, vertex.color="salmon", main="Medicine", sub=paste0("Density= ", round(graph.density(collab_med), 4)))


#create df of the 3 network attributes
att.df <- data.frame(
  category=c("Physics", "Chemistry", "Medicine"),
  density=round(c(graph.density(collab_phy), graph.density(collab_chem), graph.density(collab_med)), 4),
  net_size=c(length(V(collab_phy)$name), length(V(collab_chem)$name), length(V(collab_med)$name)),
  ties=c(length(E(collab_phy)$weight), length(E(collab_chem)$weight), length(E(collab_med)$weight)),
  avg_deg=round(c(mean(degree(collab_phy)), mean(degree(collab_chem)), mean(degree(collab_med))), 2),
  deg_cent=round(c(centralization.degree(collab_phy)$centralization, centralization.degree(collab_chem)$centralization, centralization.degree(collab_med)$centralization), 4)
)
att.df

#Since networks are very similar- this is boring, but oh well
#bar graph of network degree centralization
g<-ggplot(data = att.df, aes(x = category, y = deg_cent, fill=category)) +
  geom_bar(stat="identity") + theme_minimal()
g+labs(title="Centralization (degree) by Nobel Prize Publication Collaboration Network", 
       x="Category of Publication Network", y = "Degree Centralization")
ggsave("boring.png")