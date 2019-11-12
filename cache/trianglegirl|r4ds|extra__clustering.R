## ----eval = FALSE--------------------------------------------------------
## library(plotly)
## plot_ly(data = iris, x = Sepal.Length, y = Sepal.Width, z = Petal.Width,
##         color = Species, type = "scatter3d", mode = "markers")


## ---- echo = FALSE-------------------------------------------------------
knitr::include_graphics("images/EDA-plotly.png")


## ---- echo = FALSE-------------------------------------------------------
knitr::include_graphics("images/EDA-hclust.png")


## ------------------------------------------------------------------------
small_iris <- sample_n(iris, 50)
  
iris_hclust <- small_iris %>% 
  select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) %>% 
  dist() %>% 
  hclust(method = "complete")


## ----fig.height = 4------------------------------------------------------
plot(iris_hclust, labels = small_iris$Species)


## ------------------------------------------------------------------------
(clusters <- cutree(iris_hclust, 3))

ggplot(small_iris, aes(x = Sepal.Width, y = Sepal.Length)) +
  geom_point(aes(color = factor(clusters)))


## ---- echo = FALSE-------------------------------------------------------
knitr::include_graphics("images/EDA-linkage.png")


## ----fig.height = 4------------------------------------------------------
small_iris %>% 
  select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) %>% 
  dist() %>% 
  hclust(method = "single") %>% 
  plot(labels = small_iris$Species)


## ---- echo = FALSE-------------------------------------------------------
knitr::include_graphics("images/EDA-kmeans.png")


## ------------------------------------------------------------------------
iris_kmeans <- small_iris %>% 
  select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) %>% 
  kmeans(centers = 3, nstart = 20, iter.max = 50)

iris_kmeans$cluster


## ------------------------------------------------------------------------
ggplot(small_iris, aes(x = Sepal.Width, y = Sepal.Length)) +
  geom_point(aes(color = factor(iris_kmeans$cluster)))

small_iris %>% 
  group_by(iris_kmeans$cluster) %>% 
  summarise(n_obs = n(), avg_width = mean(Sepal.Width), avg_length = mean(Sepal.Length))

