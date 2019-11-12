## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)

apps <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-07-24/apps_googleplaystore.csv")

glimpse(apps)



## ------------------------------------------------------------------------

apps %>% 
  count(categoria, sort = TRUE) %>% 
  mutate(categoria = fct_reorder(categoria, n)) %>% 
  ggplot(aes(x = categoria, y = n, label = n)) + 
  geom_col() + 
  geom_text(hjust = 1, col = "white") +
  coord_flip() + 
  labs(title = "Cantidad de aplicaciones por Categoría en Play Store",
       x = "Categoría",
       y = "Frecuencia absoluta",
       caption = "Fuente de datos: Kaggle")

perc_redondeado <- function(x){
    paste(round(x, 3)*100, "%", sep = "")
}


apps %>% 
  count(categoria, sort = TRUE) %>% 
  mutate(perc = n / sum(n),
         perc_acum = cumsum(perc),
         categoria = fct_reorder(categoria, -n)) %>% 
  ggplot(aes(categoria, perc)) + 
  geom_col() + 
  geom_point(aes(x = categoria, y = perc_acum, group = 1)) +
  geom_line(aes(x = categoria, y = perc_acum, group = 1)) + 
  geom_text(aes(x = categoria, y = perc_acum, group = 1, label = perc_redondeado(perc_acum)),
            vjust = -1, angle = 30, size = 3.5) + 
  theme_light() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title = "Pareto de aplicaciones por Categoría en Play Store",
       x = "Categoría",
       y = "Frecuencia relativa",
       caption = "Fuente de datos: Kaggle")
  



