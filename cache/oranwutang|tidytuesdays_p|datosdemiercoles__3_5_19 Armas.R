library(tidyverse)
library(magrittr)
library(RColorBrewer)
library(ggpubr)
comercio_hispanoamerica_mundo <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-05-01/comercio_hispanoamerica_mundo_agregado.csv")

comercio_hispanoamerica_mundo %>% 
  filter(nombre_comunidad_producto=="Armas") %>% 
  group_by(anio,nombre_pais_origen) %>% 
  summarise(Total=sum(valor_importado_dolares)) %>% 
  ggplot(aes(x=nombre_pais_origen, y=Total/10^6))+
  geom_line(size=2, color="tomato2")+
  geom_point(aes(color=as.factor(anio)),size=4)+
  scale_x_discrete(limits = rev(levels(as.factor(comercio_hispanoamerica_mundo$nombre_pais_origen))))+
  coord_flip()+
  scale_color_brewer(palette = "Greens")+
  theme(panel.background = element_rect(fill="black"),
         panel.grid = element_blank(),
        legend.background = element_rect(fill = "black"),
        legend.key = element_rect(fill = "black", color = NA),
        legend.text = element_text(color="white"),
        legend.title = element_text(color="white"),
        legend.position = c(0.9,0.15),
        axis.title.y = element_blank(),
        axis.title.x = element_text(color="yellow"),
        axis.text = element_text(color="yellow"),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        title = element_text(color="yellowgreen", size=16),
        plot.background = element_rect(fill = "black"))+
  labs(color="Año", 
        title = "Gasto armamentístico en Latinoamérica",
        subtitle = "Gasto en importaciones de armas por años")+
  ylab(label = "Millones de Dólares")
