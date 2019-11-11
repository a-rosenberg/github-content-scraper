library(tidyverse)

partidos_fifa_copa_mundial_procesado <- readr::read_delim("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-10/partidos.txt",delim = "\t")

partidos <- partidos_fifa_copa_mundial_procesado

head(partidos)

## Organización de los datos

partidos <- partidos %>% 
  mutate(partido_orden = as.numeric(gsub("[()]", "", partido_orden)))

## Grafico exploratorio de cantidad de goles por Copa

partidos %>% 
  group_by(anio) %>% 
  summarize(num_partidos = n(), goles = sum(equipo_1_final,equipo_2_final), rate = goles/num_partidos) %>% 
  ggplot(aes(anio, rate)) +
  geom_col() + geom_smooth()

## Cantidad de partidos ganados en Mundiales por País
  
partidos %>% 
  filter(equipo_1_final != equipo_2_final) %>% 
  mutate(ganador = if_else(equipo_1_final > equipo_2_final, equipo_1, equipo_2)) %>% 
  count(ganador, sort = TRUE) %>% 
  ggplot(aes(fct_reorder(ganador, n), n, fill = ganador, label = n)) + 
  geom_col() +
  geom_text(hjust = -0.2) +
  coord_flip() +
  theme_minimal() + 
  theme(legend.position = "none") +
  labs(title = "Cantidad de partidos ganados en Mundiales por País",
       subtitle = "DatosDeMiércoles",
       caption = "Fuente: Open Public Domain Football Data",
       x = "País",
       y = "Cantidad de partidos ganados") 

## País con mayor cantidad de partidos ganados por Copa Mundial

partidos %>% 
  filter(equipo_1_final != equipo_2_final) %>% 
  mutate(ganador = if_else(equipo_1_final > equipo_2_final, equipo_1, equipo_2)) %>% 
  count(anio, ganador, sort = TRUE) %>% 
  group_by(anio) %>% 
  mutate(posicion = rank(-n, ties.method = "first")) %>% 
  ungroup() %>% 
  filter(posicion <= 1) %>% 
  ggplot(aes(x = as_factor(anio), y = n, col = ganador, label = ganador)) +
  geom_point() +
  geom_text(angle = 45, vjust = 1) +
  theme_minimal() + 
  theme(legend.position = "none") +
  labs(title = "País con mayor cantidad de partidos ganados por Copa Mundial",
       subtitle = "DatosDeMiércoles",
       caption = "Fuente: Open Public Domain Football Data",
       x = "Copa Mundial",
       y = "Cantidad de partidos ganados") 

## Identificando el partido final de cada Copa

max_partidos <- partidos %>% 
  group_by(anio) %>% 
  summarize(max_partido = max(partido_orden))

## Definiendo el ganar de cada partido, este si incluye el Empate

ganador_partido <- partidos %>% 
  mutate(ganador = case_when(
    equipo_1_final > equipo_2_final ~ equipo_1,
    equipo_1_final < equipo_2_final ~ equipo_2,
    TRUE ~ "Empate")) 

## Seleccionando el ganador del último partido de cada copa con su número de partidos ganados

campeon <- ganador_partido %>% 
  semi_join(max_partidos, by = c("partido_orden" = "max_partido", "anio" = "anio")) %>% 
  select(anio, ganador)

## Gráfico del País campeon con relación a la cantidad de partidos ganados por Copa Mundial

ganador_partido %>% 
  count(anio, ganador, sort = TRUE) %>% 
  right_join(campeon, by = c("anio", "ganador")) %>% 
  filter(ganador != "Empate") %>% 
  ggplot(aes(x = as_factor(anio), y = n, col = ganador, label = ganador)) +
  geom_point() +
  geom_text(angle = 45, vjust = 1) +
  theme_minimal() + 
  theme(legend.position = "none") +
  labs(title = "País campeon con relación a la cantidad de partidos ganados por Copa Mundial",
       subtitle = "DatosDeMiércoles",
       caption = "Fuente: Open Public Domain Football Data",
       x = "Copa Mundial",
       y = "Cantidad de partidos ganados") 
  
