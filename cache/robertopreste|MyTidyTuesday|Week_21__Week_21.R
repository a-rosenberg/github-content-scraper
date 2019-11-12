## ---- results='hide', message=FALSE, warning=FALSE-----------------------
library(tidyverse)
library(funModeling)
library(gridExtra)


## ---- results='hide'-----------------------------------------------------
df = read_csv("data/calfire_frap.csv", skip = 1, 
              col_names = c("id", "objectid", "year", "state", "agency", "unit_id", "fire_name", "inc_num", 
                            "alarm_date", "cont_date", "cause", "comments", "report_ac", "gis_acres", "c_method", 
                            "objective", "fire_num", "shape_length", "shape_area", "fire_cause", "plot_date"))


## ------------------------------------------------------------------------
head(df)


## ------------------------------------------------------------------------
df %>% df_status()


## ------------------------------------------------------------------------
fire_per_years <- df %>% 
    group_by(year, fire_cause) %>% 
    summarise(fires = n(), 
              tot_area_burnt = sum(shape_area), 
              mean_area_burnt = mean(shape_area))


## ---- fig.height=4-------------------------------------------------------
line_1 <- fire_type_years %>% 
    ggplot(aes(x = year, y = fires, colour = fire_cause)) + 
    geom_line()
smooth_1 <- fire_type_years %>% 
    ggplot(aes(x = year, y = fires, colour = fire_cause)) + 
    geom_smooth()
grid.arrange(line_1, smooth_1, nrow = 2)


## ---- fig.width=4--------------------------------------------------------
fire_type_years %>% 
    ggplot(aes(x = year, y = fires)) + 
    geom_col(aes(fill = fire_cause), position = "dodge") +
    geom_smooth(aes(color = fire_cause)) + 
    facet_grid(~ fire_cause) + 
    guides(fill = FALSE, color = FALSE)


## ------------------------------------------------------------------------
fire_area_years <- df %>% 
    group_by(year, fire_cause) %>% 
    summarise(tot_area_burnt = sum(shape_area), 
              mean_area_burnt = mean(shape_area))


## ---- fig.height=4-------------------------------------------------------
line_2 <- fire_area_years %>% 
    ggplot(aes(x = year, y = tot_area_burnt, color = fire_cause)) + 
    geom_line()
smooth_2 <- fire_area_years %>% 
    ggplot(aes(x = year, y = tot_area_burnt, color = fire_cause)) + 
    geom_smooth()
grid.arrange(line_2, smooth_2, nrow = 2)


## ---- fig.height=4-------------------------------------------------------
line_3 <- fire_area_years %>% 
    ggplot(aes(x = year, y = mean_area_burnt, colour = fire_cause)) + 
    geom_line()
smooth_3 <- fire_area_years %>% 
    ggplot(aes(x = year, y = mean_area_burnt, colour = fire_cause)) + 
    geom_smooth()
grid.arrange(line_3, smooth_3, nrow = 2)


## ------------------------------------------------------------------------
biscuit_area <- df %>% 
    filter(fire_name == "BISCUIT", year == 2002) %>% 
    select(shape_area)
others_area <- df %>% 
    filter(fire_name != "BISCUIT", fire_name != "RUSH", year == 2002) %>% 
    summarise(tot = sum(shape_area))

area_comp <- tibble(fire_name = c("Biscuit", "Others"), 
                    area_burnt = c(biscuit_area[[1]], others_area[[1]]))


## ------------------------------------------------------------------------
area_comp %>% 
    ggplot(aes(x = fire_name, y = area_burnt, fill = fire_name)) + 
    geom_col()


## ------------------------------------------------------------------------
fire_area_years %>% filter(year == 2012)


## ------------------------------------------------------------------------
df %>% filter(year == 2012) %>% arrange(desc(shape_area))


## ------------------------------------------------------------------------
df %>% select(alarm_date) %>% is.na()

