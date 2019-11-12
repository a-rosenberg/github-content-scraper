## ------------------------------------------------------------------------
# week 13

library(tidyverse)
library(skimr)
library(paletteer)


## ------------------------------------------------------------------------
dat <- read.csv("../data/week13_alcohol_global.csv")

skim(dat)
head(dat)


## ------------------------------------------------------------------------
# reshape so I can group and make faceted plots
# top_n to pull out 40 countries with highest consumption
# make country a factor
# gather() to put it in long format
dat2 <- dat %>%
    rename(beer = beer_servings,
           wine = wine_servings,
           spirits = spirit_servings) %>%
    top_n(4, total_litres_of_pure_alcohol) %>%  
    mutate(country = as.factor(country)) %>%  
    gather(key = "alc_type", value = "value", 
           -country, -total_litres_of_pure_alcohol) 


## ------------------------------------------------------------------------
# scatterplot matrix
# (everything against everything else)
plot(dat, main = "Scatterplot Matrix")


## ------------------------------------------------------------------------

# exploratory bar chart
p <- ggplot(dat2, aes(x = alc_type, y = value, fill = alc_type)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~country, ncol = 2) +
    theme_bw() +
    labs(title = "Amount of alcohol consumed by type", 
         subtitle = "4 countries with highest total litres of pure alcohol", 
         x = "alcohol type", 
         y = "# servings")


## ------------------------------------------------------------------------
p


## ------------------------------------------------------------------------
p + scale_fill_brewer(type = "qual", palette = "Paired")


## ------------------------------------------------------------------------
p + scale_fill_brewer(type = "qual", palette = "Dark2")


## ------------------------------------------------------------------------
p + scale_fill_paletteer_d(nord, aurora)


## ------------------------------------------------------------------------
p + scale_fill_paletteer_d(nord, polarnight)


## ------------------------------------------------------------------------
p + scale_fill_paletteer_d(nord, snowstorm)


## ------------------------------------------------------------------------
p + scale_fill_paletteer_d(quickpalette, beach)


## ------------------------------------------------------------------------
p + scale_fill_paletteer_d(quickpalette, waterfall)


## ------------------------------------------------------------------------
p + scale_fill_paletteer_d(quickpalette, sunset)


## ------------------------------------------------------------------------
p + scale_fill_paletteer_d(rcartocolor, ag_Sunset)


## ------------------------------------------------------------------------
p + scale_fill_paletteer_d(rcartocolor, TealRose)


## ------------------------------------------------------------------------
p + scale_fill_paletteer_d(rcartocolor, TealGrn)


## ------------------------------------------------------------------------
p + scale_fill_paletteer_d(rcartocolor, Geyser)

