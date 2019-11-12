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

# exploratory bar chart
p <- ggplot(dat2, aes(x = alc_type, y = value, fill = alc_type)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~country, ncol = 2) +
    theme_bw() +
    labs(x = "alcohol type", 
         y = "# servings")


## ------------------------------------------------------------------------
print(p)


## ------------------------------------------------------------------------
palettes <- palettes_d_names


## ------------------------------------------------------------------------
plot_fun <- function(base_plot, col_pkg, col_pal) {
    Title <- paste(col_pkg, col_pal)
    out <- base_plot +
        scale_fill_paletteer_d(!!ensym(col_pkg), !!ensym(col_pal)) +
        ggtitle(Title)
    return(out)
}


## ------------------------------------------------------------------------
# p is my base plot from up above
map2(palettes$package, palettes$palette, ~ plot_fun(p, .x, .y))

