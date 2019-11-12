## ------------------------------------------------------------------------
library(tidyverse)

theme_set(theme_light())

phd_field <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-19/phd_by_field.csv")


## ------------------------------------------------------------------------
glimpse(phd_field)


## ------------------------------------------------------------------------
head(phd_field)


## ------------------------------------------------------------------------
summary(phd_field)


## ------------------------------------------------------------------------
phd_field %>% 
    count(year)


## ------------------------------------------------------------------------
phd_field %>%
    filter(is.na(n_phds)) %>%
    group_by(year) %>%
    summarise(n = n())


## ------------------------------------------------------------------------
phd_processed <- phd_field %>%
    filter(!is.na(n_phds))


## ------------------------------------------------------------------------
phd_processed %>%
    group_by(year) %>%
    summarise(total_n_phds = sum(n_phds)) %>%
    mutate(year = forcats::fct_reorder(as.factor(year), total_n_phds)) %>%
    ggplot(aes(year, total_n_phds)) + 
    geom_col() +
    scale_y_continuous(labels = scales::comma_format()) +
    coord_flip() + 
    labs(x = "Year",
        y = "# of PhDs awarded",
        title = "2016 had the highest number of PhDs awarded.",
        subtitle = "2010 had the lowest number of PhDs awarded.")


## ------------------------------------------------------------------------
phd_processed %>%
    group_by(year, broad_field) %>%
    summarise(total_n_phds = sum(n_phds)) %>%
    ggplot(aes(year, total_n_phds, color = broad_field)) + 
    geom_line() + 
    scale_x_continuous(breaks = c(2008, 2011, 2014, 2017)) +
    scale_color_discrete(name = "Broad field") +
    labs(x = "Year", 
        y = "Total # of PhDs awarded", 
        title = "Toal number of PhDs awarded for each broad field by year",
        subtitle = "From 2008 - 2017")


## ------------------------------------------------------------------------
phd_processed %>%
    group_by(year, major_field) %>%
    summarise(total_n_phds = sum(n_phds)) %>%
    top_n(3) %>%
    ggplot(aes(year, total_n_phds, color = major_field)) + 
    geom_line() + 
    scale_x_continuous(breaks = c(2008, 2011, 2014, 2017)) +
    labs(x = "Year", 
        y = "Total # of PhDs awarded", 
        title = "Toal number of PhDs awarded by major field and year",
        subtitle = "Filtered to just look at the top 3 major fields.") 

