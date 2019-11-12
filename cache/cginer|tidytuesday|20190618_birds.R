## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE)


## ----packages------------------------------------------------------------
# To read the data set
library(readr)

# Data manipulation
library(dplyr)

# Visualization
library(ggplot2)
library(ggbeeswarm) # geom_quasirandom


## ----getdata-------------------------------------------------------------
bird_counts <- read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-18/bird_counts.csv",
    col_types = cols(col_integer(), col_character(), col_character(),
                     col_double(), col_double(), col_double())
    )


## ----explore, eval=FALSE-------------------------------------------------
## # Quick look at the dataset and summaries by year
## head(bird_counts)
## 
## bird_counts %>%
##     group_by(year) %>%
##     summarize(n = n(),
##               num_species_listed = length(unique(species)),
##               num_species_counted = length(unique(species[how_many_counted > 0])),
##               total_counted = sum(how_many_counted),
##               mean_counted_total = mean(how_many_counted),
##               mean_counted = mean(how_many_counted[how_many_counted > 0]))
## 


## ----annotations---------------------------------------------------------
old_friends <- c("Herring Gull", "European Starling", "House Sparrow", "Mallard", "Barn Swallow", "Rock Pigeon")
new_friends <- c("American Robin", "Blue Jay", "Downy Woodpecker", "Black-capped Chickadee", "Canada Goose", "Eastern Towhee", "House Finch", "Wood Duck", "Red-tailed Hawk", "Red-winged Blackbird", "Wild Turkey", "Spotted Sandpiper", "Northern Cardinal", "Double-crested Cormorant", "Northern Mockingbird", "American Goldfinch", "Mourning Dove", "American Crow", "Ring-billed Gull")

bird_counts <- bird_counts %>%
    mutate(Class = factor(
        case_when(species %in% new_friends ~ "New friends",
                  species %in% old_friends ~ "Old friends",
                  TRUE ~ "Others"),
        levels = c("Others", "New friends", "Old friends")))



## ----plot_annotations, fig.height=3--------------------------------------
ggplot(bird_counts %>% filter(year %in% "1921"), aes("", fill = Class)) +
    geom_bar() +
    coord_flip() +
    scale_fill_viridis_d(begin = 0.1, end = 0.8, direction =  -1) +
    labs(x = NULL, y = "Number of species", title = "Personal classification of birds in the data",
         subtitle = "There are still many birds to learn about!") +
    scale_x_discrete(expand = c(0, 0), breaks = NULL) +
    scale_y_continuous(expand = c(0, 0 ))


## ----summary_birds-------------------------------------------------------
summary_birds <- bird_counts %>%
    group_by(species, species_latin, Class) %>%
    summarize(
        mean_counted_by_hour = mean(how_many_counted_by_hour, na.rm = TRUE),
        variance_counted_by_hour = var(how_many_counted_by_hour, na.rm = TRUE),
        years_spotted = sum(how_many_counted > 0)) %>%
    mutate(
        Regularity = case_when(years_spotted < 5 ~ "Rare",
                               years_spotted < 94/2 ~ "Less than half of the years",
                               years_spotted < 95-5 ~ "More than half of the years",
                               years_spotted > 94-5 ~ "Very common")
    )



## ----mean_count----------------------------------------------------------
ggplot(summary_birds %>% filter(mean_counted_by_hour > 0), aes(Class, mean_counted_by_hour)) +
    geom_quasirandom(aes(colour = Class), show.legend = FALSE, na.rm = TRUE) +
    geom_text(data = summary_birds %>% filter(species %in% c("Wood Duck", "Spotted Sandpiper", "Barn Swallow")),
              aes(label = species), size = 2.5, nudge_x = 0.28) +
    geom_text(data = summary_birds %>% filter(species %in% c("Eastern Towhee")),
              aes(label = species), size = 2.5, nudge_x = -0.33) +
    scale_y_log10(breaks = c(0.0001, 0.01, 1, 100),
                  labels = c("0.0001", "0.01", "1", "100")) +
    annotation_logticks(sides = "l") +
    scale_colour_viridis_d(begin = 0.1, end = 0.8, direction =  -1) +
    labs(x = NULL, y = "Mean bird count per hour", title = "Bird abundance",
         subtitle = "Most birds I know are fairly common") +
    theme_minimal()


## ------------------------------------------------------------------------
# Impute number of hours in years that have NAs (several years between 1921 and 1950)
# I'm going to use Downy Woodpecker as reference, because it's spotter every year at a similar rate
downys_per_hour_1929_1950 <- bird_counts %>%
    filter(species %in% "Downy Woodpecker" & year <= 1950) %>%
    pull(how_many_counted_by_hour) %>%
    mean(na.rm = TRUE)

bird_counts_imputed <- bird_counts %>%
    group_by(year) %>%
    # Use Downy woodpecker to estimate total_hours
    mutate(total_hours = if_else(
        is.na(total_hours),
        round(how_many_counted[species %in% "Downy Woodpecker"] / downys_per_hour_1929_1950),
        total_hours)) %>%
    # Fill counts/h those years
    mutate(how_many_counted_by_hour = if_else(
        is.na(how_many_counted_by_hour),
        how_many_counted/total_hours,
        how_many_counted_by_hour))

# Order by mean abundance
bird_order <- summary_birds %>%
    arrange(mean_counted_by_hour) %>%
    pull(species)
bird_counts_imputed <- bird_counts_imputed %>%
    mutate(species = factor(species, levels = bird_order))


## ------------------------------------------------------------------------
ggplot(bird_counts_imputed %>%
           filter(Class %in% c("New friends", "Old friends") & how_many_counted_by_hour),
       aes(year, species, size = how_many_counted_by_hour, colour = Class)) +
    geom_point(show.legend = c(colour = FALSE)) +
    scale_size_area(max_size = 7, breaks = c(1, 10, 100, 400), name = "Birds per hour") +
    scale_colour_viridis_d(begin = 0.1, end = 0.8, direction =  -1, drop = FALSE) +
    scale_x_continuous(expand = c(0.04, 0)) +
    facet_grid(rows = vars(Class), scales = "free", space ="free") +
    theme_minimal() +
    labs(title = "Temporal trends",
         subtitle = "Downy Woodpecker sigthings are used to impute\nthe total counting hours for some years before 1950")

