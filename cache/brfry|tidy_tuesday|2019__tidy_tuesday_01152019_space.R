# libraries
library(tidyverse)
library(RColorBrewer)

#===============================================================================

# read in data
agencies_df <- read_csv("data/agencies.csv")
launches_df <- read_csv("data/launches.csv")

#===============================================================================

# set colors and themes. idk which theme i like so there are two for now.
colors_four = RColorBrewer::brewer.pal(5, "Dark2")[5:2]

colors_space <- c("#542788", 
                  "#35978f",
                  "#7f3b08",
                  "#bf812d",
                  "#9ebcda"
                   )
        
colors_space2 <- c("#542788", 
                   "#35978f",
                   "#00441b",
                   "#006d2c",
                   "#b30000",
                   "#9ebcda"
)

# theme with custom colors
theme_space <- list(theme_bw(),  scale_color_manual(values = colors_space),
                    scale_fill_manual(values = colors_space))

theme_space2 <- list(theme_bw(),  scale_color_manual(values = colors_space2),
                     scale_fill_manual(values = colors_space2))
#===============================================================================

# explore launches
launches_df %>%
        count(type, sort = TRUE) %>%
        top_n(10, n) %>%
      #  mutae(launch_date = lubridate::ymd(launch_date))) %>%
      #  group_by(type) %>%
      #  summarise()
        ggplot(aes(x = type, y = n)) +
        geom_point()

#===============================================================================

# how have launches changed by agency type and number over time:
launches_df %>%
        group_by(launch_year, agency_type, type) %>%
        summarise(count = n()) %>%
        rename(Agency = agency_type) %>%
        ggplot(aes(x = launch_year, y = count, fill = Agency)) +
        geom_bar(stat = "identity") + 
        labs(title = "The Changing Space Industry", y = "Number of Launches",
             x = "Year") +
        theme_space

#===============================================================================

# filtering for state only:
launches_df %>%
        filter(agency_type == "state") %>%
        group_by(launch_year, state_code) %>%
        summarise(count = n()) %>%
        rename(State = state_code) %>%
        mutate(Nations = ifelse(State == "US", "United States",
                               ifelse(State == "CN", "China",
                                      ifelse(State == "SU", "Soviet Union",
                                             ifelse(State == "RU", "Russia",
                                                    ifelse(State == "IN", "India",
                                             "Other")))))) %>%
        mutate(Nations = factor(Nations, levels = c("United States", "China", 
                                                  "Soviet Union", "Russia", 
                                                  "India", "Other"))) %>%
        ggplot(aes(x = launch_year, y = count, fill = Nations)) +
        geom_bar(stat = "identity") + 
        labs(title = "Nation State Launches", y = "Number of Launches",
             x = "Year") +
        theme_space2 

#===============================================================================

# just compare private vs startup
launches_df %>%
        filter(agency_type != "state") %>%
        group_by(launch_year, agency_type, state_code) %>%
        summarise(count = n()) %>%
        mutate(state_code = ifelse(state_code == "US", "United States",
                                   ifelse(state_code == "RU", "Russia",
                                          ifelse(state_code == "J", "Japan",
                                                 ifelse(state_code == "F", "France",
                                                        " Cayman Islands"))))) %>%
        ggplot(aes(x = launch_year, y = count, fill = agency_type)) +
        geom_bar(stat = "identity") + 
        labs(title = "Private and Startup Launches", y = "Number of Launches",
             x = "Year") +
        facet_grid(state_code~.) +
        theme_space2 #+ scale_fill_viridis_d(name = " ")

#===============================================================================

# filtering for private and startup only and comparing all companies
launches_df %>%
        filter(agency_type != "state") %>%
        group_by(launch_year, agency, state_code) %>%
        summarise(count = n()) %>%
        mutate(state_code = ifelse(state_code == "US", "United States",
                                   ifelse(state_code == "RU", "Russia",
                                          ifelse(state_code == "J", "Japan",
                                                 ifelse(state_code == "F", "France",
                                                        " Cayman Islands"))))) %>%
        ggplot(aes(x = launch_year, y = count, fill = agency)) +
        geom_bar(stat = "identity") + 
        labs(title = "Private and Startup Launches", y = "Number of Launches",
             x = "Year") +
        facet_grid(state_code~.) +
        theme_space2 + scale_fill_viridis_d(name = "Company Code")


#===============================================================================
