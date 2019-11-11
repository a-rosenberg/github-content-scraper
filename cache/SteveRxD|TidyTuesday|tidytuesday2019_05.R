# Load packages
library(tidyverse)
library(janitor)
library(cowplot)
library(ggrepel)
library(scales)

# Read data and clean with janitor:

coast_vs_waste <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/coastal-population-vs-mismanaged-plastic.csv") %>% clean_names()
waste_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-plastic-waste-vs-gdp-per-capita.csv")  %>% clean_names()

# Reference:
  # Definitions of mismanaged plastic waste:
    #https://ourworldindata.org/grapher/mismanaged-plastic-waste?tab=chart

# Combine into a single dataframe:

waste <- waste_vs_gdp %>%
  left_join(coast_vs_waste, by = c("entity", "code", "year")) %>% 
  #filter to only show 2010, the only year with waste data
  filter(year == '2010') %>%    
  #exclude rows that are not counties
  filter(!is.na(code) & entity != 'World') %>% 
  #drop the duplicate column resulting from join
  select(-contains('.y')) 
  
# Tidy up names:

waste1 <- waste %>%
  rename(waste_capita = contains('per_capita_plastic'),
         income_capita = contains('gdp_per_capita'),
         mismanaged_total = contains('mismanaged_plastic'),
         total_pop = contains('total_pop'),
         coastal_pop = contains('coastal_pop')) 

# Calculate three new variables:

waste2 <- waste1 %>% 
  #mismanaged waste per capita (kg per day)
  mutate(mismanaged_capita = 
           mismanaged_total / 365 * 1000 / coastal_pop) %>% 
  #share of plastic waste that is mismanaged (%)
  mutate(share_mismanaged = mismanaged_capita / waste_capita *100) %>% 
  #total plastic waste (million tonnes)
  mutate(waste_total = waste_capita * 365 / 1000 / 10^6 * coastal_pop)

# Create plots:

  #set color and alpha for plot points
  mycolor1 <- "#0070C0"
  myalpha1 <- 0.7
  
  #set color and alpha for the trend lines
  mycolor2 <- "#FF9900"
  myalpha2 <- 0.15

  plot1 <- ggplot(data = waste2 %>% filter(), 
                  aes(x = income_capita, y = waste_capita, 
                      size = waste_total)) +
    geom_smooth(method = "lm", color = mycolor2, alpha = myalpha2, show.legend = FALSE) + 
    geom_point(color = mycolor1, alpha = myalpha1)  +   
    background_grid(major = "xy", minor = "none") +
    scale_x_continuous(trans=log10_trans(), labels = comma) +
    scale_y_continuous(trans=log10_trans()) +
    labs(title = "Plastic waste tends to increase \nwith country income",
         x = "",
         y = "Plastic waste per capita (kg per day)",
         size = "Total plastic waste (million tonnes)") +
    geom_text_repel(aes(label = entity),
      color         = "red",
      size          = 4,
      data          = subset(waste2, waste_capita > 1 | waste_capita < .015 | code %in% c("USA","CHN")
                        | (income_capita > 80000 & waste_capita < 0.1)),
      nudge_y       = .25,
      segment.color = "grey50",
      direction     = "x") 

  plot2 <- ggplot(data = waste2,
                  aes(x = income_capita, y = share_mismanaged,
                      size = waste_total)) +
    geom_smooth(color = mycolor2, alpha = myalpha2, show.legend = FALSE) + 
    geom_point(color = mycolor1, alpha = myalpha1)  +  
    background_grid(major = "xy", minor = "none") +
    scale_x_continuous(trans=log10_trans(), labels = comma) +
    scale_y_continuous(trans=log10_trans()) +
    labs(title = "The proportion of mismanaged plastic \nwaste falls as incomes rise",
         x = "GDP per capita (log scale)",
         y = "Share of plastic waste that is mismanaged (%)") +
    geom_text_repel(aes(label = entity),
      color         = "red",
      size          = 4,
      data          = subset(waste2, code %in% c("USA","CHN") | 
                        (income_capita < 10000 & share_mismanaged < 10)),
      nudge_y       = .25,
      segment.color = "grey50",
      direction     = "x") 

  plot3 <- ggplot(data = waste2, 
                  aes(x = income_capita, y = mismanaged_capita, 
                      size = waste_total)) +
    geom_smooth(color = mycolor2, alpha = myalpha2, show.legend = FALSE) + 
    geom_point(color = mycolor1, alpha = myalpha1)  +    
    background_grid(major = "xy", minor = "none") +
    scale_x_continuous(trans=log10_trans(), labels = comma) +
    scale_y_continuous(trans=log10_trans())  +
    labs(title = "Mismanaged waste peaks \nfor middle-income countries",
         x = "",
         y = "Mismanaged plastic waste per capita (kg per day)")  +
    geom_text_repel(aes(label = entity),
      color         = "red",
      size          = 4,
      data          = subset(waste2, code %in% c("USA","CHN") | 
                       mismanaged_capita > .16 | mismanaged_capita < .001),
      nudge_y       = .1,
      nudge_x       = .05,
      segment.color = "grey50",
      direction = "x") 

# Get legend from plot1 which will be given own panel:
l <- get_legend(plot1) 

# Final plot:
plot_grid(plot1 + theme(legend.position = "none"), 
          plot2 + theme(legend.position = "none"), 
          plot3 + theme(legend.position = "none"),
          NULL,
          NULL,
          l,
          nrow = 2, rel_heights = c(6,1)
          )
  
