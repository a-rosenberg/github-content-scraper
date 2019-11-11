# Load libraries
library(tidyverse)
library(gganimate)
library(viridis)
library(ggdark)

altrenderer <- gifski_renderer(loop=FALSE)

# Load data
big_epa_cars <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-15/big_epa_cars.csv")

# Filter variables to be used in analysis
make_class_data<-big_epa_cars%>%
  select(c(comb08, city08, highway08, 
           fuelCost08, fuelType, fuelType1, 
           make, model, VClass, year, cylinders))
  
# Filter Major US automakers by brand
american<-make_class_data%>%
  mutate(brand = case_when(
    str_detect(make, paste(c("Ford", "Lincoln", "Mercury"), collapse = '|')) ~ "Ford Motor Company",
    str_detect(make, paste(c("Chrysler", "Dodge", "Jeep", "Ram","Plymouth","Eagle"), collapse = "|")) ~ "Chrysler Group",
    str_detect(make, paste(c("General Motors","GMC","Cheverolet","Hummer", "Buick", "Pontiac","Saturn", "Oldsmobile"),collapse = "|"))~"General Motors"))

################### Need to figure out why I can't just pipe this after the mutation above
american2<-american%>%
  filter(!is.na(brand))%>%
  group_by(brand, year, cylinders)%>%
  summarize(mean_city = mean(city08),
            mean_highway = mean(highway08),
            mean_fuel = mean(fuelCost08))

# Calculate number of years in data set
n_years <-length(unique(american2$year))

# Creat baseplot for animation
p<-ggplot(american2, 
       aes(x = mean_city, 
           y = mean_highway, 
           color = brand, 
           size = cylinders))+
  geom_point(alpha = 0.8, 
             na.rm = TRUE)+
  scale_x_continuous(limits = c(0,60))+
  scale_y_continuous(limits = c(0,60))+
  scale_color_viridis_d()+
  dark_theme_minimal()+
  theme(panel.grid.major = element_line("Red", 
                                        size = 0.1),
        panel.grid.minor = element_line("Red",
                                        size = 0.1),
        plot.title = element_text(color = "Green1"),
        plot.subtitle = element_text(color = "Green1"),
        plot.caption = element_text(color = "Green1"),
        axis.title.x = element_text(color = "Green1"),
        axis.title.y = element_text(color = "Green1"),
        axis.text = element_text(color = "Red"),
        legend.text = element_text(color = "Green1"),
        legend.title = element_text(color = "Green1"))
  

# Create animated plot
p_animate<-p+
  transition_states(year, transition_length = 3, state_length = 9)+
  ease_aes('linear')+
  labs(title = "Fuel Efficiency: City v. Highway", 
       subtitle = "Year: {closest_state}",
       x = "City (mpg)",
       y = "Highway (mpg)",
       color = "Auto Group",
       size = "Cylinders",
       caption = "Source: fueleconomy.gov | Graph: @mathl3t3")+ 
  view_follow(fixed_x=TRUE, fixed_y=TRUE)


# Animate plot
p_gif<-animate(p_animate,
               renderer = altrenderer,
               nframes = n_years*10,
               fps=10, 
               width = 800, 
               height = 500,
               rewind = FALSE)

# Save animation
anim_save(
  animation = p_gif,
  filename = here::here("brandfuel.gif")
)
