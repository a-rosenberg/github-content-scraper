setwd("~/TidyTuesday/2018-09-04")
library(tidyverse)
library(ggiraphExtra)


###Fastfood data
data <- read.csv2("fastfood_calories.csv", sep=",", dec=".") %>%
  select(-X,-salad) 
 
### Color palette based on restaurant logos 
library(paletter)
tacobell <- create_palette(image_path = "~/TidyTuesday/2018-09-04/Taco_bell_logo.jpg",
                            number_of_colors =1,
                            type_of_variable = "categorical")
dairyqueen <- create_palette(image_path = "~/TidyTuesday/2018-09-04/dq_logo.jpg",
                             number_of_colors =1,
                             type_of_variable = "categorical")
subway <- create_palette(image_path = "~/TidyTuesday/2018-09-04/logo-subway.jpg",
                         number_of_colors =1,
                         type_of_variable = "categorical")
Chikfila <- create_palette(image_path = "~/TidyTuesday/2018-09-04/Chick_fil_a_logo.jpg",
                         number_of_colors =1,
                         type_of_variable = "categorical")
arbys <- create_palette(image_path = "~/TidyTuesday/2018-09-04/arbys_logo.jpg",
                           number_of_colors =1,
                           type_of_variable = "categorical")
burgerking <- create_palette(image_path = "~/TidyTuesday/2018-09-04/BG_logo.jpg",
                        number_of_colors =1,
                        type_of_variable = "categorical")
sonic <- create_palette(image_path = "~/TidyTuesday/2018-09-04/Sonic-logo.jpg",
                             number_of_colors =1,
                             type_of_variable = "categorical")
mcdonalds <- create_palette(image_path = "~/TidyTuesday/2018-09-04/mcdo_logo.jpg",
                        number_of_colors =1,
                        type_of_variable = "categorical")

palette = c("Chick Fil-A"= Chikfila,
            "Taco Bell" = tacobell,
            "Subway" = subway,
            "Dairy Queen" = dairyqueen,
            "Arbys" = arbys,
            "Burger King" = burgerking,
            "Sonic" = sonic,
            "Mcdonalds" = mcdonalds)

###plot average calories per entree item for each restaurant
data %>% transform(restaurant=fct_reorder(restaurant, calories, .fun=mean)) %>%
  ggplot( aes(x=restaurant, y=calories))+
  stat_summary(fun.y="mean", geom="bar", aes(fill=restaurant)) +
  scale_fill_manual(values=palette)+
  theme_bw() + 
  theme(legend.position="none",
        axis.text = element_text(size=14),
        axis.title = element_text(size=16),
        strip.text = element_text(size=14),
        plot.title = element_text(size = 18))+
  labs(x="", y="Average calories per item")

####Average per restaurant
data_rest <- data %>%
  select(restaurant, calories, total_fat, total_carb, sugar,
         protein, fiber, calcium, vit_a, vit_c) %>%
  group_by(restaurant) %>%
  summarise_all(funs(mean(.,na.rm=TRUE))) %>%
  rename(carb=total_carb,fat=total_fat) %>%
  transform(restaurant=fct_reorder(restaurant, calories, .fun=mean)) 
  
#Spider plot
ggRadar(data=data_rest, aes(color=restaurant))+
  scale_fill_manual(values=palette)+
  scale_color_manual(values=palette)+
  theme_bw() + 
  theme(legend.position="none",
          axis.text = element_text(size=14),
          strip.text = element_text(size=14),
          plot.title = element_text(size = 18)) +
  facet_wrap(~restaurant, nrow=2) 

#Animated spider plot
library(gganimate)
ggRadar(data=data_rest, aes(color=restaurant))+
  scale_fill_manual(values=palette)+
  scale_color_manual(values=palette)+
  ggtitle('{closest_state}')+
  theme_bw() + 
  theme(legend.position="none",
        axis.text = element_text(size=14),
        strip.text = element_text(size=14),
        plot.title = element_text(size = 18)) +
  transition_states(restaurant, transition_length=1, state_length=1)+
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')
