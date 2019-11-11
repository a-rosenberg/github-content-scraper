setwd("2019/W16_EconUpgrades/")
library(tidyverse)
library(gganimate)
women_research <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/women_research.csv")

# https://medium.economist.com/mistakes-weve-drawn-a-few-8cdd8a42d368

head(women_research)

women_research <- women_research %>% 
    mutate(percent_men = 1 - percent_women) %>% 
    mutate(percent_women = percent_women * 100,
           percent_men = percent_men * 100,
           field = factor(field))

#################
# Bars
#################

plt <- ggplot(data = women_research) + 
    geom_bar(aes(x = country, 
                 y = -percent_women,
                 fill = "female"),
             stat = "identity") + 
    geom_bar(aes(x = country, 
                 y = percent_men,
                 fill = "male"),
             stat = "identity") + 
    labs(title = "Gender composition of research authors",
         x = "",
         y = "Percent",
         caption = "Plot: @jmstone27") + 
    scale_y_continuous(limits = c(-100,100),
                       breaks = c(-100, -50, 0, 50, 100),
                       labels = c("100","50","0","50","100")) + 
    scale_fill_manual(name = "", 
                      values = c("female"="#db0f2d","male"="#42a3ca"),
                      labels = c("Female", "Male")) + 
    ggthemes::theme_economist() + 
    theme(
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 10)
    ) + 
    coord_flip() +
    transition_states(field, 
                      transition_length = 1,
                      state_length = 3,
                      wrap = TRUE) + 
    labs(subtitle = "Field: {closest_state}")

animate(plt, width = 400, height = 600)

anim_save("Bar-Research_Author_Gen_Composition.gif")


######################################################
# Lollipop/Thermometer type slider
######################################################

plt <- ggplot(data = women_research) + 
    geom_segment(aes(y = 0,
                     yend = -percent_women,
                     x = country,
                     xend = country),
                 color = "#db0f2d",
                 size = 2) + #women-segment
    geom_point(aes(x = country, y = -percent_women, color = "female"),
               size = 3) + #women-point
    geom_segment(aes(y = 0,
                     yend = percent_men,
                     x = country,
                     xend = country),
                 color = "#42a3ca",
                 size = 2) + #men-segment
    geom_point(aes(x = country, y = percent_men, color = "male"),
               size = 3) + #men-point
    labs(title = "Gender composition of \nresearch paper authors",
         x = "",
         y = "Percent",
         caption = "Plot: @jmstone27") + 
    scale_y_continuous(limits = c(-100,100),
                       breaks = c(-100, -50, 0, 50, 100),
                       labels = c("100","50","0","50","100")) + 
    scale_color_manual(name = "", 
                      values = c("female"="#db0f2d","male"="#42a3ca"),
                      labels = c("Female", "Male")) + 
    ggthemes::theme_economist() + 
    theme(
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 10)
    ) + 
    coord_flip() + 
    transition_states(field, 
                      transition_length = 1,
                      state_length = 3,
                      wrap = TRUE) + 
    labs(subtitle = "Field: {closest_state}")

animate(plt, width = 400, height = 600)

anim_save("Lollipop-Research_Author_Gen_Composition.gif")
