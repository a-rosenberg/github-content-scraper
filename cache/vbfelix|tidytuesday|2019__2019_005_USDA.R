# libraries ---------------------------------------------------------------

library(tidyverse)
library(janitor)
library(showtext)


# import ------------------------------------------------------------------

url    <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-29/clean_cheese.csv"
df_or  <- read_csv(url)


# pre-plot ------------------------------------------------------------

glimpse(df_or)

summary(df_or)

df_or %>% 
  select(1,2,4) %>% 
  mutate(ratio = Cheddar/Mozzarella) %>% 
  clean_names() -> df

# plot --------------------------------------------------------------------

font_add_google("Diplomata SC")
font_families_google()
showtext_auto()

x11()

df %>% 
  mutate(change = if_else(ratio > 1, "a","b")) %>% 
  ggplot(aes(year, ratio))+
  annotate("text", x= 2005, y = 3, label = "The Fall of\nCheddar", col = "#ff9522", family = "Diplomata SC",
           size = 20)+
  # annotate("rect", xmin = 1970, xmax = 2017, ymin = 1, ymax = 5, fill = "#ff9522", alpha = .3)+
  # annotate("rect", xmin = 1970, xmax = 2017, ymin = 0, ymax = 1, fill = "#fcf3d9", alpha = .5)+
  geom_hline(yintercept = 1, col = "royalblue3",size = 1, alpha = .7)+
  geom_line(aes(col = change, group = 1),
            size = 1.5,
            show.legend = F)+
  # geom_point(aes(fill = change),
  #           shape = 21,
  #           size = 4,
  #           show.legend = F) +
  labs(x = "Year",
       y = "Cheddar/Mozzarella",
       col = "Cheese:",
       title = "Ratio of cheddar/mozzarella consumption (lbs/person)",
       subtitle = "TidyTuesday - Week 5 : Dairy products",
       caption = "graphic: H0Vinicius | source: United States Department of Agriculture.")+
  theme_minimal(32)+
  theme(legend.position = "bottom",
        axis.text = element_text(colour = "white"),
        text = element_text(colour = "white"),
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        panel.grid.major.x  = element_blank(),
        panel.grid.minor.x  = element_blank(),
        panel.grid.major.y  = element_line(size = 1 , colour = "gray18"),
        panel.grid.minor.y  = element_line(size = 1 , colour = "gray18"),
        plot.margin = unit( c(1,1,1,1),"cm"))+
  scale_colour_manual(values = c("#ff9522","#fcf3d9"))+
  scale_x_continuous(breaks = c(seq(1970,2010, by = 10),2017),
                     expand = c(0,1.5))+
  scale_y_continuous(breaks = seq(1,5,by = 1), limits = c(0,5),
                     expand = c(0,0))+
  # geom_hline(col = "royalblue2",yintercept = 1, linetype = "dashed", size = 1)+
  NULL
  
  
