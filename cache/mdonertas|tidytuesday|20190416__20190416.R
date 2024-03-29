## ----message=F, warning=F------------------------------------------------
library(tidyverse)
library(ggthemes)
library(ggrepel)
library(ggpubr)


## ----message=FALSE, warning=FALSE----------------------------------------
brexit <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/brexit.csv")

corbyn <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/corbyn.csv")

dogs <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/dogs.csv")

eu_balance <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/eu_balance.csv")

pensions <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/pensions.csv")

trade <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/trade.csv")

women_research <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/women_research.csv")


## ------------------------------------------------------------------------
corbyn_clean <- corbyn %>%
    rename(name = political_group) %>%
    na.omit()
corbyn_clean


## ------------------------------------------------------------------------
p_corbyn <- corbyn_clean %>%
    mutate(name = name %>% fct_reorder(avg_facebook_likes) %>% fct_rev()) %>%
    ggplot(aes(y = avg_facebook_likes, x = name, fill = factor(rank(avg_facebook_likes)))) +
    geom_bar(stat = 'identity') +
    coord_flip() +
    theme_economist(dkpanel = T, base_size = 12) +
    xlab('') + ylab('') +
    ggtitle('Left-click','Average number of likes per Facebook post, 2016') +
    scale_fill_brewer(type = 'seq', palette = 8) +
    guides(fill = F) +
    theme(axis.text.y = element_text(hjust = 1),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(),
          plot.title = element_text(color = 'darkred', face = 'bold'),
          plot.subtitle = element_text(color = 'gray40', face = 'italic')) +
    scale_y_continuous(labels = scales::comma) +
    labs(caption = 'Source: Facebook')


## ---- fig.align='centre'-------------------------------------------------
p_corbyn


## ---- echo = F-----------------------------------------------------------
ggsave('./corbyn.pdf', p_corbyn, units = 'cm', height = 8, width = 16)
ggsave('./corbyn.png', p_corbyn, units = 'cm', height = 8, width = 16)


## ------------------------------------------------------------------------
head(dogs)


## ------------------------------------------------------------------------
p_dogs <- dogs %>%
    mutate(year = lubridate::ymd(dogs$year, truncated = 2L)) %>%
    ggplot(aes(x = year)) + 
    geom_line(aes(y = avg_neck, color = 'Neck Size'), size = 3) +
    geom_line(aes(y = (avg_weight+1)*2, color = 'Weight'), size = 3) +
    scale_y_continuous(sec.axis = sec_axis(~./2 - 1, name = "Weight**, kg",), 
                       limits = c(38,45)) +
    theme_economist(base_size = 12) +
    scale_x_date() + 
    scale_colour_manual(values = c("darkred", "steelblue3")) +
    ggtitle('Fit as a butcher\'s dog','Characteristics of registered with the UK\'s\nKennel Club, average when fully grown') +
    guides(color = F) +
    xlab('') +
    ylab('Neck Size*, cm') +
    labs(caption = '*Where at least 100 are registered per year\n**Where at least 50 are registered per year\nSources: Kennel Club; The Economist') +
    theme(axis.title.y.left = element_text(angle = 0, colour = 'darkred', 
                                           face = 'italic', hjust = 0, 
                                           margin = margin(0,-80,0,0)),
          axis.title.y.right = element_text(angle = 0, colour = 'steelblue3', 
                                            face = 'italic', hjust = 1, 
                                            margin = margin(0,0,0,-70), 
                                            vjust = 1),
          axis.text.y.right = element_text(margin = margin(0,0,0,10)),
          plot.subtitle = element_text(margin = margin(0,0,20,0)))


## ---- fig.align='centre'-------------------------------------------------
p_dogs


## ---- echo = F-----------------------------------------------------------
ggsave('./dogs.pdf', p_dogs, units = 'cm', height = 10, width = 10)
ggsave('./dogs.png', p_dogs, units = 'cm', height = 10, width = 10)


## ------------------------------------------------------------------------
p_dogs2 <- dogs %>%
  # mutate(year = lubridate::ymd(dogs$year, truncated = 2L)) %>%
  ggplot(aes(x = avg_weight, y = avg_neck, color = year)) +
  geom_point(size = 2) +
  geom_path(size = 1,arrow = arrow(length = unit(8,"pt"))) +
  geom_label(data = filter(dogs, year %in% c(2006, 2015)),
             aes(label = year, fill = year),
             color = 'white', nudge_y = -0.2) +
  theme_economist() +
  theme(legend.position = 'right') +
  ggtitle('Fit as a butcher\'s dog','Characteristics of registered with the UK\'s Kennel Club,\naverage when fully grown') +
  xlab('Weight**, kg') + ylab('Neck Size*, cm') +
  scale_y_continuous(breaks = 42:44, limits = c(42,44.4)) +
  scale_x_continuous(breaks = 18:20, limits = c(18,20.6)) +
  labs(caption = '*Where at least 100 are registered per year\n**Where at least 50 are registered per year\nSources: Kennel Club; The Economist') +
  scale_color_gradient(low = 'lightsteelblue3', high = 'steelblue4', breaks = c(2006,2015), guide = F) +
  scale_fill_gradient(low = 'lightsteelblue3', high = 'steelblue4', breaks = c(2006,2015), guide =F) +
  coord_fixed(ratio = 100*((max(dogs$avg_neck)-min(dogs$avg_neck))/max(dogs$avg_neck)) / (100*((max(dogs$avg_weight)-min(dogs$avg_weight))/max(dogs$avg_weight))))


## ---- fig.align='centre'-------------------------------------------------
p_dogs2


## ---- echo = F-----------------------------------------------------------
ggsave('./dogs2.pdf', p_dogs2, units = 'cm', height = 10, width = 12)
ggsave('./dogs2.png', p_dogs2, units = 'cm', height = 10, width = 12)


## ------------------------------------------------------------------------
p_women <- women_research %>% 
    mutate(country = fct_reorder(country, percent_women)) %>%
    mutate(field = fct_reorder(field, percent_women)) %>%
    ggplot(aes(y = field, x = country, fill = percent_women)) +
    geom_tile() +
    scale_fill_gradient2(midpoint = 0.5, low = '#084594', high = '#99000D', 
                         mid = 'snow', breaks = c(0,0.5,1), limits = c(0,1)) +
    theme_economist(base_size = 12) +
    theme(legend.position = 'right',
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          axis.text.y = element_text(hjust = 1),
          panel.grid = element_blank(),
          axis.ticks = element_blank()) +
    guides(fill = guide_colorbar('% Women')) +
    xlab('') + ylab('') +
    ggtitle('Still a man\'s world', 'Women among researchers with papers published*\n2011-15') +
    labs(caption = '*Indexed in Scopus\nSources: \'Gender in the Global Research Landscape\' by Elsevier; The Economist\nPlot by @melikedonertas')


## ---- fig.align='centre'-------------------------------------------------
p_women


## ---- echo = F-----------------------------------------------------------
ggsave('./women.pdf', p_women, units = 'cm', height = 10, width = 18)
ggsave('./women.png', p_women, units = 'cm', height = 10, width = 18)

