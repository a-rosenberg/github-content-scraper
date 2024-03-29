## ----setup, echo = TRUE--------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, warning = FALSE)
library(tidyverse)
library(ggthemes)
library(scales)


## ----source_files, echo = TRUE-------------------------------------------
govt_raw <- read_csv ("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/fed_r_d_spending.csv")
govt_clean <- govt_raw %>%
   drop_na() %>%
   mutate_at (3:6, funs(round(./1000000000,1)))
head(govt_clean,10)


## ----govt_2017, echo=TRUE------------------------------------------------
govt_clean %>%
   filter (year == "2017") %>%
   ggplot(aes(x=reorder(department, rd_budget), y=rd_budget)) +
      geom_bar(stat='identity') +
      coord_flip() +
      theme_economist () +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.title = element_blank()) +
      labs(title = "R&D budget by department",
           subtitle = "2017, billions of USD",
           caption = "Source: American Association for the Advancement of Science")


## ----dod, echo = TRUE----------------------------------------------------
govt_clean %>%
   filter (department == "DOD") %>%
   ggplot () +
      geom_rect(aes(xmin=1974, xmax=1977, ymin=0,ymax=Inf, fill="Republican"), alpha=0.2) +
      geom_rect(aes(xmin=1977, xmax=1981, ymin=0,ymax=Inf, fill="Democrat"), alpha=0.2) +
      geom_rect(aes(xmin=1981, xmax=1993, ymin=0,ymax=Inf, fill="Republican"), alpha=0.2) +
      geom_rect(aes(xmin=1993, xmax=2001, ymin=0,ymax=Inf, fill="Democrat"), alpha=0.2) +
      geom_rect(aes(xmin=2001, xmax=2009, ymin=0,ymax=Inf, fill="Republican"), alpha=0.2) +
      geom_rect(aes(xmin=2009, xmax=2017, ymin=0,ymax=Inf, fill="Democrat"), alpha=0.2) +
      scale_fill_manual (values = c('blue','red')) +
   
      geom_line(aes (year, rd_budget)) +
   
      scale_x_continuous(breaks = seq(1976, 2017,4)) +
      theme_economist() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.title = element_blank()) +
      labs(title = "Dept of Defense R&D budget increases with Republican presidents",
           subtitle = "1976-2017, billions of USD",
           caption = "Source: American Association for the Advancement of Science")


## ----govt_all, echo=TRUE-------------------------------------------------
govt_all <-
   govt_clean %>%
   ggplot() +
      geom_rect(aes(xmin=1974, xmax=1977, ymin=0,ymax=Inf, fill="Republican"), alpha=0.2) +
      geom_rect(aes(xmin=1977, xmax=1981, ymin=0,ymax=Inf, fill="Democrat"), alpha=0.2) +
      geom_rect(aes(xmin=1981, xmax=1993, ymin=0,ymax=Inf, fill="Republican"), alpha=0.2) +
      geom_rect(aes(xmin=1993, xmax=2001, ymin=0,ymax=Inf, fill="Democrat"), alpha=0.2) +
      geom_rect(aes(xmin=2001, xmax=2009, ymin=0,ymax=Inf, fill="Republican"), alpha=0.2) +
      geom_rect(aes(xmin=2009, xmax=2017, ymin=0,ymax=Inf, fill="Democrat"), alpha=0.2) +
      scale_fill_manual (values = c('blue','red')) +
   
      geom_line(aes (year, rd_budget)) +
   
      theme_economist() +
      theme(axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         axis.text.x = element_text(size = 8),
         axis.text.y = element_text(size = 7),
         legend.text=element_text(size = 8),
         legend.title = element_blank(),
         panel.spacing = unit(2, "lines"),
         strip.text = element_text(size = 7)) +
      facet_grid(department ~., scales = "free_y") +
      scale_x_continuous(breaks = seq(1976, 2017,4)) +
      scale_y_continuous(breaks = scales::pretty_breaks(2)) +
      labs(title = "R&D budget by department",
        subtitle = "1976-2017, billions of USD",
        caption = "Source: American Association for the Advancement of Science")
   
ggsave("govt_all.png",govt_all)

