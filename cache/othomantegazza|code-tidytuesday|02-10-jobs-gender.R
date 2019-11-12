## ------------------------------------------------------------------------
library(tidyverse)


## ------------------------------------------------------------------------
dat_path <- "data/2-10-jobs-gender.Rdata"
dat_url <- paste0("https://raw.githubusercontent.com/",
                 "rfordatascience/tidytuesday/master/data/",
                 "2019/2019-03-05/jobs_gender.csv")

if(!file.exists(dat_path)) {
  jobs_gender <- read.csv(dat_url)
  
  save(jobs_gender, file = dat_path)
} else {
  load(dat_path)
}


## ------------------------------------------------------------------------
to_plot <- 
  jobs_gender %>% 
  # selected categories
  filter(year == 2016,
         major_category == "Computer, Engineering, and Science")


## ------------------------------------------------------------------------
# needed to center divergent palette
lim <- 
  to_plot$percent_female %>% 
  range() %>% 
  abs() %>% 
  max()


## ------------------------------------------------------------------------
p <- 
  to_plot %>% 
  ggplot(aes(x = total_earnings_female,
             y = total_earnings_male,
             colour = percent_female, 
             size = total_workers)) +
  geom_point(alpha = .8) +
  geom_abline(slope = 1,
              intercept = 0,
              colour = "grey60",
              size = .5) +
  # scale_x_log10() +
  # scale_y_log10() +
  scale_y_continuous(labels = scales::scientific) +
  scico::scale_color_scico(palette = "imola",
                           limits = c(100 - lim, lim)) +
  theme_minimal() +
  labs(title = "Estimated Median Earnings for Fulltime Workers",
       subtitle = paste0('In the US in 2016,\n',
                         'for "Computer, Engineering, and Science" related jobs.'),
       x = "Females   [USD]", 
       y = "Males   [USD]",
       size = "Number of Workers",
       colour = "Percent of Females",
       caption = "Source: Census Bureau & Bureau of Labor\nPlot by @othomn") +
  theme(text = element_text(family = "sans",
                            colour = "grey40"), 
        plot.title = element_text(colour = "#3752C3"))

p


## ---- eval = FALSE-------------------------------------------------------
## png(filename = "plots/2-10-jobs-gender.png",
##     height = 1500, width = 2000,
##     res = 300)
## p %>% print()
## dev.off()


## ------------------------------------------------------------------------
to_plot %>% 
  select(total_earnings_female,
         total_earnings_male,
         percent_female, 
         total_workers,
         occupation,
         minor_category) %>% 
  jsonlite::write_json(path = "data/2-10-jobs-gender.json")

