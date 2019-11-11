library(tidyverse)

funding <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/fed_r_d_spending.csv") %>% 
  filter(department == "DOD")

fund_plot =
  ggplot() +
  geom_line(data = funding, aes(x = year, y = rd_budget/1000000000), size = 1) +
  geom_rect(aes(xmin=2009, xmax=2017, ymin=0, ymax=Inf, fill = "Democrat"), color = NA, alpha = 0.25) +
  geom_rect(aes(xmin=2001, xmax=2009, ymin=0, ymax=Inf, fill = "Republican"), color = NA, alpha = 0.25) +
  geom_rect(aes(xmin=1993, xmax=2001, ymin=0, ymax=Inf, fill = "Democrat"), color = NA, alpha = 0.25) +
  geom_rect(aes(xmin=1981, xmax=1993, ymin=0, ymax=Inf, fill = "Republican"), color = NA, alpha = 0.25) +
  geom_rect(aes(xmin=1977, xmax=1981, ymin=0, ymax=Inf, fill = "Democrat"), color = NA, alpha = 0.25) +
  geom_rect(aes(xmin=1974, xmax=1977, ymin=0, ymax=Inf, fill = "Republican"), color = NA, alpha = 0.25) +
  
  scale_fill_manual("President's Party",
                    values = c('blue', 'red'),  
                    guide = guide_legend(override.aes = list(alpha = 0.1))) +
  
  scale_x_continuous(breaks = seq(1976, 2017, 4)) +
  scale_y_continuous(breaks = seq(0, 100, 20)) +
  
  labs(
    title = "Dept. of Defense Spending on\nResearch & Development vs Presidential Party",
    x = "Year",
    y = "Annual Budget (Billions USD)\n") +
    
  theme_minimal(16) +
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.y = element_text(hjust = 0),
        legend.position = "right",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12)
  )


fund_plot

ggsave(filename = "20190212_Federal_Budget_DOD_by_Party.jpg", width=20, height=10, units="cm", scale=1.6)
