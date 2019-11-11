source('./get_data.R')
source('./brexit.R')

library(gganimate)

p_brexit_remake <- ggplot(data = brexit,
                   aes(x = date_pretty, 
                       y = perc, 
                       color = response)) +
  geom_smooth(fill = 'ivory3') +
  geom_point(alpha = 0.3) +
  geom_label_repel(data = brexit,
                   aes(label = label_data),
                   nudge_x = 1,
                   na.rm = TRUE) + 
  coord_cartesian(ylim = c(38,max(brexit$perc))) + 
  scale_x_date(date_breaks = "4 months", 
               date_labels = "%b '%y") +
  scale_y_continuous(position = 'right',
                     breaks = c(seq(38, 48, 2)),
                     labels = number_format(accuracy = 1,
                                            suffix = '%')) +
  theme_classic(base_size = 12, base_family = "sans")  + 
  scale_color_manual(values = c("gray", "#dd6e6d")) + 
  theme(axis.title.x = element_blank(),
        legend.position = "none",
        text = element_text(family="Arial"),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(family = "Arial")) +
  labs(title = 'Bremorse',
       y = '% responded',
       subtitle = '"In hindsight, do you think Britain was right or wrong \n to vote to leave the EU?"')
p_brexit_remake <- add_sub(p_brexit_remake, 
        "Source: NatCen Social Research", 
        x = 0, hjust = 0,
        size = 10, 
        fontfamily = "Arial")

ggdraw(p_brexit_remake)
ggsave('remade.png', p_brexit_remake,
       width = 10, height = 7.5, units = "cm")
 