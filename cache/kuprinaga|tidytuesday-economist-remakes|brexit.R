source('./get_data.R')
library(lubridate)
library(scales)
library(ggthemes)
library(magrittr)
library(gridExtra)
library(ggrepel)
library(cowplot)

glimpse(brexit)

brexit %<>%
  rename(right = percent_responding_right,
         wrong = percent_responding_wrong) %>%
  gather(response, perc, -date) %>%
  mutate(date_pretty = dmy(date)) %>%
  select(date_pretty, response, perc) %>%
  arrange(date_pretty) %>%
  group_by(response) %>% 
  mutate(rank = rank(desc(date_pretty), ties.method = "first"),
         label_data = if_else(rank == 30, response, NA_character_))


p_brexit <- ggplot(data = brexit,
       aes(x = date_pretty, 
           y = perc, 
           color = response)) +
  geom_smooth(se=F) +
  geom_point(alpha = 0.3) +
  geom_label_repel(data = brexit,
                   aes(label = label_data),
                   nudge_x = 1,
                   na.rm = TRUE) + 
  coord_cartesian(ylim = c(38,max(brexit$perc))) + 
  scale_x_date(date_breaks = "6 months", 
               date_labels = "%b '%y") +
  scale_y_continuous(position = 'right',
                     breaks = c(seq(38, 48, 2)),
                     labels = number_format(accuracy = 1)) +
  theme_classic(base_size = 12, base_family = "sans")  + 
  scale_color_manual(values = c("#116ea0", "#dd6e6d")) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        text = element_text(family="Arial Narrow"),
        panel.grid.major.y = element_line(
          colour = '#c2cdd4',
          linetype = 'solid',
          size = 0.5
        ),
        plot.title = element_text(family = "Arial Narrow")) +
  labs(title = '"In hindsight, do you think Britain was right or wrong \n to vote to leave the EU?"',
       subtitle = '% responding')


title.grob <- textGrob(
  expression(bold(underline("Bremorse"))),
  x = unit(0, "lines"), 
  y = unit(0, "lines"),
  hjust = -0.1, vjust = 0,
  gp = gpar(fontsize = 16, fontfamily = "Arial Narrow"))

foot.grob <- textGrob(
  label = "Source: NatCen Social Research
  ",
  x = unit(0, "lines"), 
  y = unit(0, "lines"),
  hjust = -0.1, vjust = 0,
  gp = gpar(fontsize = 10, fontfamily = "Arial Narrow"))

p_brexit_original_copy <- arrangeGrob(p_brexit, 
                  top = title.grob,
                  bottom = foot.grob)
grid.draw(p_brexit_original_copy)

ggsave('original_r.png', p_brexit_original_copy,
       width = 10, height = 9, units = "cm")

