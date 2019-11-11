library(tidyverse)

load("data/phyllo.Rdata")

p <- ggplot(dat, 
       aes(x = as.factor(Plant),
           y = angle,
           # fill = angle,
           # colour = angle
           colour = Genotype
           )) +
  geom_point(
    alpha = .4, size = 1.8, 
    # shape = 21
    ) +
  facet_grid(. ~ Genotype) +
  xlab("Plant") +
  ylab("Angle [degrees]") +
  ylim(0, 360) +
  # scale_fill_viridis_c(guide = FALSE) +
  scale_color_viridis_d(guide = FALSE) +
  theme_void() +
  theme(strip.text = element_blank())

p

pdf(file = "cards/phyllo.pdf",
    height = 4, width = 6,
    paper = "a4")
p
dev.off()

