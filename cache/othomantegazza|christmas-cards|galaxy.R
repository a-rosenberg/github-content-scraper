library(tidyverse)
library(gridExtra)
load("data/galaxy.Rdata")

ratio <- 1.6
scale_rt <- 1000
ylims <- c(-15, 10)
# xlims <- c(-.025, .02)
xlims <- (ylims/scale_rt)*ratio
wd <- xlims[2] - xlims[1]
ht <- ylims[2] - ylims[1]
# bins_ratio <- wd/ht
bin_h <- .4

p <- ggplot(to_plot,
            aes(x = PC5,
                y = PC1)) + 
  geom_hex(
    # bins = 80
    binwidth = c((bin_h/scale_rt), bin_h)
    ) +
  theme_void() +
  lims(x = xlims,
       y = ylims) +
  coord_fixed(ratio = 1/scale_rt, expand = FALSE) +
  theme(
    # aspect.ratio = 1/1.6,
    panel.background = element_rect(fill = "#440154FF"),
    plot.background = element_rect(colour = "grey80", size = .1),
    plot.margin = margin(t = 10, r = 8,
                         b = 10, l = 8, unit = "mm"))

p_vir <- 
  p +
  scale_fill_viridis_c(
    begin = .2, end = 1,
    option = "D",
    guide = FALSE,
    trans = "log")


png(filename = "cards/galaxy-single.png",
    width = 11.7/2, 
    height = 8.27/2,
    units = "in",
    res = 300)
p_vir
dev.off()

pdf("cards/galaxy.pdf",
    width = 11.7, 
    height = 8.27,
    paper = "a4r")
grid.arrange(grobs = list(p_vir, p_vir, p_vir, p_vir),
             layout_matrix = matrix(1:4, ncol = 2))
dev.off()


pdf("cards/galaxy-inf.pdf",
    width = 11.7, 
    height = 8.27,
    paper = "a4r")
p_inf <- 
  p +
  scale_fill_viridis_c(
    begin = .2, end = 1,
    option = "E",
    # option = "D",
    guide = FALSE,
    trans = "log") + 
  theme(
    panel.background = element_rect(fill = "#FFFFFF"),
    plot.background = element_rect(colour = "grey80", size = .1),
    plot.margin = margin(t = 10, r = 8,
                         b = 10, l = 8, unit = "mm"))
grid.arrange(grobs = list(p_inf, p_inf, p_inf, p_inf),
             layout_matrix = matrix(1:4, ncol = 2))
dev.off()

png(filename = "cards/galaxy-single-white.png",
    width = 11.7/2, 
    height = 8.27/2,
    units = "in",
    res = 300)
p_inf
dev.off()
