library(tidyverse)
library(ptrapr)
library(ggraph)

# get data ----------------------------------------------------------------

data("good_panicle")

# fix node types
igraph::vertex_attr(good_panicle)$type <- 
  igraph::vertex_attr(good_panicle)$type %>% 
  {case_when(. == "Seconday" ~ "Secondary",
             TRUE ~ .)} %>% 
  factor(levels = c("Generating", "Primary",
                    "Secondary",
                    "spikelet", "End"))


# plot panicle ------------------------------------------------------------

point_size <- 2

plot_p <- function(panicle)
  {
  p <- 
    panicle %>% 
    ggraph() + 
    geom_edge_link(colour = "grey40",
                   edge_width = .54,
                   arrow = grid::arrow(length = unit(1.2,
                                                     "mm"),
                                       type = "closed"),
                   end_cap = circle(point_size/2, "mm"),
                   start_cap = circle(point_size/2, "mm")) +
    geom_node_point(aes(colour = type),
                    # size = 3.7,
                    size = point_size,
                    alpha = 1) +
    scale_x_reverse() +
    coord_flip() +
    # scale_color_manual(values = scico::scico(
    #   10, palette = "lajolla")[c(3,8,7,6,5)],
    #   guide = FALSE) +
    theme_void() +
    theme(aspect.ratio = 1.413793,
          plot.background = element_rect(colour = "grey80", size = .1),
          # panel.background = element_rect(fill = "ivory1"),
          # panel.grid.major = element_line(colour = "grey40",
          #                                 size = .2, linetype = "dotted"),
          # panel.grid.minor = element_line(colour = "grey60",
          #                                 size = .1, linetype = "dotted"),
          plot.margin = margin(8, 4, 8, 4, unit = "mm"))

  return(p)  
}

p <- 
  good_panicle %>% 
  plot_p() +
  scale_color_manual(values = scico::scico(
    10, palette = "lajolla")[c(8, 7, 6, 5, 3)],
    guide = FALSE)

p

p_grob <- p %>% ggplotGrob()

library(gridExtra)

pdf("cards/panicle.pdf",
    height = 11.7, 
    width = 8.27,
    paper = "a4")
grid.arrange(grobs = list(p_grob, p_grob, p_grob, p_grob),
             layout_matrix = matrix(1:4, ncol = 2))
dev.off()

library(cowplot)

pdf("cards/panicle2.pdf",
    height = 11.7, 
    width = 8.27,
    paper = "a4")
cowplot::plot_grid(
  plotlist = list(p_grob, p_grob, p_grob, p_grob),
  nrow = 2
)
dev.off()
# png("cards/panicle.png",
#     height = 1200,
#     width = 1200,
#     res = 300)
# p
# dev.off()
# 


# other panicle, other colors ---------------------------------------------
# 
# path_to_folder <- system.file(package = "ptrapr") %>%
#   paste0("/extdata")
# 
# panicle_paths <- 
#   list.files(path_to_folder) %>%
#   str_remove(".ricegr|.ricepr") %>%
#   unique()
# 
# pan4 <- 
#   path_to_folder %>% 
#   paste(panicle_paths[9], sep = "/") %>% 
#   read_full_panicle()
# 
# pan4 %>% 
#   plot_p()

p_blue <- 
  good_panicle %>% 
  plot_p() + 
  scale_color_manual(values = scico::scico(
    10, palette = "devon")[c(1, 2, 3, 5, 7)], 
    guide = FALSE)

p_blue_grob <-
  p_blue %>% 
  ggplotGrob()

library(gridExtra)

pdf("cards/panicle_blue.pdf",
    height = 11.7, 
    width = 8.27,
    paper = "a4")
grid.arrange(grobs = list(p_blue_grob, p_blue_grob,
                          p_blue_grob, p_blue_grob),
             layout_matrix = matrix(1:4, ncol = 2))
dev.off()


# Single plots ------------------------------------------------------------

png("cards/panicle-single.png",
    height = 11.7/2, 
    width = 8.27/2,
    units = "in",
    res = 300)
p
dev.off()

png("cards/panicle-single-blue.png",
    height = 11.7/2, 
    width = 8.27/2,
    units = "in",
    res = 300)
p_blue
dev.off()
