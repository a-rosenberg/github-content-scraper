#' @importFrom grid gList
#'
#' @export
geom_quantileframe <- function(mapping = NULL,
                            data = NULL,
                            stat = "identity",
                            position = "identity",
                            ...,
                            sides = "bl",
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE
                            ) {

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomQuantileFrame,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      sides = sides,
      na.rm = na.rm,
      ...
    )
  )
}

#' @export
GeomQuantileFrame <- ggplot2:::ggproto(`_class` = "GeomQuantileFrame",
                          `_inherit` = ggplot2:::Geom,
                          optional_aes = c("x", "y"),

  draw_panel = function(data, panel_scales, coord, sides = "bl") {
    rugs <- list()
    data <- coord[["transform"]](data, panel_scales)
    gp <- grid::gpar(col = alpha("#181C1D", data[["alpha"]]),
               lty = data[["linetype"]],
               lwd = data[["size"]] * .pt,
               lineend = "butt")
    if (!is.null(data[["x"]])) {
      if (grepl("b", sides)) {
        gp <- grid::gpar(col = alpha("#181C1D", data[["alpha"]]),
                   lty = data[["linetype"]],
                   lwd = data[["size"]] * .pt * 3,
                   lineend = "butt")
        rugs[["x_b"]] <- ggplot2:::ggname("range_x_b",
                                grid::segmentsGrob(x0 = unit(as.numeric(quantile(data[["x"]], 0.25)), "native"),
                                             x1 = unit(as.numeric(quantile(data[["x"]], 0.75)), "native"),
                                             y0 = unit(0, "npc"),
                                             y1 = unit(0, "npc"),
                                             gp = gp))
        # Min/Max segment
        gp <- grid::gpar(col = alpha("#181C1D", data[["alpha"]]),
                   lty = data[["linetype"]],
                   lwd = data[["size"]] * .pt,
                   lineend = "butt")
        rugs[["x_b2"]] <- ggplot2:::ggname("range_x_b",
                                          grid::segmentsGrob(x0 = unit(min(data[["x"]]), "native"),
                                                       x1 = unit(max(data[["x"]]), "native"),
                                                       y0 = unit(0, "npc"),
                                                       y1 = unit(0, "npc"),
                                                       gp = gp))
      }

      if (grepl("t", sides)) {
        rugs[["x_t"]] <- ggplot2:::ggname("range_x_t",
                                grid::segmentsGrob(x0 = unit(min(data[["x"]]), "native"),
                                             x1 = unit(max(data[["x"]]), "native"),
                                             y0 = unit(1, "npc"),
                                             y1 = unit(1, "npc"),
                                             gp = gp))
      }
    }

    if (!is.null(data[["y"]])) {
      if (grepl("l", sides)) {

        gp <- grid::gpar(col = alpha("#181C1D", data[["alpha"]]),
                   lty = data[["linetype"]],
                   lwd = data[["size"]] * .pt * 5,
                   lineend = "butt")
        rugs[["y_l"]] <- ggplot2:::ggname("range_y_l",
                                          grid::segmentsGrob(y0 = unit(as.numeric(quantile(data[["y"]], 0.25)), "native"),
                                                       y1 = unit(as.numeric(quantile(data[["y"]], 0.75)), "native"),
                                                       x0 = unit(0, "npc"),
                                                       x1 = unit(0, "npc"),
                                                       gp = gp))
        # Min/Max segment
        gp <- grid::gpar(col = alpha("#181C1D", data[["alpha"]]),
                   lty = data[["linetype"]],
                   lwd = data[["size"]] * .pt,
                   lineend = "butt")
        rugs[["y_l2"]] <- ggplot2:::ggname("range_y_l",
                                          grid::segmentsGrob(y0 = unit(min(data[["y"]]), "native"),
                                                       y1 = unit(max(data[["y"]]), "native"),
                                                       x0 = unit(0, "npc"),
                                                       x1 = unit(0, "npc"),
                                                       gp = gp))
      }

      if (grepl("r", sides)) {
        rugs[["y_r"]] <- ggplot2:::ggname("range_y_r",
                                grid::segmentsGrob(y0 = unit(as.numeric(quantile(data[["y"]], 0.25), "native"),
                                             y1 = unit(as.numeric(quantile(data[["y"]], 0.75))), "native"),
                                             x0 = unit(1, "npc"),
                                             x1 = unit(1, "npc"),
                                             gp = gp))
      }
    }
    ggplot2:::ggname("geom_quantileframe", grid::gTree(children = do.call(grid::gList, rugs)))
  },
  default_aes = ggplot2::aes(colour = "#181C1D", size = 0.5,
                    linetype = 1, alpha = NA),

  draw_key = ggplot2:::draw_key_path
  )

