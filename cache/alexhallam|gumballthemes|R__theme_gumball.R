#' Gumball Theme
#'
#' [ggplot2] plot theme for my own personal tastes
#' @export
scale_color_gumball <- function(...){
    viridis::scale_color_viridis(discrete = T, direction = -1, end = .85, option = "A")
}

quantile_breaks <- function(val, prob, digits) {
  function(x) round(as.numeric(quantile(val, prob)),digits)
}

quantile_trans <- function(val, prob, digits) {
  scales::trans_new(
    name = "quantile",
    transform = function(x) x,
    inverse = function(x) x,
    breaks = quantile_breaks(val, prob, digits))
}
#' @export
scale_x_quantile <- function(val, prob = seq(0, 1, 0.25), digits = 2, ...) {
  scale_x_continuous(..., trans = quantile_trans(val, prob, digits))
}
#' @export
scale_y_quantile <- function(val, prob = seq(0, 1, 0.25), digits = 2, ...) {
  scale_y_continuous(..., trans = quantile_trans(val, prob, digits))
}
#' @export
theme_gumball <- function () {
  theme_classic(base_size=12) %+replace%
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      legend.key=element_blank(),
      legend.title = element_blank(),
      legend.position = "bottom",
      axis.line.x = element_blank(),
      axis.line.y = element_blank()
    )
}


