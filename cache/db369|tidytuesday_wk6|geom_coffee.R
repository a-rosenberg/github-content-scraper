# EmilHvitfeldt/geom_heart.r
# https://gist.github.com/EmilHvitfeldt/a680703215c430bd1f6e2aa6831f3846
geom_coffee <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", 
                        ..., parse = FALSE, nudge_x = 0, nudge_y = 0, check_overlap = FALSE, 
                        na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("Specify either `position` or `nudge_x`/`nudge_y`", 
           call. = FALSE)
    }
    position <- position_nudge(nudge_x, nudge_y)
  }
  layer(data = data, mapping = mapping, stat = stat, geom = GeomText, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(parse = parse, check_overlap = check_overlap, 
                      na.rm = na.rm, label = sprintf('\u2615'), ...))
}