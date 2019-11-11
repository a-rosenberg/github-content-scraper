require(magrittr)
require(ggplot2)
require(gganimate)
require(transformr)

bike_traffic <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-02/bike_traffic.csv")

# formatting time and date

bike_traffic %<>% 
         dplyr::mutate(date_time = lubridate::mdy_hms(date),
                      # date = format(date_time, '%d/%m/%Y'),
                      # time = format(date_time, '%H:%M:%S'),
                       hour = lubridate::hour(date_time))

# summarizing mean traffic by hour 
mean_traffic <- bike_traffic %>%
  dplyr::group_by(hour, crossing, direction) %>%
  dplyr::summarize(bike_count =mean(bike_count, na.rm=T)) #%>% head()



# rush hour 

rushhr <- data.frame(rush = c("AM Rush (6 - 9AM)", "PM Rush (4 - 7PM)"),
                     lb = c(6, 16),
                     ub = c(9, 19))
# static plot 
ggplot2::ggplot(mean_traffic, 
             ggplot2::aes(x=hour, y=bike_count, size = bike_count, 
                    group = crossing, color = crossing)
     ) +
  ggplot2::geom_point(alpha = 0.8, show.legend = F) + geom_line(size = 1.25) + 
  ggplot2::facet_wrap(~direction) +
  ggplot2::theme_classic() +
  ggplot2::geom_rect(data = rushhr, 
                     ggplot2::aes(xmin = lb, xmax = ub, 
                                  ymin = -Inf, ymax = Inf,
                                  fill=rush
                     ),
                     alpha = 0.25,  
                     inherit.aes = F) +
  ggplot2::ylab("Bike Count") +
  ggplot2::xlab("Hour of Day") +
  ggplot2::theme(legend.position ="bottom", 
                 legend.title = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(size = 16),
                 axis.text = ggplot2::element_text(size = 16),
                 axis.title = ggplot2::element_text(size = 20))


animated_traffic <- ggplot2::ggplot(mean_traffic, 
                              ggplot2::aes(x=hour, y=bike_count, 
                              group = crossing, color = crossing)
     ) +
  ggplot2::geom_point(alpha = 0.8, show.legend = F) + 
  ggplot2::geom_line(size = 1.25, show.legend = F) + 
  # geom_segment(aes(xend=25, yend = bike_count), linetype = 2, colour = 'grey') + 
  

  ggplot2::facet_wrap(~direction) +
  ggplot2::theme_classic()   +
  ggplot2::geom_text(aes(x=hour, y = bike_count + 8, label = crossing), show.legend = F) + 
  ggplot2::geom_rect(data = rushhr, 
                     ggplot2::aes(xmin = lb, xmax = ub, 
                                  ymin = -Inf, ymax = Inf,
                                  fill=rush
                     ),
                     alpha = 0.25,  
                     inherit.aes = F) +
    ggplot2::ylab("Bike Count") +
    ggplot2::xlab("Hour of Day") +
    ggplot2::theme(legend.position ="top", 
                 legend.title = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(size = 16),
                 axis.text = ggplot2::element_text(size = 16),
                 axis.title = ggplot2::element_text(size = 20)) +

  ggplot2::labs(title = 'Hour: {frame_along}') + 
#  transition_time(date_time) #+
  gganimate::transition_reveal(hour) 


gganimate::animate(animated_traffic, fps = 6)
  

gganimate::anim_save("animated_traffic.gif", animated_traffic)