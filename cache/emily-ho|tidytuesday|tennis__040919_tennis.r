
require(magrittr)

player_dob <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/player_dob.csv")
grand_slams <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slams.csv")
grand_slam_timeline <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slam_timeline.csv")

age_slams_comb <- dplyr::left_join(grand_slams, player_dob, by = c("name")) %>% 
  dplyr::mutate(age = tournament_date - date_of_birth) %>% # needs to be datetime
  dplyr::group_by(name, age, gender) %>% 
  dplyr::summarize(counts = n()) %>% 
  dplyr::group_by(name) %>% 
  dplyr::mutate(total_wins = cumsum(counts)) %>% 
  dplyr::arrange(desc(total_wins))
  

# test plot
age_slams_comb %>% 
  ggplot2::ggplot(ggplot2::aes(x = age, 
  	y = total_wins, group = name)) +
  ggplot2::geom_point() +
  ggplot2::geom_step() +
  ggplot2::facet_wrap(~gender) +
  ggplot2::theme_bw()


tmp <- dplyr::left_join(grand_slams, player_dob, by = c("name")) %>% 
         dplyr::mutate(age = tournament_date - date_of_birth)

df <- grand_slam_timeline %>% 
	dplyr::rename(name = "player") %>% 
	dplyr::left_join(., age_slams_comb, by = c('name', 'year'))

##############

dplyr::glimpse(df)

dat <- grand_slam_timeline %>% 
			dplyr::filter(outcome == "Won") %>% 
			dplyr::arrange(year) %>% 
			dplyr::group_by(player) %>% 
			dplyr::mutate(count = seq(n())) 

dat <- grand_slam_timeline %>%
				dplyr::filter(outcome == "Won") %>%
				dplyr::mutate(outcome == ifelse(outcome == "Won", 1,0)) 

theme_bluewhite <- function (base_size = 11, base_family = "") {
    ggplot2::theme_classic() + 
        ggplot2::theme(
            panel.grid.major  = ggplot2::element_line(color = "white"),
            panel.background = ggplot2::element_rect(fill = "lightblue"),
          #  panel.border = ggplot2::element_rect(color = "lightblue", fill = NA),
            axis.line = ggplot2::element_line(color = "lightblue"),
            axis.ticks = ggplot2::element_line(color = "lightblue"),
            axis.text = ggplot2::element_text(color = "steelblue")
        )
}




dat %>% 
    ggplot2::ggplot(., ggplot2::aes(x=player, y = count, col = player,
                                    group = player)) + 
    ggplot2::geom_point() + 
    ggplot2::geom_line() + 
    ggplot2::facet_wrap(~tournament) +
    theme_bluewhite() +
    ggplot2::theme(legend.position = "none")

    
dat %>% 
    ggplot2::ggplot(., ggplot2::aes(x=year, y = count, col = player,
                                 group = player)) + 
    ggplot2::geom_point() + 
    ggplot2::geom_line() + 
    ggplot2::facet_wrap(~tournament) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
  #  ggplot2::geom_text() +
    ggplot2::labs(title = 'Hour: {frame_along}') + 
    #  transition_time(date_time) #+
    gganimate::transition_reveal(count)


gganimate::animate(anim, fps = 6)
  
