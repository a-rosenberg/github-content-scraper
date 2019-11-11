women_research <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/women_research.csv")

women_research %<>% 
	dplyr::arrange(desc(country), field) %>%
	dplyr::mutate(percent_men = 1 - percent_women,
	       		  ratio = percent_men/percent_women)

women_research %>% 
   		 ggplot2::ggplot(ggplot2::aes(x = country, y = ratio, group = field, col = field,fill = field, shape = field)) + 
		 ggplot2::geom_point(size = 4.5)  + 
		 ggplot2::theme_bw() + ggplot2::ylim(c(0, 12)) + 
		 ggplot2::geom_hline(yintercept = 1, lwd = 1.5, col = 'red') + 
		 ggplot2::coord_flip() +
	     ggplot2::theme(axis.text = ggplot2::element_text(size = 17),
			   plot.subtitle = ggplot2::element_text(size = 13),
			   plot.title = ggplot2::element_text(size = 25),
			   axis.title = ggplot2::element_text(size = 20),
			   legend.text = ggplot2::element_text(size = 15)) +
	     ggplot2::labs(title = "% of women authors in Scopus articles and patent applications (2011-2015)",
	          caption = "Tidy Tuesday | @emilyhho") + 
#ggplot2::scale_y_continuous(c(0,1,3,6,9,12)) + 
	     ggplot2::xlab("") +
	     ggplot2::scale_y_continuous(name = "Ratio of Men / Women publishing", c(0,1, 3, 6, 9, 12))

ggplot2::ggsave("economist.png")