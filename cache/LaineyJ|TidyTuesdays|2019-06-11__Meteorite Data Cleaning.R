## ----setup, include = FALSE----------------------------------------------
library("tidyverse")
library("mgcv")
library("maps")
library("extrafont")
library("ggrepel")
library("dummies")
library("corrplot")
library("grid")
library("gridExtra")

meteorites <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-11/meteorites.csv") %>%
  filter(!is.na(lat))


## ------------------------------------------------------------------------
filter_meteorites <- function(boundaries) {
  met_filter <- in.out(as.matrix(boundaries[, c("lat","long")]),
                      as.matrix(meteorites[, c("lat", "long")]))
  met_filtered <- meteorites[met_filter, ]
}

us <- map_data("state")
iowa <- map_data("state", "iowa")

meteorites_us <- filter_meteorites(us)
meteorites_ia <- filter_meteorites(iowa)


## ------------------------------------------------------------------------
plt_iowa <- meteorites_ia %>%
  ggplot(aes(x = long,
             y = lat,
             size = mass,
             color = fall)) + 
  geom_point(na.rm = TRUE) +
  borders("state", "iowa")

plt_iowa <- plt_iowa +
  scale_color_brewer(name = "Falls vs. Finds", palette = "Dark2") +
  scale_size_continuous(name = "Mass (g)",
                        labels = scales::comma) +
  geom_text_repel(aes(label = paste(name, year, sep = ", ")),
                  size = 3.5,
                  color = "black",
                  point.padding = 0.5,
                  min.segment.length = 5,
                  family = "Segoe UI") +
  labs(title = "Meteorite Impacts in Iowa by Mass and Fall Type",
       subtitle = "Meteorite \"falls\" were identified shortly after their fall;
Meteorite \"finds\" were identified at a later date
") +
  theme(text = element_text(family = "Segoe UI"),
        plot.background = element_rect(fill = "whitesmoke"),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(family = "Franklin Gothic Medium"),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.background = element_rect(fill = "whitesmoke"))
plt_iowa


## ------------------------------------------------------------------------
met_dummies <- function(df) {
  fall_dummies <- dummy(df$fall)
  df$fall_bin <- fall_dummies[, 1]
  df
}

meteorites_us <- suppressWarnings(met_dummies(meteorites_us))
meteorites_ia <- suppressWarnings(met_dummies(meteorites_ia))

us_mean <- mean(meteorites_us$fall_bin)
ia_mean <- mean(meteorites_ia$fall_bin)


## ------------------------------------------------------------------------
cor_df <- meteorites_us[, c(5, 7, 11)]
cor_df <- rename(cor_df,
                 Year = year,
                 Mass = mass,
                 Fell = fall_bin)

cor_mat <- cor(cor_df, use = "complete.obs")
return_corrplot <- function() {
  correls <- corrplot(cor_mat,
                      method = "color",
                      type = "upper",
                      bg = "whitesmoke",
                      diag = FALSE,
                      outline = TRUE,
                      addCoef.col = "black",
                      tl.col = "black",
                      cl.pos = "n")
}


## ------------------------------------------------------------------------
plt_iowa

return_corrplot()

