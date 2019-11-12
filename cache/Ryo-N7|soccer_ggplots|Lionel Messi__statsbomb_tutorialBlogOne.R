## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)


## ---- message=FALSE, warning=FALSE---------------------------------------
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(tidyverse, ## mainly dplyr, purrr, and tidyr
               StatsBombR, SBpitch, soccermatics,
               extrafont, ggupset, tibbletime,
               ggtext, ggrepel, glue,
               patchwork, cowplot, gtable, grid,
               magick)

## loading fonts
loadfonts(device = "win", quiet = TRUE)


## ---- eval=FALSE---------------------------------------------------------
## library(StatsBombR)
## comps <- FreeCompetitions()
## 
## glimpse(comps)


## ---- eval=FALSE---------------------------------------------------------
## messi_matches_raw <- comps %>%
##   filter(competition_id == 11) %>%
##   FreeMatches()
## 
## messi_data_raw <- StatsBombFreeEvents(MatchesDF = messi_matches_raw)


## ---- eval=FALSE---------------------------------------------------------
## messi_data_clean <- messi_data_raw %>%
##   allclean() %>%
##   left_join(comps %>% select(season_id, season_name), by = "season_id")


## ---- eval=FALSE---------------------------------------------------------
## messi_data_clean <- messi_data_clean %>%
##   ## player name
##   mutate(player.name = case_when(
##     player.name == "Oleguer Presas Renom" ~ "Oleguer",
##     player.name == "Xavier Hernández Creus" ~ "Xavi",
##     player.name == "Carles Puyol i Saforcada" ~ "Carles Puyol",
##     player.name == "Anderson Luís de Souza" ~ "Deco",
##     player.name == "Rafael Márquez Álvarez" ~ "Rafa Márquez",
##     player.name == "Giovanni van Bronckhorst" ~ "Gio v.Bronckhorst",
##     player.name == "Samuel Eto'o Fils" ~ "Samuel Eto'o",
##     player.name == "Víctor Valdés Arribas" ~ "Víctor Valdés",
##     player.name == "Juliano Haus Belletti" ~ "Juliano Belletti",
##     player.name == "Ludovic Giuly" ~ "Ludovic Giuly",
##     player.name == "Andrés Iniesta Luján" ~ "Andrés Iniesta",
##     player.name == "Ronaldo de Assis Moreira" ~ "Ronaldinho",
##     player.name == "Lionel Andrés Messi Cuccittini" ~ "Lionel Messi",
##     player.name == "Fernando Navarro i Corbacho" ~ "Fernando Navarro",
##     player.name == "Sylvio Mendes Campos Junior" ~ "Sylvinho",
##     player.name == "Damià Abella Pérez" ~ "Damià",
##     player.name == "Rubén Iván Martínez Andrade" ~ "Ronaldinho",
##     player.name == "Ronaldo de Assis Moreira" ~ "Rubén",
##     player.name == "Thiago Motta" ~ "Thiago Motta",
##     player.name == "Mark van Bommel" ~ "Mark van Bommel",
##     player.name == "Henrik Larsson" ~ "Henrik Larsson",
##     player.name == "José Edmílson Gomes de Moraes" ~ "Edmílson",
##     player.name == "Gabriel Francisco García de la Torre" ~ "Gabri",
##     player.name == "Santiago Ezquerro Marín" ~ "Santi Ezquerro",
##     player.name == "Maximiliano Gastón López" ~ "Maxi López",
##     player.name == "Gianluca Zambrotta" ~ "Gianluca Zambrotta",
##     player.name == "Eiður Smári Guðjohnsen" ~ "Eiður Guðjohnsen",
##     player.name == "Lilian Thuram" ~ "Lilian Thuram",
##     player.name == "Javier Pedro Saviola Fernández" ~ "Javier Saviola",
##     player.name == "Gnégnéri Yaya Touré" ~ "Yaya Touré",
##     player.name == "Bojan Krkíc Pérez" ~ "Bojan",
##     player.name == "Eric-Sylvain Bilal Abidal" ~ "Eric Abidal",
##     player.name == "Gabriel Alejandro Milito" ~ "Gabriel Milito",
##     player.name == "Giovani dos Santos Ramírez" ~ "Giovani dos Santos",
##     player.name == "Víctor Vázquez Solsona" ~ "Víctor Vázquez",
##     player.name == "Thierry Henry" ~ "Thierry Henry",
##     player.name == "José Manuel Pinto Colorado" ~ "José Manuel Pinto",
##     player.name == "Daniel Alves da Silva" ~ "Dani Alves",
##     player.name == "Sergio Busquets i Burgos" ~ "Sergio Busquets",
##     player.name == "Seydou Kéita" ~ "Seydou Kéita",
##     player.name == "José Martín Cáceres Silva" ~ "Martín Cáceres",
##     player.name == "Gerard Piqué Bernabéu" ~ "Gerard Piqué",
##     player.name == "Aliaksandr Hleb" ~ "Aliaksandr Hleb",
##     player.name == "Pedro Eliezer Rodríguez Ledesma" ~ "Pedro",
##     player.name == "Sergio Rodríguez García" ~ "Rodri",
##     player.name == "Rafael Romero Serrano" ~ "Fali",
##     player.name == "José Manuel Rueda Sampedro" ~ "José Manuel Rueda",
##     player.name == "Zlatan Ibrahimovic" ~ "Zlatan Ibrahimovic",
##     player.name == "Dmytro Chygrynskiy" ~ "Dmytro Chygrynskiy",
##     player.name == "Maxwell Scherrer Cabelino Andrade" ~ "Maxwell",
##     player.name == "Jeffren Isaac Suárez Bermúdez" ~ "Jeffren",
##     player.name == "Víctor Sánchez Mata" ~ "Víctor Sánchez",
##     player.name == "Thiago Alcântara do Nascimento" ~ "Thiago Alcântara",
##     player.name == "David Villa Sánchez" ~ "David Villa",
##     player.name == "Javier Alejandro Mascherano" ~ "Javier Mascherano",
##     player.name == "Andreu Fontàs Prat" ~ "Andreu Fontàs",
##     player.name == "Ibrahim Afellay" ~ "Ibrahim Afellay",
##     player.name == "Manuel Agudo Durán" ~ "Nolito",
##     player.name == "Marc Bartra Aregall" ~ "Marc Bartra",
##     player.name == "Adriano Correia Claro" ~ "Adriano",
##     player.name == "Martín Montoya Torralbo" ~ "Martín Montoya",
##     player.name == "Jonathan dos Santos Ramírez" ~ "Jonathan dos Santos",
##     player.name == "Francesc Fàbregas i Soler" ~ "Cesc Fàbregas",
##     player.name == "Alexis Alejandro Sánchez Sánchez" ~ "Alexis Sánchez",
##     player.name == "Juan Isaac Cuenca López" ~ "Isaac Cuenca",
##     player.name == "Gerard Deulofeu Lázaro" ~ "Gerard Deulofeu",
##     player.name == "Cristian Tello" ~ "Cristian Tello",
##     player.name == "Sergi Roberto Carnicer" ~ "Sergi Roberto",
##     player.name == "Marc Muniesa Martínez" ~ "Marc Muniesa",
##     TRUE ~ player.name
##   )) %>%
##   ## pass.recipient.name
##   mutate(pass.recipient.name = case_when(
##     pass.recipient.name == "Oleguer Presas Renom" ~ "Oleguer",
##     pass.recipient.name == "Xavier Hernández Creus" ~ "Xavi",
##     pass.recipient.name == "Carles Puyol i Saforcada" ~ "Carles Puyol",
##     pass.recipient.name == "Anderson Luís de Souza" ~ "Deco",
##     pass.recipient.name == "Rafael Márquez Álvarez" ~ "Rafa Márquez",
##     pass.recipient.name == "Giovanni van Bronckhorst" ~ "Gio v.Bronckhorst",
##     pass.recipient.name == "Samuel Eto'o Fils" ~ "Samuel Eto'o",
##     pass.recipient.name == "Víctor Valdés Arribas" ~ "Víctor Valdés",
##     pass.recipient.name == "Juliano Haus Belletti" ~ "Juliano Belletti",
##     pass.recipient.name == "Ludovic Giuly" ~ "Ludovic Giuly",
##     pass.recipient.name == "Andrés Iniesta Luján" ~ "Andrés Iniesta",
##     pass.recipient.name == "Ronaldo de Assis Moreira" ~ "Ronaldinho",
##     pass.recipient.name == "Lionel Andrés Messi Cuccittini" ~ "Lionel Messi",
##     pass.recipient.name == "Fernando Navarro i Corbacho" ~ "Fernando Navarro",
##     pass.recipient.name == "Sylvio Mendes Campos Junior" ~ "Sylvinho",
##     pass.recipient.name == "Damià Abella Pérez" ~ "Damià",
##     pass.recipient.name == "Rubén Iván Martínez Andrade" ~ "Ronaldinho",
##     pass.recipient.name == "Ronaldo de Assis Moreira" ~ "Rubén",
##     pass.recipient.name == "Thiago Motta" ~ "Thiago Motta",
##     pass.recipient.name == "Mark van Bommel" ~ "Mark van Bommel",
##     pass.recipient.name == "Henrik Larsson" ~ "Henrik Larsson",
##     pass.recipient.name == "José Edmílson Gomes de Moraes" ~ "Edmílson",
##     pass.recipient.name == "Gabriel Francisco García de la Torre" ~ "Gabri",
##     pass.recipient.name == "Santiago Ezquerro Marín" ~ "Santi Ezquerro",
##     pass.recipient.name == "Maximiliano Gastón López" ~ "Maxi López",
##     pass.recipient.name == "Gianluca Zambrotta" ~ "Gianluca Zambrotta",
##     pass.recipient.name == "Eiður Smári Guðjohnsen" ~ "Eiður Guðjohnsen",
##     pass.recipient.name == "Lilian Thuram" ~ "Lilian Thuram",
##     pass.recipient.name == "Javier Pedro Saviola Fernández" ~ "Javier Saviola",
##     pass.recipient.name == "Gnégnéri Yaya Touré" ~ "Yaya Touré",
##     pass.recipient.name == "Bojan Krkíc Pérez" ~ "Bojan",
##     pass.recipient.name == "Eric-Sylvain Bilal Abidal" ~ "Eric Abidal",
##     pass.recipient.name == "Gabriel Alejandro Milito" ~ "Gabriel Milito",
##     pass.recipient.name == "Giovani dos Santos Ramírez" ~ "Giovani dos Santos",
##     pass.recipient.name == "Víctor Vázquez Solsona" ~ "Víctor Vázquez",
##     pass.recipient.name == "Thierry Henry" ~ "Thierry Henry",
##     pass.recipient.name == "José Manuel Pinto Colorado" ~ "José Manuel Pinto",
##     pass.recipient.name == "Daniel Alves da Silva" ~ "Dani Alves",
##     pass.recipient.name == "Sergio Busquets i Burgos" ~ "Sergio Busquets",
##     pass.recipient.name == "Seydou Kéita" ~ "Seydou Kéita",
##     pass.recipient.name == "José Martín Cáceres Silva" ~ "Martín Cáceres",
##     pass.recipient.name == "Gerard Piqué Bernabéu" ~ "Gerard Piqué",
##     pass.recipient.name == "Aliaksandr Hleb" ~ "Aliaksandr Hleb",
##     pass.recipient.name == "Pedro Eliezer Rodríguez Ledesma" ~ "Pedro",
##     pass.recipient.name == "Sergio Rodríguez García" ~ "Rodri",
##     pass.recipient.name == "Rafael Romero Serrano" ~ "Fali",
##     pass.recipient.name == "José Manuel Rueda Sampedro" ~ "José Manuel Rueda",
##     pass.recipient.name == "Zlatan Ibrahimovic" ~ "Zlatan Ibrahimovic",
##     pass.recipient.name == "Dmytro Chygrynskiy" ~ "Dmytro Chygrynskiy",
##     pass.recipient.name == "Maxwell Scherrer Cabelino Andrade" ~ "Maxwell",
##     pass.recipient.name == "Jeffren Isaac Suárez Bermúdez" ~ "Jeffren",
##     pass.recipient.name == "Víctor Sánchez Mata" ~ "Víctor Sánchez",
##     pass.recipient.name == "Thiago Alcântara do Nascimento" ~ "Thiago Alcântara",
##     pass.recipient.name == "David Villa Sánchez" ~ "David Villa",
##     pass.recipient.name == "Javier Alejandro Mascherano" ~ "Javier Mascherano",
##     pass.recipient.name == "Andreu Fontàs Prat" ~ "Andreu Fontàs",
##     pass.recipient.name == "Ibrahim Afellay" ~ "Ibrahim Afellay",
##     pass.recipient.name == "Manuel Agudo Durán" ~ "Nolito",
##     pass.recipient.name == "Marc Bartra Aregall" ~ "Marc Bartra",
##     pass.recipient.name == "Adriano Correia Claro" ~ "Adriano",
##     pass.recipient.name == "Martín Montoya Torralbo" ~ "Martín Montoya",
##     pass.recipient.name == "Jonathan dos Santos Ramírez" ~ "Jonathan dos Santos",
##     pass.recipient.name == "Francesc Fàbregas i Soler" ~ "Cesc Fàbregas",
##     pass.recipient.name == "Alexis Alejandro Sánchez Sánchez" ~ "Alexis Sánchez",
##     pass.recipient.name == "Juan Isaac Cuenca López" ~ "Isaac Cuenca",
##     pass.recipient.name == "Gerard Deulofeu Lázaro" ~ "Gerard Deulofeu",
##     pass.recipient.name == "Cristian Tello" ~ "Cristian Tello",
##     pass.recipient.name == "Sergi Roberto Carnicer" ~ "Sergi Roberto",
##     pass.recipient.name == "Marc Muniesa Martínez" ~ "Marc Muniesa",
##     TRUE ~ pass.recipient.name
##   ))


## ---- eval=FALSE---------------------------------------------------------
## saveRDS(messi_data_clean, file = here::here("data/messi_data_clean.RDS"))


## ---- echo=FALSE---------------------------------------------------------
messi_data_clean <- readRDS(file = here::here("data/messi_data_clean.RDS"))


## ------------------------------------------------------------------------
clasico_1112 <- messi_data_clean %>% 
  filter(match_id == 69334) %>% 
  mutate(shot.statsbomb_xg = if_else(is.na(shot.statsbomb_xg), 
                                     0, shot.statsbomb_xg))

clasico_1112_xg <- clasico_1112 %>% 
  group_by(team.name) %>% 
  summarize(tot_xg = sum(shot.statsbomb_xg) %>% signif(digits = 2)) %>% 
  mutate(team_label = glue::glue("{team.name}: {tot_xg} xG"))

clasico_1112 <- clasico_1112 %>% 
  left_join(clasico_1112_xg, by = "team.name") %>% 
  mutate(player_label = case_when(
    shot.outcome.name == "Goal" ~ glue::glue("{player.name}: {shot.statsbomb_xg %>% signif(digits = 2)} xG"),
    TRUE ~ ""))


## ---- fig.height=6, fig.width=10-----------------------------------------
windowsFonts(robotoc = windowsFont("Roboto Condensed"))

clasico_xg_timelineplot <- clasico_1112 %>% 
  ggplot() +
  geom_segment(x = 0, xend = 95,
               y = 0, yend = 0) +
  geom_rect(data = clasico_1112 %>% filter(shot.outcome.name == "Goal"),
            aes(xmin = minute - 2, xmax = minute + 2,
                ymin = -0.005, ymax = 0.005), 
            alpha = 0.3, fill = "green") +
  geom_label_repel(data = clasico_1112 %>% filter(shot.outcome.name == "Goal"),
             aes(x = minute, y = 0,
                 color = team.name, label = player_label), 
             nudge_x = 4, nudge_y = 0.003, family = "robotoc",
             show.legend = FALSE) +
  geom_point(data = clasico_1112 %>% filter(shot.statsbomb_xg != 0),
             shape = 21, stroke = 1.5,
             aes(x = minute, y = 0, 
                 size = shot.statsbomb_xg, fill = team.name)) +
  scale_color_manual(values = c("Barcelona" = "#a50044",
                                "Real Madrid" = "black")) +
  scale_fill_manual(values = c("Barcelona" = "#a50044",
                                "Real Madrid" = "white")) +
  facet_wrap(vars(team_label), ncol = 1) +
  scale_x_continuous(breaks = seq(0, 95, by = 5),
                     labels = c(seq(0, 40, by = 5), "HT", 
                                seq(50, 90, by = 5), "FT"),
                     limits = c(-3, 95),
                     expand = c(0.01, 0)) +
  scale_y_continuous(limits = c(-0.005, 0.005),
                     expand = c(0, 0)) +
  scale_size(range = c(2, 6)) +
  labs(caption = "By @R_by_Ryo") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(size = 16, family = "robotoc", 
                                  face = "bold", color = "grey20"),
        plot.caption = element_text(family = "robotoc", color = "grey20",
                                    hjust = 0),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())
  
clasico_xg_timelineplot


## ------------------------------------------------------------------------
clasico_rollsum <- clasico_1112 %>% 
  group_by(minute, team.name, period) %>% 
  summarize(sumxg = sum(shot.statsbomb_xg)) %>% 
  ungroup() %>% 
  group_by(team.name) %>% 
  mutate(rollsum = lag(cumsum(sumxg)),
         rollsum = if_else(is.na(rollsum), 0, rollsum)) %>% 
  select(team.name, minute, rollsum, sumxg) %>%
  mutate(rollsum = case_when(
    row_number() == n() & sumxg != 0 ~ rollsum + sumxg,
    TRUE ~ rollsum
  ))

clasico_rollsum <- clasico_rollsum %>% 
  left_join(clasico_1112 %>% filter(shot.outcome.name == "Goal") %>% select(minute, shot.outcome.name, team.name, player.name), 
            by = c("minute", "team.name")) %>% 
  mutate(rollsum_goal = rollsum + sumxg,
         minute_goal = minute + 1,
         player_label = case_when(
           shot.outcome.name == "Goal" ~ glue::glue("{player.name}: {sumxg %>% signif(digits = 2)} xG"),
           TRUE ~ ""))

glimpse(clasico_rollsum)


## ---- fig.height=6, fig.width=10-----------------------------------------
tot_clasico_df <- clasico_1112_xg %>% 
  pull(tot_xg)

clasico_rollsumxg_plot <- clasico_rollsum %>% 
  ggplot(aes(x = minute, y = rollsum, 
             group = team.name, color = team.name)) +
  geom_line(size = 2.5) +
  geom_label_repel(data = clasico_rollsum %>% filter(shot.outcome.name == "Goal"),
             aes(x = minute_goal, y = rollsum_goal, 
                 color = team.name, label = player_label), 
             nudge_x = 6, nudge_y = 0.15, family = "Roboto Condensed",
             show.legend = FALSE) +
  geom_point(data = clasico_rollsum %>% filter(shot.outcome.name == "Goal"),
             aes(x = minute_goal, y = rollsum_goal, color = team.name), show.legend = FALSE,
             size = 5, shape = 21, fill = "white", stroke = 1.25) +
  scale_color_manual(values = c("Barcelona" = "#a50044",
                                 "Real Madrid" = "#000000"),
                     labels = c("<b style ='color:#a50044'>Barcelona</b>", 
                                "<b style='color: black'>Real Madrid</b>")) +
  scale_fill_manual(values = c("Barcelona" = "#a50044",
                               "Real Madrid" = "#000000")) +
  scale_x_continuous(breaks = c(seq(0, 90, by = 5), 94),
                     labels = c(seq(0, 40, by = 5), "HT", 
                                seq(50, 90, by = 5), "FT"),
                     expand = c(0.01, 0),
                     limits = c(0, 94)) +
  scale_y_continuous(sec.axis = sec_axis(~ ., breaks = tot_clasico_df)) +
  labs(title = "<b style='color: black'>Real Madrid: 1 </b><b style='color: black; font-size: 20'>(1st, 40 pts.)</b><br> <b style ='color:#a50044'>Barcelona: 3 </b><b style ='color:#a50044; font-size: 20'>(2nd, 34 pts.)</b>",
       subtitle = "December 10, 2011 (Matchday 16)",
       x = NULL,
       y = "Expected Goals") +
  theme_minimal() +
  theme(text = element_text(family = "Roboto Condensed"),
        plot.title = element_markdown(size = 40, family = "Roboto Condensed"),
        plot.subtitle = element_text(size = 18, family = "Roboto Condensed",
                                     color = "grey20"),
        axis.title = element_text(size = 18, color = "grey20"),
        axis.text = element_text(size = 16, face = "bold"),
        panel.grid.minor = element_blank(),
        legend.text = element_markdown(size = 16),
        legend.position = c(0.2, 0.95),
        legend.direction = "horizontal",
        legend.title = element_blank())

clasico_rollsumxg_plot


## ---- echo=FALSE, eval=FALSE---------------------------------------------
## ggsave(plot = clasico_rollsumxg_plot,
##        filename = here::here("Lionel Messi/output/clasico_rollsumxg_plot.png"),
##        height = 6, width = 10)


## ------------------------------------------------------------------------
roll_final_pass <- clasico_1112 %>% 
  group_by(team.name, minute) %>% 
  mutate(count = case_when(
    type.name == "Pass" & location.x >= 80 ~ 1L,
    TRUE ~ 0L
  )) %>% 
  select(team.name, minute, count) %>% 
  ungroup()


## ------------------------------------------------------------------------
first_min <- clasico_1112$minute %>% unique() %>% first()
last_min <- clasico_1112$minute %>% unique() %>% last()
minute <- c(first_min:last_min)
team.name <- c("Real Madrid", "Barcelona")

crossing(minute, team.name) %>% slice(26:32)


## ------------------------------------------------------------------------
rolling_sum <- tibbletime::rollify(.f = sum, window = 5)

roll_clasico_pass <- crossing(minute, team.name) %>%
  left_join(roll_final_pass, by = c("minute", "team.name")) %>% 
  group_by(team.name, minute) %>% 
  summarize_all(sum) %>% 
  ungroup() %>% 
  mutate(count = ifelse(is.na(count), 0, count)) %>% 
  group_by(team.name) %>% 
  mutate(rollsum = rolling_sum(count),
         rollsum = ifelse(is.na(rollsum), 0, rollsum)) %>% 
  group_by(team.name) %>% 
  select(-count) %>% 
  filter(row_number() %% 5 == 1 | row_number() == n())

roll_clasico_pass %>% head(5)


## ---- fig.height=6, fig.width=10-----------------------------------------
windowsFonts(robotoc = windowsFont("Roboto Condensed"))

finalthird_rollingplot <- roll_clasico_pass %>% 
  ggplot(aes(x = minute, y = rollsum, 
             group = team.name)) +
  geom_line(data = roll_clasico_pass,
            size = 1.2) +
  geom_point(data = roll_clasico_pass,
             aes(fill = team.name),
             size = 3.5, shape = 21, stroke = 2.5) +
  scale_x_continuous(breaks = seq(0, 95, by = 5),
                     labels = c(seq(0, 40, by = 5), "HT", 
                                seq(50, 90, by = 5), "FT"),
                     limits = c(-3, 95),
                     expand = c(0.01, 0)) +
  scale_y_continuous(breaks = seq(0, 30, by = 5),
                     labels = seq(0, 30, by = 5)) +
  scale_fill_manual(values = c("Barcelona" = "#a50044",
                               "Real Madrid" = "white"),
                    labels = c("<b style ='color:#a50044'>Barcelona</b>", 
                               "<b style='color: black'>Real Madrid</b>")) +
  labs(title = "<b style='color: black'>Real Madrid: 1 </b><b style='color: black; font-size: 20'>(1st, 40 pts.)</b><br> <b style ='color:#a50044'>Barcelona: 3 </b><b style ='color:#a50044; font-size: 20'>(2nd, 34 pts.)</b>",
       subtitle = "December 10, 2011 (Matchday 16)",
       x = NULL,
       y = "Final Third Passes") +
  theme_minimal() +
  theme(text = element_text(family = "robotoc"),
        plot.title = element_markdown(size = 40, family = "robotoc"),
        plot.subtitle = element_text(size = 18, family = "robotoc",
                                     color = "grey20"),
        axis.title = element_text(size = 18, color = "grey20"),
        axis.text = element_text(size = 16, face = "bold"),
        panel.grid.minor = element_blank(),
        legend.text = element_markdown(size = 14),
        legend.position = c(0.25, 0.95),
        legend.direction = "horizontal",
        legend.title = element_blank())

finalthird_rollingplot


## ------------------------------------------------------------------------
library(gtable)
library(grid)

png(filename = here::here("Lionel Messi/output/clasico_match_plot_RAW.png"), 
    width = 1000, height = 1600, res = 144, bg = "white")

one <- ggplotGrob(finalthird_rollingplot)
two <- ggplotGrob(clasico_xg_timelineplot)

gg <- rbind(one, two, size = "last")
gg$widths <- unit.pmax(one$widths, two$widths)

grid.newpage()
grid.draw(gg)
dev.off()


## ---- fig.height=14, fig.width=10, message=FALSE, eval=FALSE-------------
## ## ...delete all {ggtext} code and resave ggplot objects...
## clasico_match_plot <- plot_grid(finalthird_rollingplot,
##           clasico_xg_timelineplot, ncol = 1,
##           align = "hv", axis = "l")
## 
## ggsave(plot = clasico_match_plot,
##        filename = here::here("Lionel Messi/output/clasico_match_plotRAW.png"),
##        height = 14, width = 10)


## ------------------------------------------------------------------------
add_logo <- function(plot_path, logo_path, logo_position, logo_scale = 10){

    # Requires magick R Package https://github.com/ropensci/magick

    # Useful error message for logo position
    if (!logo_position %in% c("top right", "top left", "bottom right", "bottom left")) {
        stop("Error Message: Uh oh! Logo Position not recognized\n  Try: logo_positon = 'top left', 'top right', 'bottom left', or 'bottom right'")
    }

    # read in raw images
    plot <- magick::image_read(plot_path)
    logo_raw <- magick::image_read(logo_path)

    # get dimensions of plot for scaling
    plot_height <- magick::image_info(plot)$height
    plot_width <- magick::image_info(plot)$width

    # default scale to 1/10th width of plot
    # Can change with logo_scale
    logo <- magick::image_scale(logo_raw, as.character(plot_width/logo_scale))

    # Get width of logo
    logo_width <- magick::image_info(logo)$width
    logo_height <- magick::image_info(logo)$height

    # Set position of logo
    # Position starts at 0,0 at top left
    # Using 0.01 for 1% - aesthetic padding

    if (logo_position == "top right") {
        x_pos = plot_width - logo_width - 0.01 * plot_width
        y_pos = 0.01 * plot_height
    } else if (logo_position == "top left") {
        x_pos = 0.01 * plot_width
        y_pos = 0.01 * plot_height
    } else if (logo_position == "bottom right") {
        x_pos = plot_width - logo_width - 0.01 * plot_width
        y_pos = plot_height - logo_height - 0.001 * plot_height
    } else if (logo_position == "bottom left") {
        x_pos = 0.01 * plot_width
        y_pos = plot_height - logo_height - 0.01 * plot_height
    }

    # Compose the actual overlay
    magick::image_composite(plot, logo, offset = paste0("+", x_pos, "+", y_pos))
}


## ------------------------------------------------------------------------
plot_logo <- add_logo(
  plot_path = here::here("Lionel Messi/output/clasico_match_plot_RAW.png"),
  logo_path = here::here("img/stats-bomb-logo.png"),
  logo_position = "bottom right",
  logo_scale = 5)

plot_logo

## Save Plot
magick::image_write(
  image = plot_logo, 
  path = here::here("Lionel Messi/output/clasico_match_plot_FIN.png"))


## ------------------------------------------------------------------------
pass_received_all_box <- messi_data_clean %>% 
  mutate(pass.outcome.name = fct_explicit_na(pass.outcome.name, "Complete")) %>%
  filter(type.name == "Pass",
         team.name == "Barcelona",
         pass.outcome.name == "Complete",
         ## Only passes from open play
         !play_pattern.name %in% c("From Corner", "From Free Kick",
                                   "From Throw In"),
         ## Only passes that ended up inside the box:
         pass.end_location.x >= 102 & pass.end_location.y <= 62 &
           pass.end_location.y >= 18) %>% 
  select(player.name, pass.recipient.name, 
         season_id, season_name,
         position.name, position.id,
         location.x, location.y,
         pass.end_location.x, pass.end_location.y,
         contains("pass")) %>% 
  group_by(season_name) %>% 
  add_count(player.name, pass.recipient.name, name = "pass_num") %>% 
  ungroup() %>% 
  mutate(player.name = glue::glue("{player.name}: {pass_num}")) %>% 
  mutate(pass_duo = map2(player.name, pass.recipient.name, ~c(.x, .y))) %>% 
  select(player.name, pass.recipient.name, pass_num, 
         season_name, pass_duo)


## ------------------------------------------------------------------------
pass_received_all_box %>% 
  group_by(season_name) %>% 
  nest()


## ---- fig.height=6, fig.width=8------------------------------------------
all_pass_nested_box <- pass_received_all_box %>% 
  group_by(season_name) %>% 
  nest() %>%
  mutate(plot = map2(
    .x = data, .y = season_name,
    ~ ggplot(data = .x, aes(x = pass_duo)) +
      geom_bar(fill = "#a70042") + 
      scale_x_upset(n_intersections = 10,
                    expand = c(0.01, 0.01)) +
      scale_y_continuous(expand = c(0.04, 0.04)) +
      labs(title = glue::glue("
                              Total Completed Passes Into The Box 
                              Between All Players ({.y})"),
           subtitle = "'Name: Number' = Passer, 'No Number' = Pass Receiver",
           x = NULL, y = "Number of Passes") +
      theme_combmatrix(
        text = element_text(family = "Roboto Condensed", 
                            color = "#004c99"),
        plot.title = element_text(family = "Roboto Condensed", size = 20,
                                  color = "#a70042"),
        plot.subtitle = element_text(family = "Roboto Condensed", size = 16,
                                     color = "#004c99"),
        axis.title = element_text(family = "Roboto Condensed", size = 14,
                                  color = "#004c99"), 
        axis.text.x = element_text(family = "Roboto Condensed", size = 12,
                                   color = "#004c99"),
        axis.text.y = element_text(family = "Roboto Condensed", size = 12,
                                   color = "#004c99"),
        panel.background = element_rect(fill = "white"),
        combmatrix.panel.point.size = 4,
        combmatrix.panel.point.color.fill = "#a70042",
        combmatrix.panel.line.color = "#a70042",
        panel.grid = element_line(color = "black"),
        panel.grid.major.x = element_blank(),
        axis.ticks = element_blank())))

glimpse(all_pass_nested_box)


## ---- fig.height=6, fig.width=8------------------------------------------
all_pass_nested_1112 <- all_pass_nested_box$plot[[8]] +
  scale_y_continuous(labels = seq(0, 15, by = 5),
                     breaks = seq(0, 15, by = 5),
                     limits = c(0, 15))

ggsave(plot = all_pass_nested_1112,
       filename = here::here("Lionel Messi/output/allpass_1112_plotRAW.png"),
       height = 6, width = 8)

plot_logo <- add_logo(
  plot_path = here::here("Lionel Messi/output/allpass_1112_plotRAW.png"),
  logo_path = here::here("img/stats-bomb-logo.png"),
  logo_position = "top right",
  logo_scale = 5)

plot_logo

## Save Plot
magick::image_write(
  image = plot_logo, 
  path = here::here("Lionel Messi/output/allpass_1112_plotFIN.png"))


## ---- fig.height=6, fig.width=8------------------------------------------
## Data
messi_all_shot_assist <- messi_data_clean %>% 
  mutate(pass.outcome.name = fct_explicit_na(pass.outcome.name, "Complete")) %>%
  filter(team.name == "Barcelona",
         !is.na(pass.shot_assist),
         !play_pattern.name %in% c("From Corner", "From Free Kick",
                                   "From Throw In")) %>% 
  select(player.name, pass.recipient.name, 
         season_id, season_name,
         position.name, position.id,
         location.x, location.y,
         pass.end_location.x, pass.end_location.y,
         contains("pass")) %>% 
  group_by(season_name) %>% 
  add_count(player.name, pass.recipient.name, name = "pass_num") %>% 
  ungroup() %>% 
  mutate(player.name = glue::glue("{player.name}: {pass_num}")) %>% 
  mutate(pass_duo = map2(player.name, pass.recipient.name, ~c(.x, .y))) %>% 
  select(player.name, pass.recipient.name, pass_num, 
         season_name, pass_duo)

## Nest plots
messi_nested_all_shot_assist <- messi_all_shot_assist %>% 
  group_by(season_name) %>% 
  nest() %>%
  mutate(plot = map2(
    data, season_name,
    ~ ggplot(data = .x, aes(x = pass_duo)) +
      geom_bar(fill = "#a70042") + 
      scale_x_upset(n_intersections = 10,
                    expand = c(0.01, 0.01)) +
      scale_y_continuous(expand = c(0.04, 0.04)) +
      labs(title = glue::glue("Shot Assists ({.y})"),
           subtitle = "'Name: Number' = Passer, 'No Number' = Pass Receiver",
           caption = "Source: StatsBomb",
           x = NULL, y = "Number of Passes") +
      theme_combmatrix(
        text = element_text(family = "Roboto Condensed", 
                            color = "#004c99"),
        plot.title = element_text(family = "Roboto Condensed", size = 20,
                                  color = "#a70042"),
        plot.subtitle = element_text(family = "Roboto Condensed", size = 16,
                                     color = "#004c99"),
        axis.title = element_text(family = "Roboto Condensed", size = 14,
                                  color = "#004c99"), 
        axis.text.x = element_text(family = "Roboto Condensed", size = 12,
                                   color = "#004c99"),
        axis.text.y = element_text(family = "Roboto Condensed", size = 12,
                                   color = "#004c99"),
        panel.background = element_rect(fill = "white"),
        combmatrix.panel.point.size = 4,
        combmatrix.panel.point.color.fill = "#a70042",
        combmatrix.panel.line.color = "#a70042",
        panel.grid = element_line(color = "black"),
        panel.grid.major.x = element_blank(),
        axis.ticks = element_blank())))

## Plot 2011/2012
messi_nested_all_shot_assist$plot[[8]] +
  scale_y_continuous(labels = seq(0, 12, by = 2),
                     breaks = seq(0, 12, by = 2),
                     limits = c(0, 12))

