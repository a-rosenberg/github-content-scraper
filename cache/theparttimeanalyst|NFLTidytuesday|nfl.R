nfl<-read.csv(text=getURL("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-08-28/nfl_2010-2017.csv"))

plotdat <- nfl %>% filter(game_year == 2017) %>%
                   group_by(team) %>%
                    summarise(meanrush = mean(rush_yds, na.rm = TRUE), meanpass = mean(pass_yds, na.rm = TRUE)) %>%
                      arrange(meanrush) %>%
                        mutate(team = factor(team, levels = .$team))

nflcol <- c("ARI" = "#97233F", "ATL" = "#A71930", "BAL" = "#241773", "BUF"= "#00338D","CAR" = "#0085CA", "CHI" = "#0B162A",
           "CIN" = "#FB4F14",  "CLE" = "#311D00", "DAL" = "#003594", "DEN" = "#FB4F14", "DET" = "#0076B6", "GB" = "#203731", 
           "HOU" = "#03202F", "IND" = "#002C5F", "JAX" = "#006778", "KC" = "#E31837", "LA"= "#002A5E", "LAC" = "#024000",
           "MIA" = "#008E97", "MIN"= "#4F2683", "NE" = "#002244", "NO" = "#D3BC8D", "NYG" = "#0B2265", "NYJ" = "#003F2D", 
           "OAK" = "#A5ACAF", "PHI" = "#004C54", "PIT" = "#FFB612", "SEA" ="#69BE28" , "SF" = "#AA0000", "TB" = "#D50A0A", 
           "TEN" = "#002A5C", "WAS" = "#773141")



ggplot(plotdat, aes(x = team, y = meanrush, fill = team)) + geom_col() + 
                          scale_fill_manual(values = nflcol)  +
                                labs()
                                guides(fill = FALSE) + theme_light()

ggplot(plotdat, aes(x = meanrush, y = meanpass, col = team, label = team)) + geom_text(size =6) + 
                          scale_color_manual(values = nflcol) +
  guides(col = FALSE) + theme_light() +
              labs(x = "Mean Rush Yds", y = "Mean Pass YDS", title = "2017 NFL Pass and Rush COmparison")

