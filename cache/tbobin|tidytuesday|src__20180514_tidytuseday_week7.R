

library(tidyverse)
library(purrrlyr)
library(viridis)



df_sw_raw <- read.csv("./Data/week7_starwars.csv")

# replace all "" with NA
df_sw_raw[df_sw_raw == "" ] <- NA


# Not Fans #

# if not seen any of the films not a Fan
# if not self considered as fan not a Fan

# Star Wars: Episode I The Phantom Menace
# Star Wars: Episode II Attack of the Clones
# Star Wars: Episode III Revenge of the Sith
# Star Wars: Episode IV A New Hope
# Star Wars: Episode V The Empire Strikes Back
# Star Wars: Episode VI Return of the Jedi


df_sw_not_Fans <- df_sw_raw %>% 
  mutate(Fan = ifelse(
    (Have.you.seen.any.of.the.6.films.in.the.Star.Wars.franchise. == "No") |
      (Do.you.consider.yourself.to.be.a.fan.of.the.Star.Wars.film.franchise. == "No") |
      is.na(Do.you.consider.yourself.to.be.a.fan.of.the.Star.Wars.film.franchise.), "No", "Yes"
    )) %>% 
  filter(Fan == "No") %>% 
  mutate(have.you.seen.Episode.I = ifelse(!is.na(Which.of.the.following.Star.Wars.films.have.you.seen..Please.select.all.that.apply.),
                                          1, 0 ),
         have.you.seen.Episode.II = ifelse(!is.na(X), 1, 0),
         have.you.seen.Episode.III = ifelse(!is.na(X.1), 1, 0),
         have.you.seen.Episode.IV = ifelse(!is.na(X.2), 1, 0),
         have.you.seen.Episode.V = ifelse(!is.na(X.3), 1, 0),
         have.you.seen.Episode.VI = ifelse(!is.na(X.4), 1, 0)) %>% 
  select(RespondentID, Fan:have.you.seen.Episode.VI)



df_Fan_seen_old <- df_sw_not_Fans %>% 
  select(have.you.seen.Episode.I:have.you.seen.Episode.III) %>% 
  by_row(sum, .collate = "cols", .to = "sum_seen_old")

df_Fan_seen_new <- df_sw_not_Fans %>% 
  select(have.you.seen.Episode.IV:have.you.seen.Episode.VI) %>% 
  by_row(sum, .collate = "cols", .to = "sum_seen_new")


df_sw_not_Fans <- cbind(df_sw_not_Fans, 
                        new = df_Fan_seen_new$sum_seen_new, 
                        old = df_Fan_seen_old$sum_seen_old) %>% 
  mutate(`one of episodes I - III and one of IV - VI` = ifelse(new > 0 & old > 0 , 1, 0),
         `only one of episodes I - III` = ifelse(new > 0 & old == 0 , 1, 0),
         `only one of episodes IV - VI`= ifelse(new == 0 & old > 0 , 1, 0),
         `no one` = ifelse(new == 0 & old == 0 , 1, 0))

df_sw_not_Fans %>% 
  select(RespondentID, `one of episodes I - III and one of IV - VI`:`no one`) %>% 
  gather(`one of episodes I - III and one of IV - VI`:`no one`,
         key = "seen", value = "count") %>% 
  mutate(seen = factor(seen, levels = c("one of episodes I - III and one of IV - VI", 
                                        "only one of episodes I - III", 
                                        "only one of episodes IV - VI",
                                        "no one"), ordered = T)) %>% 
  filter(count > 0) %>% 
  ggplot(aes(x = seen, fill = seen)) + 
  geom_bar(stat = "count") +
  geom_text(aes(label = ..count..), stat = "count", vjust = -1) +
  viridis::scale_fill_viridis(discrete = T ) +
  scale_y_continuous(limits = c(0, 360)) +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        legend.position = "", 
        axis.text.y = element_blank()) +
  labs(y = "", x = "",
       title = "Which episodes have people seen, that are not a fan of the franchise?",
       subtitle = "A fan is someone who constider himself as a fan, all others are not seen as a fan.
Most of the people, who consider themself not as as Fan and have seen at least one movie, have seen at least one of 
the new and one of the old episodes. The fewest people of this group saw only the old episodes.",
       caption = "@T_bobin\nsource: https://github.com/rudeboybert/fivethirtyeight") 
  



