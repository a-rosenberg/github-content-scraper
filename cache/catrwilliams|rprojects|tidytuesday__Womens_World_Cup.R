## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=FALSE)
setwd("~/../Google Drive/Data Analysis/Tidy Tuesday/07-09-19 - Womens World Cup")


## ----library-------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load("data.table","tidyverse","visdat")

theme_set(theme_classic())


## ----import--------------------------------------------------------------
df_wwc_outcomes <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/wwc_outcomes.csv")
df_squads <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/squads.csv")
df_codes <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/codes.csv")

df_wwc_outcomes <- left_join(df_wwc_outcomes, df_codes, by = "team")


## ----view----------------------------------------------------------------
#df_squads
glimpse(df_squads)
head(df_squads)
summary(df_squads)

sapply(df_squads, function(x) n_distinct(x))

#df_wwc_outcomes
glimpse(df_wwc_outcomes)
head(df_wwc_outcomes)
summary(df_wwc_outcomes)

sapply(df_wwc_outcomes, function(x) n_distinct(x))


## ----missing-------------------------------------------------------------
#Visualize missing values
vis_miss(df_squads, sort_miss=TRUE)
vis_miss(df_wwc_outcomes, sort_miss=TRUE)

#see count of missing values
na_values <- function(df){
  na <- colSums(is.na(df)) %>% sort(decreasing=TRUE)
  na[na>0]
}

na_values(df_squads)


## ----wrangle-------------------------------------------------------------
df_squads <- df_squads %>% mutate(caps = replace_na(caps, 1),
                                  goals = replace_na(goals, 0))

df_wwc_outcomes <- df_wwc_outcomes %>% mutate(country = str_replace(country, "Ivory Coast.*", "Ivory Coast"))

# see total number of games by country
df_games <- df_wwc_outcomes %>% group_by(country) %>% summarize(games = n())

# see total number of wins and losses by country
df_wins <- df_wwc_outcomes %>% filter(win_status == "Won") %>% group_by(country) %>% summarize(wins = n())
df_loss <- df_wwc_outcomes %>% filter(win_status == "Lost") %>% group_by(country) %>% summarize(loss = n())

# get percentage of games won
df_games_won <- inner_join(df_games, df_wins) %>% mutate(percent_won = (wins/games)*100)

# get net wins/losses by country
df_games <- left_join(df_games, df_wins) %>% mutate(wins = replace_na(wins, 0))
df_games <- left_join(df_games, df_loss) %>% mutate(loss = replace_na(loss, 0),
                                                    net = wins-loss)

# capitalize country to be more consistent with FIFA France 2019 theme
df_games <- df_games %>% mutate(country = str_to_upper(country))


## ----viz, out.width="100%"-----------------------------------------------
#Win status per country
df_wwc_outcomes %>% ggplot(aes(country))+
  geom_bar()+
  facet_wrap(~win_status)+
  coord_flip()+
  labs(title="Game Status Per Country", y="number of games")

#Win percentage per country
df_games_won %>% mutate(country = fct_reorder(country,percent_won)) %>% ggplot(aes(country, percent_won))+
  geom_col()+
  coord_flip()+
  labs(title="Percentage of Games Won Per Country", y="percent")

#ages of players
mean <- mean(df_squads$age)
median <- median(df_squads$age)

h <- hist(df_squads$age, breaks = "FD", plot = FALSE) #histogram with Freedman-Diaconis rule for binwidth

df_squads %>% ggplot(aes(age))+
  geom_histogram(aes(y = ..density..), breaks = h$breaks, alpha = 0.5, col = "white")+
  geom_vline(xintercept=mean, color="red", size=2)+
  geom_vline(xintercept=median, color="blue", size=1.5)+
  labs(title="Distribution of Players' Ages", subtitle="mean: red, median: blue")


## ----final---------------------------------------------------------------
# load custom fonts
windowsFonts(Elegance = windowsFont("Elegance"))
windowsFonts(OpenSans = windowsFont("Open Sans"))

df_games %>% mutate(country = fct_reorder(country,net)) %>% 
  ggplot(aes(country, net, fill=net < 0))+
    geom_col(width=0.95, color="white", size=0.3)+
    coord_flip()+
    scale_fill_manual(name = "net < 0", values = setNames(c("#D6000A","#F08C01"), c(T,F)))+
    scale_y_continuous(expand=c(0,0), limits=c(-37,37), breaks=c(-30,-20,-10,0,10,20,30))+
    labs(title="Women's World Cup - Net Wins Per Country", subtitle="From 1991 - 2019", y="NET WINS / LOSSES", fill="Net Game Status")+
    theme(plot.background = element_rect(fill="#23207C"),
          panel.background = element_rect(fill="#23207C"),
          legend.position="none",
          axis.line = element_line(color="white"),
          axis.text = element_text(color="white", family="OpenSans"),
          axis.text.y = element_text(size=7.5),
          axis.ticks = element_line(color="white"),
          axis.title = element_text(family="OpenSans", face="bold", color="white"),
          plot.title = element_text(hjust=0.5, family="Elegance", face="bold", size=16, color="#00B5ED"),
          plot.subtitle = element_text(hjust=0.5, family="Elegance", face="bold", size=13, color="#FDDB00"),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size=8))

ggsave("womens-world-cup.png")

