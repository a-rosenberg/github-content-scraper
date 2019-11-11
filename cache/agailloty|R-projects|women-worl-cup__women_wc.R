options(warn = -1)
suppressPackageStartupMessages(library(tidyverse))
library(ggthemes)
library(hrbrthemes)
library(FactoMineR)
library(factoextra)
options(repr.plot.res = 480)

# Reading the datasets

wwc_outcomes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/wwc_outcomes.csv")
squads <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/squads.csv")
codes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/codes.csv")

# Combine
wwc_outcomes <- dplyr::left_join(wwc_outcomes, codes, by = "team")

# First plot : winning status

wwc_outcomes %>%
group_by(country, win_status) %>% 
count() %>%
ggplot(aes(x = reorder(country, n), y = n)) +
geom_col(aes(fill = win_status), col = "white") + coord_flip() +
labs(title = "Winning status of Women's World Cup",
    subtitle = "The length of the bar indicates the number of disputed match and the 
colors indicates the result of the match",
    caption = "Source : data.world \n #Tidytuesday",
    fill = "Win status") +
theme(legend.title = element_text("Winning status"),
     plot.subtitle = element_text(face = "italic", size = 8))
     
 # Second plot : Correlation circle
 ## spread the pos column into 4 dummy columns
 wide_df <- squads %>%
mutate(dummy = 1) %>%
spread(pos, dummy, fill = 0) %>%
select(-squad_no, -dob, -club, -country) %>%
data.frame(row.names = "player")

## PCA on the wide df
acp <- PCA(wide_df)

fviz_pca_var(acp, fill.var = "blue") + 
theme_ipsum_rc() + theme(line = element_blank()) +
labs(title = "Correlation between players attributes",
    subtitle = "Circle of correlation based on players individual attributes") +
theme(axis.title = element_blank())

# Third plot : Sample of players
set.seed(121)
selected_player <- sample(
    squads %>% select(player) %>% pull,
    size = 60
)
fviz_pca_biplot(acp, labelsize = 2, repel = TRUE, geom = "text",
                select.ind = list(name = selected_player, col = "blue"),
               title = "Sample of players with attributes") +
theme_ipsum_rc() + theme(axis.text = element_blank(), axis.title = element_blank())
