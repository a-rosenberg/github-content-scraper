#===== First example =====

# Scratch pad and other examples - NFL
library(tidyverse)
library(hrbrthemes)

# download https://github.com/rfordatascience/tidytuesday/blob/master/data/tidy_tuesday_week2.xlsx
football <- read_xlsx("data/tidy_tuesday_week2.xlsx")

# get the top 16 paid players in each position for each year
to_plot <- football %>%
  mutate(Team = 1:nrow(.)) %>%
  gather(position, salary, -c(year, Team)) %>%
  group_by(position, year) %>%
  arrange(desc(salary)) %>%
  mutate(rank = 1:n()) %>%
  filter(rank <= 16) %>%
  ungroup %>%
  mutate(salary = salary/1e6) %>% # convert to millions
  mutate(position_other = 
           fct_other(position, # we only care about running backs and the highest paid, QBs
                     keep=c("Quarterback",
                            "Running Back")))

to_plot %>%
  filter(position_other == "Other") %>% # plot every other position at rear of image
  ggplot(data=., aes(x= year + (9-rank)/16,  # like a dodge
                     y=salary,
                     color = position_other,
                     group = interaction(year, position))) +
  geom_path(size=1, alpha=0.075, color="black") +
  theme_ipsum_rc() +
  ylim(c(0,40)) + # set limits to be pretty-ish
  scale_x_continuous(breaks = unique(to_plot$year)) +
  ylab("Salary ($m)") +
  xlab("Year") +
  geom_path(data = filter(to_plot,
                          position_other != "Other"),
            size=1, alpha=0.75) +
  scale_color_manual(values=c("purple", "red"),
                     name="Position") +
  theme(legend.position = "bottom", 
        panel.grid.minor.x = element_blank()) +
  ggtitle("Top 16 annual salaries per position (2011-2018)", 
          subtitle = "Data: http://www.spotrac.com/rankings/\nGraphic: @samclifford")



#===== Second example =====
# https://github.com/timschoof/TidyTuesday/blob/master/TT02/TT02.R

# Tidy Tuesday - 10 April 2018
# Average pay for top NFL players per position

# Data source: http://www.spotrac.com/rankings/
# Article with graphic: https://fivethirtyeight.com/features/running-backs-are-finally-getting-paid-what-theyre-worth/
# Tidy Tuesday: https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(here)
library(openxlsx)

# load data
df<-read.xlsx(here("tidy_tuesday_week2.xlsx"),sheet="nfl_salary")

# convert data frame to tibble - because I want to learn about tibbles
t<-as_tibble(df)

# reorganize tibble from wide to long format
longT <- t %>%
  gather(position,salary,-year) 

# select 16 highest-paid players in each position (n = 10), per year (n = 8)
sub16 <- longT %>%
  group_by(year,position) %>%
  top_n(16) %>%
  ungroup()

# compute average salary for this subset
meanSub16 <- sub16 %>%
  mutate(salary = salary/10^6) %>% # salary in millions
  group_by(year,position) %>%
  mutate(mean_salary = mean(salary)) %>%
  ungroup

# for plotting
meanSub16plot <- meanSub16 %>%
  mutate(position = fct_recode(position, "Running Back" = "Running.Back",
                               "Defensive Lineman" = "Defensive.Lineman",
                               "Offensive Lineman" = "Offensive.Lineman",
                               "Special Teamer" = "Special.Teamer",
                               "Tight End" = "Tight.End",
                               "Wide Receiver" = "Wide.Receiver")) # rename / recode some factor levels

# plot
meanSub16plot %>%
  ggplot() +
  geom_point(aes(x = year, y = salary), colour = "gray") +
  geom_line(aes(x = year, y = mean_salary), size = 1.2) + 
  facet_wrap(~position, nrow = 2) +
  labs(x = "", y = "Average salary \n (USD in millions)", 
       title = "Average salary of 16 highest-paid NFL players \n by position") +
  theme(panel.background = element_rect(fill = "#fcfcfc" ),
        plot.background = element_rect(fill = "#fcfcfc" ),
        panel.grid.major = element_line(colour = "#d3d3d3"),
        plot.title = element_text(hjust = 0.5))# center the title

# save
ggsave("NFL.png", width = 10, height = 5)



#===== Third example =====
# https://github.com/nikdudaev/tidy_tuesdays/blob/master/week_2/week_2.R

library(tidyverse)
library(readxl)
library(ggrepel)

# Reading data
df_1 <- read_xlsx("../data/tidy_tuesday_week2.xlsx")
# Tidying data. In particular columns will become categories and will be put into one variable - player_position
df_1 <- df_1 %>% gather(`Cornerback`:`Wide Receiver`, key = "player_position", value = "salary", `Cornerback`:`Wide Receiver`)
# Selecting only top 16 salaries
df_2 <- df_1 %>% group_by(year, player_position) %>% top_n(n = 16, wt = salary)
# Making player_position variable a factor
df_2$player_position <- 
  factor(df_2$player_position, levels = c("Running Back", "Quarterback", "Offensive Lineman", "Tight End",
                                          "Wide Receiver", "Cornerback", "Defensive Lineman", "Linebacker",
                                          "Safety", "Special Teamer"))
# Dividing salary by million in order to get a more readable scale
df_2 <- df_2 %>% mutate(salary_mil = round(salary / 1000000))
# Data preparation for plot 2
# Calculating total spendings per year per position
position_year <- df_2 %>% group_by(player_position, year) %>% summarize(total_spent = sum(salary_mil))
# Calculating total spendings per year for all positions
year <- df_2 %>% group_by(year) %>% summarize(spent_per_year = sum(salary_mil))
# Joining with main data frame
df_2 <- df_2 %>% left_join(position_year, by = c("player_position", "year"))
df_2 <- df_2 %>% left_join(year, by = c("year"))
# Calculating percentage per year per position
df_2 <- df_2 %>% mutate(pct_total = (total_spent / spent_per_year) * 100)
# Creating categories of Offense and Defense
df_2 <- df_2 %>% mutate(off_def = ifelse(player_position %in% c("Quarterback", "Wide Receiver", 
                                                                "Offensive Lineman", "Running Back", 
                                                                "Tight End"), "OFFENSE", "DEFENSE"))
df_2$off_def <- factor(df_2$off_def, levels = c("OFFENSE", "DEFENSE"))
# Plot number 1
ggplot(df_2) + 
  geom_point(aes(x = year, y = salary_mil), alpha = 1/4) +
  geom_smooth(aes(x = year, y = salary_mil), se = FALSE, color = "#FF5722") +
  ylim(0, 25) +
  facet_wrap(~ player_position, nrow = 2, ncol = 5) + 
  labs(title = "The average pay for top running backs has stalled",
       subtitle = "Average cap value of 16 highest-paid players in each position",
       y = "Average cap value") +
  theme(axis.title.x = element_blank(),
        strip.text = element_text(face = "bold", size = rel(0.7)),
        strip.background = element_rect("white"),
        panel.background = element_rect("white"),
        panel.grid.major = element_line("grey", size = 0.2),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))
#ggsave("Plot1.png", width = 25, height = 15, units = "cm")

plot_labels <- data.frame(
  player_position = c("RB", "QB", "OL", "TE", "WR", "CB", "DL", "LB", "S", "ST"),
  x = c(rep(2018, 10)),
  y = c(5, 21, 10, 7.5, 12, 10, 13.5, 11, 7.5, 3)
)

# Plot 2
ggplot(df_2, aes(x = year, y = pct_total, color = player_position)) + 
  geom_point() + 
  geom_smooth(se = FALSE) + 
  geom_text(data = plot_labels, aes(x = x, y = y, label = player_position), check_overlap = TRUE) +
  facet_wrap(~ off_def) +
  labs(title = "Teams are spending less on RB's",
       subtitle = "Percent of money spent on the top 16 players at each position",
       y = "Percent spent on each position") +
  theme(axis.title.x = element_blank(),
        strip.text = element_text(face = "bold", size = rel(0.7)),
        strip.background = element_rect("white"),
        panel.background = element_rect("white"),
        panel.grid.major = element_line("grey", size = 0.2),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")
# scale_color_manual(values = c("#000000", "#FF5722", "#FFEB3B", "#4CAF50", "#03A9F4", "#009688",
#                            "#673AB7", "#3F51B5", "#9C27B0", "#E91E63")) 

#ggsave("Plot2.png", width = 25, height = 15, units = "cm")