## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=FALSE)

directory <- "07-16-19 - R4DS Community Stats"

if(!getwd() == paste0("C:/Users/Cat/Google Drive/Data Analysis/Tidy Tuesday/",directory)) {
  setwd(paste(directory))
  }


## ----library-------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse","visdat","grid","gridExtra")

theme_set(theme_minimal())


## ----import--------------------------------------------------------------
df <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-16/r4ds_members.csv")


## ----view----------------------------------------------------------------
glimpse(df)
head(df)
summary(df)
sapply(df, function(x) n_distinct(x)) %>% sort()


## ----missing-------------------------------------------------------------
#Visualize missing values
vis_miss(df, sort_miss=TRUE)


## ----wrangle-------------------------------------------------------------
# remove columns with only 1 unique value
df <- df[, sapply(df, function(x) n_distinct(x)) > 1] %>% select(-full_members)
df <- df %>% filter(date >= "2017-09-01")


## ----viz, out.width="100%"-----------------------------------------------
# active vs total membership
df %>% ggplot(aes(x=date))+
  geom_area(aes(y=total_membership, fill="Total Membership"))+
  geom_area(aes(y=weekly_active_members, fill="Active Members"))+
  scale_fill_manual(values = c("steelblue1","steelblue4")) +
  labs(title="Active Members vs Total Membership", fill="")

# total members posting messages
df %>% ggplot(aes(x=date))+
  geom_line(aes(y=weekly_members_posting_messages), color="red")+
  labs(title="Number of Members Posting Messages")

# messages posted
df %>% ggplot(aes(x=date))+
  geom_line(aes(y=messages_posted), color="purple4", size=1.2)+
  labs(title="Total Messages Posted")

## FINAL VISUALIZATION
# active vs. total membership
p1 <- df %>% ggplot(aes(x=date))+
  geom_area(aes(y=total_membership), fill="steelblue4")+
  geom_area(aes(y=weekly_active_members), fill="steelblue1")+
  labs(x="Date", y="Total Number")+
  theme(axis.title = element_text(size=9))

# messages posted vs. membership
p2 <- df %>% ggplot(aes(x=date))+
  geom_area(aes(y=total_membership, fill="Total Membership"))+
  geom_area(aes(y=weekly_active_members, fill="Active Members"))+
  geom_line(aes(y=messages_posted, color="Messages Posted"), size=1.2)+
  scale_fill_manual(values = c("steelblue1","steelblue4")) +
  scale_color_manual(values = "purple3") +
  labs(x="Date", y="Total Number")+
  theme(legend.position="bottom",
        legend.title=element_blank(),
        legend.spacing.x = unit(0.4, 'cm'),
        axis.title = element_text(size=9))+
  guides(fill = guide_legend(order=1),
         color = guide_legend(order=2))

#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
#function to create a common legend for two plots
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(p2)

tg <- grobTree(textGrob("Membership Status vs. Messages Posted", 
                        y=1, 
                        vjust=1, 
                        gp=gpar(fontface="bold", fontsize = 16)),
               textGrob("Messages posted are rapidly increasing, despite active members staying fairly constant", 
                        y=0, 
                        vjust=0, 
                        gp = gpar(fontsize=11, col="grey20")),
               cl="titlegrob")

heightDetails.titlegrob <- function(x) do.call(sum,lapply(x$children, grobHeight))

#create final plot
p3 <- grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                         p2 + theme(legend.position="none"),
                         nrow=1),
             mylegend, 
             nrow=2, 
             heights=c(10, 1),
             top = tg)

ggsave("R4DS-community-stats.png", p3)

