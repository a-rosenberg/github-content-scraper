# libraries ---------------------------------------------------------------

library(tidyverse)
library(janitor)
library(showtext)
library(ggTimeSeries)

# import ------------------------------------------------------------------

url    <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/fed_r_d_spending.csv"

df_or  <- read_csv(url) %>% clean_names()

# pre-plot ------------------------------------------------------------

df_or %>% 
  select(-c(total_outlays,discretionary_outlays,gdp)) %>% 
  filter(department %in% c("NASA")) %>% 
  mutate(rd_bil = rd_budget/(10^9)) -> df

glimpse(df)

summary(df)

n <- df %>% nrow()


df %>% 
  mutate(rd1 = c(rd_bil[1:(n-1)],NA)) %>% 
  mutate(rd2 = c(rd_bil[2:n],NA)) %>% 
  mutate(raz = rd1/rd2) %>% 
  mutate(just = case_when(
    raz < 1 ~ 1.5,
    raz >= 1 ~  -1
  )) -> df

x_axis <- c(1980,1983,2000,1990,2011) 
x_labl <- c("Shuttle Era\nBegins","Staff\nincrease","The 21st\nCentury",
            "A New\nName","SpaceX\nContract")

# plot --------------------------------------------------------------------

df %>% 
  ggplot_waterfall("year","rd_bil")+
  geom_text(data = df,
            aes(x = year, y = rd1, label = year, vjust = just))+
  geom_text(data = df,
            aes(x = year, y = rd2, label = year +1,vjust = - (just-.25) ))+
  labs(x = "",
       y = "Research and Development Budget (Billions/$)",
       fill = "",
       title = "NASA Timeline| R&D Budget | 1976-2017",
       subtitle = "TidyTuesday - Week 7: Federal R&D Spendings",
       caption = "graphic: H0Vinicius | source: AAAS & NASA.")+
  theme_bw(18)+
  theme(legend.position = "none",
        plot.margin = unit( c(.5,.5,.5,.5),"cm"),
        panel.grid = element_blank())+
  scale_x_continuous(limits = c(1975.5,2016.5),
                     breaks = x_axis,
                     labels = x_labl,
                     expand = c(0,0))+
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(5,15,by =1),
                     limits = c(5,15))+
    NULL



