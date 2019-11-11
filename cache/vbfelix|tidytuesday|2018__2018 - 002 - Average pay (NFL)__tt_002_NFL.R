# Packages ----------------------------------------------------------------

library(readxl)
library(tidyverse)


# Function ----------------------------------------------------------------

watermark<-function(logo){
  png::readPNG(logo)%>%
    grid::rasterGrob(image = ., interpolate = T)
}

# Import ------------------------------------------------------------------
list.files()
df_or<-read_xlsx("tidy_tuesday_week2.xlsx")

df_or %>% 
  mutate(Quarterback = Quarterback/10^6) %>% 
  group_by(year) %>% 
  top_n(16,Quarterback)->df

qb<- watermark("quarterback.png")

stadium <- watermark("stadium.png")


# Auxiliar data -----------------------------------------------------------

df %>% 
  group_by(year) %>% 
  summarise(mean = mean(Quarterback),
            max = max(Quarterback)) ->df_summ

df %>% 
  filter(year == 2011 | year == 2018) %>% 
  group_by(year) %>% 
  summarise(max = max(Quarterback),
            min = min(Quarterback),
            mean = mean(Quarterback)) ->df_ext


# Plot --------------------------------------------------------------------


df %>%
  ggplot(aes(year, Quarterback))+
  labs(x = "Year",y="Salary ($ million)",
       col="",
       caption = "Graphic: @H0Vinicius | Source: spotrac.com",
       title = "Top 16 quarterbacks (2011-2018)",
       subtitle = "Week 2 of #TidyTuesday")+
  theme_minimal(16)+
  theme(legend.position = "bottom")+
  scale_x_continuous(breaks = 2011:2018,
                     labels = 2011:2018,
                     limits = c(2010,2018.5))+
  scale_y_continuous(breaks = seq(0,40,by=5),
                     labels = seq(0,40,by=5),
                     limits = c(-5,40) )+
  annotation_custom(qb,xmin = 2009.5,xmax = 2011,ymin = -5,ymax = 10)+
  ggrepel::geom_label_repel(data = df_ext,
                            aes(y= max,label = paste0("$",round(max,2))),
                            nudge_y = 4)+
  ggrepel::geom_label_repel(data = df_ext,
                            aes(y= min,label = paste0("$",round(min,2))),
                            nudge_y = -4)+
  ggrepel::geom_label_repel(data = df_ext,
                            aes(y= mean,label = paste0("$",round(mean,2))),
                            nudge_x = c(-.5,.5))+
  geom_point(size=2, alpha = .75)+
  geom_point(data = df_ext, aes(y = min, col = "Minimum"),size=3)+
  geom_point(data = df_ext,aes(y = mean, col = "Average"),size=3)+
  geom_point(data = df_ext, aes(y = max, col = "Maximum"),size=3)+
  geom_curve(x = 2011+.15,
             xend = 2018-.15,
             y=df_ext$max[1]+2, yend = df_ext$max[2],
             curvature = -.2,
             arrow = arrow(length = unit(0.03, "npc")),
             size = 1.2,
             col = "#CC0000")+
  geom_curve(x = 2011+.15,
             xend = 2018-.15,
             y=df_ext$min[1]+.5, yend = df_ext$min[2],
             curvature = -.05,
             arrow = arrow(length = unit(0.03, "npc")),
             size = 1.2,
             col = "#0066CC")+
  geom_curve(x = 2011+.15,
             xend = 2018-.15,
             y=df_ext$mean[1]+.5, yend = df_ext$mean[2],
             curvature = -.15,
             arrow = arrow(length = unit(0.03, "npc")),
             size = 1.2,alpha=.6,
             col = "#FF6600")+
  scale_color_manual(values = c("#FF6600","#CC0000","#0066CC"))
ggsave("tt002.png",scale = 2.5)


# Plot with back ground ----------------------------------------------------------------
qb2<- watermark("quarterback2.png")
df %>%
  ggplot(aes(year, Quarterback))+
  labs(x = "Year",y="Salary ($ million)",
       col="",
       caption = "Graphic: @H0Vinicius | Source: spotrac.com",
       title = "Top 16 quarterbacks (2011-2018)",
       subtitle = "Week 2 of #TidyTuesday")+
  theme_minimal(16)+
  theme(legend.position = "bottom")+
  theme(plot.margin = unit(c(1,3,1,3), "cm"),
        panel.grid.major = element_line(colour = NA),
        panel.grid.minor = element_line(colour = NA))+
  scale_x_continuous(breaks = 2011:2018,
                     labels = 2011:2018,
                     limits = c(2010,2018.5))+
  scale_y_continuous(breaks = seq(0,40,by=5),
                     labels = seq(0,40,by=5),
                     limits = c(-5,40) )+
  annotation_custom(stadium)+
  geom_hline(yintercept = seq(0,40,by=5),col="white",alpha=.45)+
  geom_vline(xintercept = 2011:2018,col="white",alpha=.45)+
  annotation_custom(qb2,xmin = 2009.5,xmax = 2011,ymin = -5,ymax = 10)+
  ggrepel::geom_label_repel(data = df_ext,
                            aes(y= max,label = paste0("$",round(max,2))),
                            nudge_x = c(-.25,.25))+
  ggrepel::geom_label_repel(data = df_ext,
                            aes(y= min,label = paste0("$",round(min,2))),
                            nudge_y = -2)+
  ggrepel::geom_label_repel(data = df_ext,
                            aes(y= mean,label = paste0("$",round(mean,2))),
                            nudge_x = c(-.25,.25))+
  geom_point(data = df_ext, aes(y = min, col = "Minimum"),size=3)+
  geom_point(data = df_ext,aes(y = mean, col = "Average"),size=3)+
  geom_point(data = df_ext, aes(y = max, col = "Maximum"),size=3)+
  geom_curve(x = 2011+.15,
             xend = 2018-.15,
             y=df_ext$max[1]+2, yend = df_ext$max[2],
             curvature = -.2,
             arrow = arrow(length = unit(0.03, "npc")),
             size = 1.2,
             col = "#CC0000")+
  geom_curve(x = 2011+.15,
             xend = 2018-.15,
             y=df_ext$min[1]+.5, yend = df_ext$min[2],
             curvature = -.05,
             arrow = arrow(length = unit(0.03, "npc")),
             size = 1.2,
             col = "#0066CC")+
  geom_curve(x = 2011+.15,
             xend = 2018-.15,
             y=df_ext$mean[1]+.5, yend = df_ext$mean[2],
             curvature = -.15,
             arrow = arrow(length = unit(0.03, "npc")),
             size = 1.2,alpha=.6,
             col = "#FF6600")+
  scale_color_manual(values = c("#FF6600","#CC0000","#0066CC"))

ggsave("tt002-2.png",scale = 2.5)
