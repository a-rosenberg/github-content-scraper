library(readr)
library(dplyr)
library(magrittr)
library(stringr)
library(lubridate)
library(ggplot2)
library(RColorBrewer)

emperors <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-13/emperors.csv")

BCEtransform <- function(date,BCE){
  # transform a positive BCE date to a negative
  # only when BCE is TRUE
  # NB: the year 0000 does not exist; this was the year 1 BCE
  # but the data has taken this into account already
  # and emperors born in BCE have an adjusted year (eg born in 63BCE is 62 in the data)
  if(BCE){
  yr = as.numeric(format(date, "%Y"))
  date = date - years(2*yr)
  }
  return(date)
}

emperors %<>% mutate(bce_birth = ifelse(!is.na(notes),
                                        # if BCE is mentioned, birth years are BCE
                                        ifelse(str_detect(notes,"BCE"),T,F),
                                        F),
                     birth_corrected = as_date( # weirdly, the date object is not returned as a date
                       mapply(BCEtransform,birth,bce_birth)),
                     # same for reign
                     bce_reign = ifelse(!is.na(notes),
                                        # if "reign.start are BCE" is mentioned, reign start is BCE (one guy but still)
                                        ifelse(str_detect(notes,"reign.start are BCE"),T,F),
                                        F),
                     reign_start_corrected = as_date(
                       mapply(BCEtransform,reign_start,bce_reign))
                     )

# express years in age/duration
emperors %<>% mutate(
  ## age at start reign
  age_reign_start = as.numeric(reign_start_corrected-birth_corrected)/365.25,
  ## age at end reign
  age_reign_end = as.numeric(reign_end-birth_corrected)/365.25,
  ## age at death
  age_death = as.numeric(death-birth_corrected)/365.25,
  # reign duration
  reign_duration = as.numeric(reign_end-reign_start_corrected)/365.25
)

# change levels to highest death count first
cause_level <- count(emperors,cause,sort=T)
emperors$cause <- factor(emperors$cause, levels = rev(cause_level$cause))

# adjust esthetics of plot
theme_set(theme_light(base_size = 12, base_family = "Courier"))
cscale_killer <- c(brewer.pal(8,"Spectral"),brewer.pal(5,"BuPu")[2:5],brewer.pal(4,"PiYG"))
cscale_rise <- cscale_killer[c(1,3,4,7,8,10,12,13)]


# make plots
emperors %>%
  ggplot(aes(x = cause, y = age_death)) +
  geom_jitter(aes(size = reign_duration, color=killer), width=0.2) +
  scale_colour_manual(values=cscale_killer) +
  coord_flip() +
  theme(panel.grid.major.x = element_line(linetype="dotted",color="darkgrey"),
        panel.grid.minor.x = element_line(linetype="dotted",color="lightgrey"),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        plot.caption = element_text(size = 8, color = "darkgrey")
  ) +
  labs(title = "Death to Roman Emperors",
       y = "Age at death (years)",
       x = "Cause of death",
       size = "Duration of reign",
       color = "Killed by",
       caption = "data: Wikipedia / credit: Georgios Karamanis") 


emperors %>%
  ggplot(aes(x = cause, y = age_death)) +
  geom_jitter(aes(size = reign_duration, color=rise), width=0.2) +
  scale_colour_manual(values=cscale_rise) +
  coord_flip() +
  theme(panel.grid.major.x = element_line(linetype="dotted",color="darkgrey"),
        panel.grid.minor.x = element_line(linetype="dotted",color="lightgrey"),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        plot.caption = element_text(size = 8, color = "darkgrey")
  ) +
  labs(title = "Rise and fall of Roman Emperors",
       y = "Age at death (years)",
       x = "Cause of death",
       size = "Duration of reign",
       color = "Rise to power",
       caption = "data: Wikipedia / credit: Georgios Karamanis") 


emperors %>%
  ggplot(aes(x = cause, y = reign_duration)) +
  geom_jitter(aes(size = age_death, color=rise), width=0.2, alpha=0.7) +
  scale_colour_manual(values=cscale_rise) +
  coord_flip() +
  theme(panel.grid.major.x = element_line(linetype="dotted",color="darkgrey"),
        panel.grid.minor.x = element_line(linetype="dotted",color="lightgrey"),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        plot.caption = element_text(size = 8, color = "darkgrey")
  ) +
  labs(title = "Rise and fall of Roman Emperors",
       size = "Age at death (years)",
       x = "Cause of death",
       y = "Duration of reign",
       color = "Rise to power",
       caption = "data: Wikipedia / credit: Georgios Karamanis") 

# change levels to highest rise to power-count first
rise_level <- count(emperors,rise,sort=T)
emperors$rise <- factor(emperors$rise, levels = rev(rise_level$rise))

emperors %>%
  ggplot(aes(x = rise, y = reign_duration)) +
  geom_jitter(aes(size = age_death, color=cause), width=0.2, alpha=0.7) +
  scale_colour_manual(values=cscale_rise) +
  coord_flip() +
  theme(panel.grid.major.x = element_line(linetype="dotted",color="darkgrey"),
        panel.grid.minor.x = element_line(linetype="dotted",color="lightgrey"),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        plot.caption = element_text(size = 8, color = "darkgrey")
  ) +
  labs(title = "Rise and fall of Roman Emperors",
       size = "Age at death (years)",
       color = "Cause of death",
       y = "Duration of reign",
       x = "Rise to power",
       caption = "data: Wikipedia / credit: Georgios Karamanis") 
