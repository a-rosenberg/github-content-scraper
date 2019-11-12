## ----setup, eval = TRUE--------------------------------------------------
library(tidyverse)
library(ggthemes)
library(viridis)
library(gganimate)


## ------------------------------------------------------------------------
global_mortality <- here::here("week_3/data", "global_mortality.xlsx") %>%
  readxl::read_xlsx(.)


## ------------------------------------------------------------------------
names(global_mortality) <- str_trim(
  str_remove_all(names(global_mortality), "[[:punct:]]")
)
causes <- colnames(global_mortality)[ -c(1, 2, 3)]





## ------------------------------------------------------------------------

mortality_tall <- tidyr::gather(
  data = global_mortality,
  key = "cause",
  value = "percent",
  -c(country, countrycode, year),
  factor_key = TRUE
  )



## ------------------------------------------------------------------------
mortality_country <- group_by(mortality_tall, country, cause) %>%
  summarise(percent = sum(percent, na.rm = TRUE))


## ------------------------------------------------------------------------
regions <- c(
  "Andean Latin America",
  "Central Latin America",
  "Tropical Latin America",
  "Latin America and Caribbean",
  "Southern Latin America",
  "North America",
  "Caribbean",
  "Australasia",
  "Oceania",
  "East Asia",
  "South Asia",
  "Central Asia",
  "Southeast Asia",
  "Eastern Europe",
  "Central Europe",
  "Western Europe",
  "North Africa and Middle East",
  "Eastern Sub-Saharan Africa",
  "Central Sub-Saharan Africa",
  "Western Sub-Saharan Africa",
  "Southern Sub-Saharan Africa",
  "Sub-Saharan Africa"
)

country_groups <- filter(mortality_tall, country %in% regions) %>%
    droplevels()



## ------------------------------------------------------------------------
country_groups$country <- factor(
  country_groups$country,
  levels = regions
)


## ---- fig.show = "animate", interval = 2---------------------------------

p <- ggplot(country_groups, aes(country, cause, frame = year)) +
  geom_tile(aes(fill = percent)) +
  scale_fill_viridis(name = "% Population") +
  theme_tufte() +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("County Group") + ylab("Cause") +
  theme(legend.title = element_blank())

gganimate(p)



## ---- fig.show = "animate", interval = 2---------------------------------
sdi <- c(
  "High SDI",
  "High-middle SDI",
  "Middle SDI",
  "Low-middle SDI",
  "Low SDI"
)

sdi_groups <- filter(mortality_tall, country %in% sdi) %>%
  droplevels()

sdi_groups$country <- factor(
  sdi_groups$country,
  levels = sdi
)

p2 <- ggplot(sdi_groups, aes(country, cause, frame = year)) +
  geom_tile(aes(fill = percent)) +
  scale_fill_viridis(name = "% Population") +
  theme_tufte() +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("") + ylab("Cause")

gganimate(p2)



## ------------------------------------------------------------------------
uk <- c("England", "Northern Ireland", "Scotland", "Wales")
uk_groups <- filter(mortality_tall, country %in% uk) %>%
  droplevels()



## ------------------------------------------------------------------------
top_10 <- group_by(uk_groups, country, year) %>%
    top_n(n = 10, wt = percent)  %>%
    droplevels() %>%
    pull(cause) %>% unique

uk_groups <- filter(uk_groups, cause %in% top_10) %>% droplevels()



## ----fig.show = "animate", interval = 2----------------------------------
uk_groups$country <- factor(
  uk_groups$country,
  levels = uk
)

p3 <- ggplot(uk_groups, aes(percent, cause, col = country, frame = year)) +
      geom_point(aes(size = percent)) + theme_tufte()

gganimate(p3)


